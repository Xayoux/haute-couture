# Documentation ------------------------------------------------------------
#' @title
#' Créer la base BACI prête à l'utilisation du projet sur la haute-couture et
#' la mode
#'
#' @description Retirer les outliers de la base de données BACI, calcule les
#' gammes de chaque flux. Défini les secteurs étudiés spécifiquement dans
#' ce projet et défini la classification géographiqe utilisée.
#'
#' Utilise les fonctions `clean_uv_outliers`, `gamme_ijkt_fontagne_1997` et
#' `add_chelem_classification` du package `analyse.competitivite`.
#'
#' Les flux entre CHN et HKG sont supprimés dès le de départ. Donc calcul des
#' outliers et des gammes sans ces données. 
#' 
#' 
#' @param baci Chemin d'accès vers un dossier parquet BACI.
#' @param ponderate La variable à utiliser pour les pondérations si
#' pondération nécessaire dans le calcul des gammes. 
#' @param years Années étudiées.
#' @param codes Codes produits HS6 (sous forme de vecteur de chaînes de
#' caractères) utilisés.
#' @param method_outliers La méthode à utiliser pour la définition des
#' outliers. Les méthodes possible sont : 'fh13', 'h06', 'sd', 'be11'. 
#' @param seuil_H_outliers Le seuil supérieur pour la définition des outliers.
#' Doit dépendre de la méthode utilisée. 
#' @param seuil_L_outliers Seuil inférieur pour la définition des outliers.
#' Doit dépendre de la méthode utilisée. 
#' @param year_ref L'année de référence dans la définition des produits
#' sélectionnés. 
#' @param alpha_H_gamme Le seuil supérieur permettant de déterminer si un
#' flux est haut de gamme ou non. 
#' @param seuil_2_HG Le seuil à partir duquel on considère que la France
#' (et les autres pays) sont spécialiés dans le haut de gamme pour un produit. 
#' @param path_list_k_concu Chemin d'acès pour enregistrer le fichier
#' excel contenant les informations sur les produits et concurrents
#' sélectionnés. 
#' @param path_output Chemin d'accès vers le dossier où enregistrer les données
#' produits en format parquet. 
#' @param return_output Booléen indiquant si le résultat doit être retourné
#' pour modification ou non. 
#' @param return_pq Booléen indiquant si les données retournées doivent
#' l'être en format parquet ou non. 
#' @param remove Booléen indiquant s'il faut supprimer ou non le dossier
#' d'enregistrement des données s'il existe déjà (pour éviter une erreur).
#' @return
# Fonction create_baci_processed -------------------------------------------
## Définition de la fonction  ----------------------------------------------
create_baci_processed <- function(baci, ponderate, years = NULL, codes = NULL, 
                                  method_outliers = 'classic', 
                                  seuil_H_outliers, seuil_L_outliers, year_ref = 2010,
                                  alpha_H_gamme, seuil_2_HG, path_list_k_concu,
                                  path_output, 
                                  return_output, remove = TRUE){

  ## Suppression de l'ancienne base ----------------------------------------
  # Supprimer la précédente base de données si elle existe (évite les remplacements foireux)
  if (remove) {
    if (file.exists(path_output)) {
      unlink(path_output, recursive = TRUE)
    }
  }


  ## Suppression des flux entre CHN et HKG ---------------------------------
  baci_mi_processed <-
    baci  |>
    open_dataset()  |>
    filter(
      !(exporter %in% c("CHN", "HKG") & importer %in% c("CHN", "HKG"))
    )


  ## Suppression outlier + calcul gammes -----------------------------------
  # BACI sans outlier + calcul de gamme
   baci_mi_processed <- 
     # Suppression des outliers
     analyse.competitivite::clean_uv_outliers(
       baci = baci_mi_processed,
       years = years,
       codes = codes,
       method = method_outliers,
       seuil_H = seuil_H_outliers,
       seuil_L = seuil_L_outliers,
       path_output = NULL,
       return_output = TRUE,
       return_pq = TRUE
     ) |> 
     # Calcul des gammes
     analyse.competitivite::gamme_ijkt_fontagne_1997(
       ponderate = ponderate,
       alpha_H = alpha_H_gamme,
       pivot = "longer",
       path_output = NULL,
       return_output = TRUE,
       return_pq = TRUE
     ) |> 
     mutate(
       sector = substr(k, 1, 2),
       sector = 
         dplyr::case_when(
           sector %in% c("61", "62", "65") ~ "Habillement",
           sector == "42" ~ "Maroquinerie",
           sector == "64" ~ "Chaussures",
           sector == "71" ~ "Bijouterie"
         ) 
     )
   

  ## Création des documents annexes ----------------------------------------
  ### Définition produits hauts de gamme pour la France --------------------
   # Définir les produits sur lesquels la France se positionne dans le haut de gamme
   product_HG_france <-
     baci_mi_processed |>
     # Garder uniquement les flux français de l'année de référence
     dplyr::filter(
       exporter == "FRA",
       t == year_ref
      ) |>
     # Calculer la somme des flux de chaque produit pour chaque gamme
     dplyr::summarize(
       .by = c(k, gamme_fontagne_1997),
       total_v_tikg = sum(v, na.rm = TRUE)
     ) |>
     dplyr::collect() |>
     # Calculer la part que représente chaque gamme par produit
     dplyr::mutate(
       .by = k,
       share_total_v_gamme_tikg = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
     ) |>
     # Garder uniquement les produits dont la gamme H est supérieure au seuil
     dplyr::filter(
       gamme_fontagne_1997 == "H",
       share_total_v_gamme_tikg >= seuil_2_HG
     ) |>
     # Renvoyer un vecteur avec les codes produits uniquement
     dplyr::arrange(k) |> 
     dplyr::select(k) |>
     dplyr::distinct() |> 
     # Ajouter la description des produits
     dplyr::left_join(
       df_product |>  select(HS92, description_HS92),
       by = c("k" = "HS92")
     ) 


  ### Sélection des concurrents --------------------------------------------
   # Sélectionner les concurrents
   concurrents_vector <- 
     baci_mi_processed |>
     # Garder uniquement l'année de référence et les produits sélectionnés
     dplyr::filter(
       t == year_ref,
       k %in% unique(product_HG_france$k)
      ) |> 
     # Somme des flux pour chaque produit, exportateur et gamme
     dplyr::summarize(
       .by = c(exporter, k, gamme_fontagne_1997),
       total_v_tikg  = sum(v, na.rm = TRUE)
      ) |> 
     dplyr::collect() |> 
     # Calcul de la part de chaque gamme par produit
     dplyr::mutate(
       .by = c(exporter, k),
       share_total_v_gamme_tikg = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
     ) |> 
     # Garder uniquement les gammes H
     dplyr::filter(
       gamme_fontagne_1997 == "H"
     ) |> 
     # calculer part de marché de chaque exportateur dans le haut de gamme
     dplyr::mutate(
       .by = c(k),
       market_share_HG = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
     ) |> 
     # Garder les exportateur dont le haut de gamme représente au moins 75%
     # de leurs exportations du produit et dont lapart de marché est d'au moins 5%. 
     # Ou garder les exportateurs dont la part de marché est d'au moins 10%.
     dplyr::filter(
       (share_total_v_gamme_tikg >= seuil_2_HG & (market_share_HG >= 0.05 | exporter == "FRA")) |
         (market_share_HG >= 0.1)
     ) |> 
     select(exporter, k, share_total_v_gamme_tikg, market_share_HG) |> 
     arrange(k, market_share_HG)
   

  ### Statistiques des concurrents ----------------------------------------
   # Regarder les concurrents par secteur
   concurrents_sector <- 
     baci_mi_processed |> 
     filter(
       t == year_ref,
       k %in% unique(product_HG_france$k)
     ) |> 
     # Somme des flux pour chaque secteur, exportateur et gamme
     dplyr::summarize(
       .by = c(exporter, sector, gamme_fontagne_1997),
       total_v_tikg  = sum(v, na.rm = TRUE)
     ) |> 
     dplyr::collect() |> 
     # Calcul de la part de chaque gamme par produit
     dplyr::mutate(
       .by = c(exporter, sector),
       share_total_v_gamme_tikg = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
     ) |> 
     # Garder uniquement les gammes H
     dplyr::filter(
       gamme_fontagne_1997 == "H"
     ) |> 
     # calculer part de marché de chaque exportateur dans le haut de gamme
     dplyr::mutate(
       .by = c(sector),
       market_share_HG = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
     ) |> 
     # Garder les exportateur dont le haut de gamme représente au moins 75%
     # de leurs exportations du produit et dont la part de marché est d'au moins 5%. 
     # Ou garder les exportateurs dont la part de marché est d'au moins 10%.
     dplyr::filter(
       (share_total_v_gamme_tikg >= seuil_2_HG & (market_share_HG >= 0.05 | exporter == "FRA")) |
         (market_share_HG >= 0.1)
     ) |> 
     select(exporter, sector, share_total_v_gamme_tikg, market_share_HG) |> 
     arrange(sector, market_share_HG)
   

  ### Enregistrement des fichiers annexes -----------------------------------
   # Enregistrer les produits et concurrents sélectionnés
   list_k_concu <- 
     list(
       "product_HG_france" = dplyr::as_tibble(product_HG_france),
       "product_concurrents" = concurrents_vector,
       "sector_concurrents" = concurrents_sector
     ) |> 
     writexl::write_xlsx(path_list_k_concu)
   

  ## Fitrage des produits + ajout classification géo à BACI ----------------
   # Finalisation de la base BACI
   df_baci <-
     baci_mi_processed |>
     # Garder uniquement les produits sélectionnés à partir de la France
     # Garder uniquement les flux haut de gamme de ces produits
     dplyr::filter(
       k %in% unique(product_HG_france$k),
       gamme_fontagne_1997 == "H"
     ) |>
     dplyr::select(!c(alpha_H, alpha_L)) |>
     # Ajouter la classification chelem
     analyse.competitivite::add_chelem_classification(
       path_output = NULL,
       return_output = TRUE,
       return_pq = TRUE
     ) |>
     # Modifier la classification géographique
     # Limiter le nombre de régions / correspondre à ce qui nous interesse
     dplyr::mutate(
       exporter_name_region = 
         dplyr::case_when(
           # Catégories pour la Bijouterie
           exporter == "TUR" & sector == "Bijouterie" ~ "Turquie",
           exporter == "USA" & sector == "Bijouterie" ~ "USA",
           exporter_name_region %in%
             c("South America, Central America and Caribbean", "North America") & 
             sector == "Bijouterie" ~ "RDM",
           # Catégories générales
           exporter == "FRA" ~ "France",
           exporter == "ITA" ~ "Italie",
           exporter == "GBR" ~ "Reste de l'UE",
           exporter_name_region == "European Union" ~ "Reste de l'UE",
           exporter == "CHE" ~ "Suisse",
           exporter %in% c("CHN", "HKG") ~ "Chine et HK",
           exporter_name_region %in% 
             c("South-East Asia", "South Asia and Pacific", "North-East Asia") & 
             !exporter %in% c("CHN", "HKG") ~ "Reste de l'Asie",
           exporter_name_region == "Near and Middle East" ~ "Moyen-Orient",
           exporter_name_region %in%
             c("South America, Central America and Caribbean", "North America") ~ "Amérique",
           # Par défaut dans RDM
           .default = "RDM"
         ),
       importer_name_region =
         dplyr::case_when(
           # Catégories générales
           importer == "FRA" ~ "France",
           importer == "ITA" ~ "Italie",
           importer == "GBR" ~ "Reste de l'UE",
           importer_name_region == "European Union" ~ "Reste de l'UE",
           importer == "CHE" ~ "Suisse",
           importer %in% c("CHN", "HKG") ~ "Chine et HK",
           importer %in% c("JPN", "KOR") ~ "Japon et Corée",
           importer_name_region %in% 
             c("South-East Asia", "South Asia and Pacific", "North-East Asia") ~ "Reste de l'Asie",
           importer == "ARE" ~ "ARE",
           importer_name_region == "Near and Middle East" ~ "Moyen-Orient",
           importer == "USA" ~ "USA",
           importer_name_region %in% 
             c("South America, Central America and Caribbean", "North America") ~ "Amérique",
           # Par défaut : reste du monde
           .default = "RDM" 
         )
     ) |>
     # Ecrire la base en format parquet par années
     dplyr::group_by(t) |>
     arrow::write_dataset(path_output)

  if (return_output == TRUE){
    return(df_baci)
  }
}
