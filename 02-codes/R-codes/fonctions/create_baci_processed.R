create_baci_processed <- function(baci, ponderate, years = NULL, codes = NULL, 
                                  method_outliers = 'classic', 
                                  seuil_H_outliers, seuil_L_outliers, year_ref = 2010,
                                  alpha_H_gamme, seuil_2_HG, path_list_k_concu,
                                  path_output, 
                                  return_output, return_pq, remove = TRUE){
  
  # Supprimer la précédente base de données si elle existe (évite les remplacements foireux)
  if (remove) {
    if (file.exists(path_output)) {
      unlink(path_output, recursive = TRUE)
    }
  }
 
  # BACI sans outlier + calcul de gamme
   baci_mi_processed <- 
     # Suppression des outliers
     analyse.competitivite::clean_uv_outliers(
       baci = baci,
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
   
   
   # Enregistrer les produits et concurrents sélectionnés
   list_k_concu <- 
     list(
       "product_HG_france" = dplyr::as_tibble(product_HG_france),
       "product_concurrents" = concurrents_vector,
       "sector_concurrents" = concurrents_sector
     ) |> 
     writexl::write_xlsx(path_list_k_concu)
   
   
   # Finalisation de la base BACI
   df_baci <-
     baci_mi_processed |>
     # Garder uniquement les produits sélectionnés à partir de la France
     # Garder uniquement les flux haut de gamme de ces produits
     dplyr::filter(
       k %in% unique(product_HG_france$k),
       gamme_fontagne_1997 == "H"
     ) |>
     dplyr::select(!c(alpha_H, alpha_L, med_ref_t_k)) |>
     # Ajouter la classification chelem
     analyse.competitivite::add_chelem_classification(
       path_output = path_output,
       return_output = return_output,
       return_pq = return_pq
     )

   return(df_baci)
}