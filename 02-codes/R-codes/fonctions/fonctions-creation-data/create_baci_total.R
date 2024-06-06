#' Crée une database de baci avec la classification CHELEM modifiée
#' ainsi que les secteurs utilisés. Tous les flux sont gardés pour les produits
#' sélectionnés
#'
#' @param path_baci Chemin d'accès à un dossier baci parquet
#' @param codes Codes produits à garder
#' @param path_output Chemin d'accès au dossier parquet de baci_total
#' @return Données de baci avec tous les flux et les bonnes classifications pays
create_baci_total <- function(path_baci, codes, path_output){

  # Supprimer le dossier d'enregistrement de la base s'il existe déjà
  # Evite d'écrire sur un autre fichier et de faire n'importe quoi
  if(dir.exists(path_output)) unlink(path_output, recursive = TRUE)

  # Créer la base BACI contenant tous les flux des produits sélectionnés
  # Estimation de la qualité basée sur tous les produits
  # Ouvrir la base BACI_mi_brute
  # Df sans otuliers et avec les gammes de calculées
  path_baci |>
    arrow::open_dataset() |>
    # Ajouter la classification CHELEM de base
    analyse.competitivite::add_chelem_classification(
      path_output = NULL,
      return_output = TRUE,
      return_pq = TRUE
    ) |>
    # Modifier les catégories géograhiques de la même manière que le df principal
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
        ),
      sector = substr(k, 1, 2),
      sector = 
        dplyr::case_when(
          sector %in% c("61", "62", "65") ~ "Habillement",
          sector == "42" ~ "Maroquinerie",
          sector == "64" ~ "Chaussures",
          sector == "71" ~ "Bijouterie"
        )
    )  |>
    dplyr::filter(k %in% codes) |>
    dplyr::group_by(t) |>
    arrow::write_dataset(path_output)
}
