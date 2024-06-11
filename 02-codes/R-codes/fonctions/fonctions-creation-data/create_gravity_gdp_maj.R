# Documentation ------------------------------------------------------------
#' @title
#' Mettre à jour les données du PIB du PIB
#'
#' @description
#' Met à jour les données de gravity avec les dernières données disponibles
#' du PIB de la banque mondiale.
#'
#' @param path_raw_data_folder Chemin d'accès où sont stockées les données
#' brutes du projet. Les données du PIB seront téléchargées dans ce dossier.
#' @param last_year_gravity La dernière année présente dans les données de
#' gravity. Les années suivantes auront les données de la banque mondiale
#' pour le PIB.
#' @param gravity_variables Les variables de gravité qu'il convient de garder
#' de la base gravity. Attention : penser à inclure 'year', 'iso3_o', iso3_d',
#' gdp_o', 'gdp_d'.
#' @param path_gravity_parquet_folder Le chemin d'accès au dossier contenant
#' les fichiers parquet de la base gravity que l'on souhaite mettre à jour.
#' @param path_output Chemin d'accès au dossier qui contiendra les fichiers
#' parquet de la nouvelle base gravity mise à jour. 
#' @return Les données gravity avec les données du PIB mise à jour.


# Fonction create_gravity_gdp_maj ------------------------------------------
## Définition de la fonction -----------------------------------------------
create_gravity_gdp_maj <- function(path_raw_data_folder, last_year_gravity,
                                   gravity_variables,
                                   path_gravity_parquet_folder, path_output){

  ## Téléchargement des données --------------------------------------------
  # Télécharger les données du PIB depuis l'API de la world Bank
  wbstats::wb_data(indicator = "NY.GDP.MKTP.CD") |>
    readr::write_csv(here::here(path_raw_data_folder, "gdp-wb.csv"))


  ## Traitement des données du PIB -----------------------------------------
  # Modifier les données pour les rendre plus facilement utilisables
  # Définir le df contenant les donénes du PIB mis à jour pour tous les pays
  df_gdp_maj <-
    # Ouvrir les données téléchargées
    here::here(path_raw_data_folder, "gdp-wb.csv") |>
    readr::read_csv() |>
    # Renommer les variables avec des noms plus évocateurs
    dplyr::rename(
      gdp = NY.GDP.MKTP.CD,
      year = date,
      iso3 = iso3c
    ) |>
    # Sélectionnées que les variables utiles
    dplyr::select(iso3, year, gdp) |>
    # Définir le GDP en thousand current $
    dplyr::mutate(gdp = gdp/1000) |>
    # Garqer uniquement les années postérieures à la dernière révision de gravity
    dplyr::filter(year >= last_year_gravity)


  ## Mise à jour de gravity ------------------------------------------------
  # Créer la base gravity mise à jour pour le gdp
  df_gravity_maj <-
    # Ouvrir la base initiale de gravity
    path_gravity_parquet_folder |>
    arrow::open_dataset() |>
    # Sélectionner les variables de gravité : sauf gdp (car maj)
    dplyr::select(dplyr::all_of(gravity_variables), -c(gdp_o, gdp_d)) |>
    # Garder une seule année (une fois chaque couple de pays)
    dplyr::filter(year == last_year_gravity) |>
    # Supprimer la variable year pour fusionner les nouvelles données sur tous
    # les couples de pays
    dplyr::select(-year) |>
    # Joindre les données maj du GDP pour tous les couples de pays, selon
    # les pays d'origine
    dplyr::left_join(
      df_gdp_maj,
      dplyr::join_by(iso3_o == iso3)
    ) |>
    dplyr::rename(gdp_o = gdp) |>
    # Joindre les données maj du GDP pour tous les couples de pays (selon la
    # bonne année), selon les pays de destination
    dplyr::left_join(
      df_gdp_maj,
      dplyr::join_by(iso3_d == iso3, year)
    ) |>
    dplyr::rename(gdp_d = gdp)


  ## Ecriture des données --------------------------------------------------
  # Ecrire les données initiales de gravity en parquet
  path_gravity_parquet_folder |>
    arrow::open_dataset() |>
    dplyr::select(dplyr::all_of(gravity_variables)) |>
    dplyr::group_by(year) |>
    arrow::write_dataset(here::here(path_output))


  # Ecrire les données mise à jour de gravity en parquet
  df_gravity_maj |>
    dplyr::group_by(year) |>
    arrow::write_dataset(here::here(path_output)) 
}
