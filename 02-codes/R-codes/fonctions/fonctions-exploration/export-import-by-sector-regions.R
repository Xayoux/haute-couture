#  ------------------------------------------------------------------------
#
# Title : exportation_by_sector_regions
#    By : Romain CAPLIEZ
#  Date : 2024-05-06
#
#  ------------------------------------------------------------------------

# Fonction pour les exports -----------------------------------------------
# Fonction pour représenter les parts de marché des pays au sein d'une région par secteur
export_by_sector_regions <- function(path_baci_processed_parquet, 
                                     path_df_product_HG,
                                          exporter_region,
                                          year_ref, seuil_market_share,
                                          path_output, wb) {
  # Importer la liste des produits haut de gamme sélectionnés pour la France
  df_products_HG <-
    path_df_product_HG |> 
    read_xlsx(sheet = "product_HG_france")


  # Créer le nom de la feuille = nom de la région étudiée (nb max de caractères = 30)
  sheet_name <- substr(exporter_region, 1, 30)

  # Créer la feuille dans le workbook
  openxlsx::addWorksheet(wb, sheet_name)

  # Récupérer les parts de marché des pays au sein de la région
  df_market_share_sector_region_mi_processed <-
    # Importe les données de BACI
    path_baci_processed_parquet |>
    arrow::open_dataset() |>
    dplyr::collect() |>
    # Regroupe les deux régions d'Afriques (question de nb de régions et de couleurs)
    dplyr::mutate(
      exporter_name_region =
        dplyr::case_when(
          exporter_name_region %in% c("North Africa", "Sub-Sahara Africa") ~ "Africa",
          .default = exporter_name_region
        )
    ) |>
    arrow::arrow_table() |>
    # Garder uniquement les données de la région étudiée
    dplyr::filter(
      exporter_name_region == exporter_region,
    ) |>
    # Calcul les parts de marché des pays de la région étudiée selon les 4 secteurs
    analyse.competitivite::market_share(
      summarize_k = "sector",
      summarize_v = "exporter",
      seuil = 0,
      years = 2010:2022,
      codes = unique(df_products_HG$k),
      path_output = NULL,
      return_output = TRUE,
      return_pq = FALSE
    ) |>
    dplyr::arrange(t, sector, dplyr::desc(market_share_t_k_i))


  # Sélectionner les 10 pays avec la plus grande part de marché en year_ref et ayant plus de x% de part de marché
  df_exporter_sector_selected <-
    # Reprendre les données de parts de marché par secteur et par pays de la région
    df_market_share_sector_region_mi_processed |>
    # Garder uniquement les données de l'année de référence
    dplyr::filter(t == year_ref) |>
    # Garder les 10 pays avec les plus grandes parts de marché (limite le nombre de pays à un nombre représentable)
    dplyr::slice_max(
      by = sector,
      market_share_t_k_i, n = 10
    ) |>
    # Garde uniquement les pays qui ont plus de x% de parts de marché
    dplyr::filter(market_share_t_k_i > seuil_market_share) |>
    dplyr::select(exporter, sector)


  # Garde uniquement les pays sélectionnés par secteur (continuité des pays : évite les sauts dans le graphique)
  # On s'intéresse aux concurrents actuels, pas à ceux d'avant
  df_market_share_sector_region <-
    df_market_share_sector_region_mi_processed |>
    dplyr::right_join(
      df_exporter_sector_selected,
      by = c("exporter", "sector")
    ) |>
    dplyr::select(-q_t_k_i)


  # Créer la représentation graphique en aire
  graph_market_share_sector_region <-
    df_market_share_sector_region |>
    ggplot2::ggplot(ggplot2::aes(x = t, y = market_share_t_k_i, fill = exporter)) +
    ggplot2::geom_area() +
    ggplot2::scale_x_continuous(breaks = seq(2010, 2022, 2)) +
    ggplot2::scale_fill_brewer(palette = "Paired") +
    ggplot2::labs(
      x = "Année",
      y = "Parts de marché",
      title = stringr::str_glue("Exportations haut de gamme de la région {exporter_region}"),
      caption = stringr::str_glue("Les pays représentés font parti des 10 pays avec la plus grande part de marché en {year_ref}\n et ayant plus de {seuil_market_share}% de part de marché.")
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
    ) +
    ggplot2::facet_wrap(~sector, scales = "free_y")

  # Afficher le graphique pour permettre l'enregistrement
  print(graph_market_share_sector_region)

  # Ecrire les données et le graphique dans le workbook
  openxlsx::writeData(wb, sheet = sheet_name, df_market_share_sector_region)

  openxlsx::insertPlot(wb,
    sheet = sheet_name, startRow = 1,
    startCol = 7, width = 15, height = 8
  )

  # Sauvegarder le workbook
  openxlsx::saveWorkbook(wb, path_output, overwrite = TRUE)
}



# Fonction pour les imports -----------------------------------------------
# Fonction pour représenter les parts de marché des pays au sein d'une région par secteur
import_by_sector_regions <- function(path_baci_processed_parquet, 
                                     path_df_product_HG,
                                          importer_region,
                                          year_ref, seuil_market_share,
                                          path_output, wb) {
  # Importer la liste des produits haut de gamme sélectionnés pour la France
  df_products_HG <-
    path_df_product_HG |>
    read_xlsx(sheet = "product_HG_france")
  
  
  # Créer le nom de la feuille = nom de la région étudiée (nb max de caractères = 30)
  sheet_name <- substr(importer_region, 1, 30)
  
  # Créer la feuille dans le workbook
  openxlsx::addWorksheet(wb, sheet_name)
  
  # Récupérer les parts de marché des pays au sein de la région
  df_market_share_sector_region_mi_processed <-
    # Importe les données de BACI
    path_baci_processed_parquet |>
    arrow::open_dataset() |>
    dplyr::collect() |>
    # Regroupe les deux régions d'Afriques (question de nb de régions et de couleurs)
    dplyr::mutate(
      importer_name_region =
        dplyr::case_when(
          importer_name_region %in% c("North Africa", "Sub-Sahara Africa") ~ "Africa",
          .default = importer_name_region
        )
    ) |>
    arrow::arrow_table() |>
    # Garder uniquement les données de la région étudiée
    dplyr::filter(
      importer_name_region == importer_region,
    ) |>
    # Calcul les parts de marché des pays de la région étudiée selon les 4 secteurs
    analyse.competitivite::market_share(
      summarize_k = "sector",
      summarize_v = "importer",
      seuil = 0,
      years = 2010:2022,
      codes = unique(df_products_HG$k),
      path_output = NULL,
      return_output = TRUE,
      return_pq = FALSE
    ) |>
    dplyr::arrange(t, sector, dplyr::desc(market_share_t_k_i))
  
  
  # Sélectionner les 10 pays avec la plus grande part de marché en year_ref et ayant plus de x% de part de marché
  df_importer_sector_selected <-
    # Reprendre les données de parts de marché par secteur et par pays de la région
    df_market_share_sector_region_mi_processed |>
    # Garder uniquement les données de l'année de référence
    dplyr::filter(t == year_ref) |>
    # Garder les 10 pays avec les plus grandes parts de marché (limite le nombre de pays à un nombre représentable)
    dplyr::slice_max(
      by = sector,
      market_share_t_k_i, n = 10
    ) |>
    # Garde uniquement les pays qui ont plus de x% de parts de marché
    dplyr::filter(market_share_t_k_i > seuil_market_share) |>
    dplyr::select(importer, sector)
  
  
  # Garde uniquement les pays sélectionnés par secteur (continuité des pays : évite les sauts dans le graphique)
  # On s'intéresse aux concurrents actuels, pas à ceux d'avant
  df_market_share_sector_region <-
    df_market_share_sector_region_mi_processed |>
    dplyr::right_join(
      df_importer_sector_selected,
      by = c("importer", "sector")
    ) |>
    dplyr::select(-q_t_k_i)
  
  
  # Créer la représentation graphique en aire
  graph_market_share_sector_region <-
    df_market_share_sector_region |>
    ggplot2::ggplot(ggplot2::aes(x = t, y = market_share_t_k_i, fill = importer)) +
    ggplot2::geom_area() +
    ggplot2::scale_x_continuous(breaks = seq(2010, 2022, 2)) +
    ggplot2::scale_fill_brewer(palette = "Paired") +
    ggplot2::labs(
      x = "Année",
      y = "Parts de marché",
      title = stringr::str_glue("Importations haut de gamme de la région {importer_region}"),
      caption = stringr::str_glue("Les pays représentés font parti des 10 pays avec la plus grande part de marché en {year_ref}\n et ayant plus de {seuil_market_share}% de part de marché.")
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
    ) +
    ggplot2::facet_wrap(~sector, scales = "free_y")
  
  # Afficher le graphique pour permettre l'enregistrement
  print(graph_market_share_sector_region)
  
  # Ecrire les données et le graphique dans le workbook
  openxlsx::writeData(wb, sheet = sheet_name, df_market_share_sector_region)
  
  openxlsx::insertPlot(wb,
                       sheet = sheet_name, startRow = 1,
                       startCol = 7, width = 15, height = 8
  )
  
  # Sauvegarder le workbook
  openxlsx::saveWorkbook(wb, path_output, overwrite = TRUE)
}