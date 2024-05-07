# Regader l'évolution du nombre de produits sélectionnés par année
nb_product_by_year <- function(baci, ponderate, years = NULL, codes = NULL, 
                               method_outliers = 'classic', 
                               seuil_H_outliers, seuil_L_outliers,
                               alpha_H_gamme, seuil_2_HG, path_output, remove = TRUE){
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
    )
  
  # Définir les produits sur lesquels la France se positionne dans le haut de gamme
  product_HG_france <-
    baci_mi_processed |>
    # Garder uniquement les flux français de l'année de référence
    dplyr::filter(
      exporter == "FRA"
    ) |>
    # Calculer la somme des flux de chaque produit pour chaque gamme
    dplyr::summarize(
      .by = c(t, k, gamme_fontagne_1997),
      total_v_tikg = sum(v, na.rm = TRUE)
    ) |>
    dplyr::collect() |>
    # Calculer la part que représente chaque gamme par produit
    dplyr::mutate(
      .by = c(t, k),
      share_total_v_gamme_tikg = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
    ) |>
    # Garder uniquement les produits dont la gamme H est supérieure au seuil
    dplyr::filter(
      gamme_fontagne_1997 == "H",
      share_total_v_gamme_tikg >= seuil_2_HG
    ) |>
    # Renvoyer un vecteur avec les codes produits uniquement
    dplyr::arrange(t, k) |> 
    dplyr::mutate(
      sector = substr(k, 1, 2),
      sector = 
        dplyr::case_when(
          sector %in% c("61", "62", "65") ~ "Habillement",
          sector == "42" ~ "Maroquinerie",
          sector == "64" ~ "Chaussures",
          sector == "71" ~ "Bijouterie"
        )
    ) |> 
    summarize(
      .by = c(t, sector),
      n = n()
    )
  
  product_HG_france_total <- 
    product_HG_france |>
    dplyr::summarize(
      .by = t,
      n = sum(n)
    ) |> 
    mutate(sector = "Total") |> 
    rbind(product_HG_france)
  
  graph <- 
    product_HG_france_total |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = t,
        y = n,
        color = sector
      )
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    # ggplot 2::geom_line(aes(data = product_HG_france_total, x = t, y = n), color = "black", linetype = "dashed") +
    ggplot2::labs(
      title = "Nombre de produits français dans le haut de gamme par année",
      x = "Année",
      y = "Nombre de produits",
      color = ""
    ) +
    scale_color_brewer(palette = "Paired") +
    scale_x_continuous(breaks = seq(min(product_HG_france_total$t), max(product_HG_france_total$t), 2)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom"
    )
  
  print(graph)
  
  sheet_name <- "product_HG_france"
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = sheet_name)
  
  openxlsx::writeData(wb, sheet = sheet_name, product_HG_france_total, startCol = 1, startRow = 1,name = "Produits HG France")
  
  openxlsx::insertPlot(wb, sheet = sheet_name, startCol = 6, startRow = 1, width = 8, height = 5)
  
  openxlsx::saveWorkbook(wb, file = path_output, overwrite = TRUE)
}
  
  
  