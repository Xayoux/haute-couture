#  ------------------------------------------------------------------------
#
# Title : Fonctions pour explorer les données sur différents seuils
#    By : Romain CAPLIEZ...
#  Date : 2024-04-05
#
#  ------------------------------------------------------------------------

# Une fonction principale pour créer un fichier excel contenant un certain
# nombre d'informations sur les produits de haut de gamme français en fonction 
# de différents seuils de gammes. 

# Plusieurs sous-fonctions qui permettent de calculer et écrire les 
# informations facilement pour chaque seuil. 

# Chaque sous-fonction sert à écrire une information sur un seuil
# particulier / faire un graphique particulier. 

# La fonction principale va faire exécuter toutes ces sous-fonctions sur 
# l'ensemble des seuils souhaités. 


# Librairies utilisées ----------------------------------------------------
# library(tidyverse)
# library(arrow)
# library(openxlsx)
# library(here)
# library(readxl)


# Sous-fonction pour écrire le fichier d'exploration de seuils ------------
# Fonction pour avoir des informations sur les données dans chaque seuil
exploration_seuil_haut_gamme <- function(data_gammes, alpha, seuil_2, wb,
                                         df_product, folder_output, doc_title){
  
  # Ouvrir les données de gammes
  if (is.character(data_gammes) == TRUE){
    # Ouvrir les données depuis un dossier parquet
    df_gammes <-
      data_gammes |>
      arrow::open_dataset()
  }
  else if (is.data.frame(data_gammes) == TRUE){
    # Ouvrir les données depuis un dataframe : passage en format arrow
    df_gammes <-
      data_gammes |>
      arrow::arrow_table()
  }
  else{
    # Ouvrir les données depuis format arrow : rien à faire
    df_gammes <- data_gammes
  }
  
  # Dataframe avec la part de chaque gamme dans le commerce de chaque produit par exportateur
  df_gammes <- 
    df_gammes |> 
    # Garder uniquement le seuil voulu
    dplyr::filter(
      alpha_H == alpha
    ) |> 
    # Sommer tous les flux de la même catégorie pour un exportateur et un produit
    dplyr::summarize(
      .by = c(t, k, exporter, gamme_fontagne_1997),
      total_v_tikg = sum(v, na.rm = TRUE) 
    ) |> 
    dplyr::collect() |> 
    # Calculer la part de chaque gamme dans le commerce d'un produit par exportateur
    dplyr::mutate(
      .by = c(t, k, exporter),
      share_total_v_gamme_tikg = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
    )
  
  
  # Dataframe pour sélectionner les produits de luxe fr
  df_products_luxes_fr <- 
    df_gammes |> 
    # Garder uniquement les données de haut de gamme
    dplyr::filter(
      gamme_fontagne_1997 == "H"
    ) |> 
    # Calculer la part de marché sur le marché du haut de gamme
    dplyr::mutate(
      .by = c(t,k), 
      market_share = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
    ) |> 
    # Garde uniquement les lignes françaises
    # Garde uniquement les lignes dont la part du haut de gamme est supérieure à un seuil
    dplyr::filter(
      exporter == "FRA",
      share_total_v_gamme_tikg >= seuil_2 
    ) |> 
    dplyr::select(t, k, share_total_v_gamme_tikg, market_share) |> 
    dplyr::arrange(k)
  
  # Df sur les données  des produits de luxe français
  df_commerce_haut_gamme <- 
    df_gammes |> 
    # Garder uniquement les données françaises
    dplyr::filter(
      exporter == "FRA"
    ) |>
    # Calculer la valeur totale du commerce français sur tous les biens
    dplyr::mutate(
      .by = t,
      total_v_commerce = sum(total_v_tikg, na.rm = TRUE)
    ) |> 
    # Garder uniquement les produits sélectionnés précédemment
    # Garder que les données haut de gamme
    dplyr::filter(
      k%in% unique(df_products_luxes_fr$k),
      gamme_fontagne_1997 == "H"
    ) |>
    # Calculer la part de chaque produit sélectionné par rapport au commerce fr total 
    dplyr::mutate(
      .by = t,
      share_commerce_fr = total_v_tikg / total_v_commerce
    ) |> 
    # Trier la part dans le commerce fr par ordre décroissant
    dplyr::arrange(dplyr::desc(share_commerce_fr)) |> 
    dplyr::select(!c(gamme_fontagne_1997, t, exporter))
  
  
  # Part des produits sélectionnés dans le commerce français total
  part_produits_total <- 
    df_commerce_haut_gamme|> 
    # Calculer la part des produits sélectionnés dans le commerce français total
    dplyr::summarize(part_produit_total = sum(share_commerce_fr))
  
  
  # Part des produits sélectionnés dans le commerce français haut de gamme
  part_produits_haut_gamme <-
    df_gammes |> 
    # Garder seulement les biens français haut de gamme
    dplyr::filter(
      gamme_fontagne_1997 == "H",
      exporter == "FRA"
    ) |>
    # Calculer la valeur commerciale totale des biens haut de gamme fr
    dplyr::mutate(
      .by = t,
      total_v_haut_gamme = sum(total_v_tikg, na.rm = TRUE)
    ) |> 
    # Garder uniquement les biens sélectionnés
    dplyr::filter(
      k%in% unique(df_products_luxes_fr$k)
    ) |>
    # Calculer la part des biens sélectionnés dans le commerce haut de gamme fr
    dplyr::mutate(
      .by = t,
      share_commerce_fr_hg = total_v_tikg / total_v_haut_gamme
    ) |> 
    # Trier les données par ordre décroissant
    dplyr::arrange(dplyr::desc(share_commerce_fr_hg)) |>
    # Calculer la part ttale des produits sélectionnés dans le commerce haut de gamme fr
    dplyr::summarize(part_produit_haut_gamme = sum(share_commerce_fr_hg))
  
  
  # Dataframe avec les données des concurrents sur les produits haut de gamme sélectionnés
  df_concu_luxe <- 
    df_gammes |> 
    # Garder les produits sélectionnés et les données haut de gamme
    dplyr::filter(
      k %in% unique(df_products_luxes_fr$k),
      gamme_fontagne_1997 == "H",
    ) |> 
    # Calculer la part de marché de chaque concurrent sur chaque produit 
    # Part de marché sur le marché haut de gamme
    dplyr::mutate(
      .by = c(k),
      market_share = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
    ) |> 
    # Garder les produits-pays dont la valeur du haut de gamme est >= à un seuil
    # Garder les produits-pays dont la part de marché est >= 5%
    # Garder tous les produits français haut de gamme dont la part dans le commerce du produit est >= seuil_2
    dplyr::filter(
      (share_total_v_gamme_tikg >= seuil_2 & (market_share >= 0.05 | exporter == "FRA")) |
        market_share >= 0.1
      # share_total_v_gamme_tikg >= seuil_2,
      # market_share >= 0.05 | exporter == "FRA"
    ) |> 
    # Garder uniquement les variables d'intérêt
    dplyr::select(k, exporter, share_total_v_gamme_tikg, market_share) |> 
    # Trier les données
    dplyr::arrange(k, dplyr::desc(market_share)) |> 
    # Ajouter la description des produits HS6 retenus
    dplyr::left_join(
      df_product |>
        dplyr::select(HS92, description_HS92) |> dplyr::distinct(),
      by = c("k" = "HS92")
    )
  
  
  # Nb de produits sur lesquels chaque concurrent est présent
  concurrents <- 
    df_concu_luxe |> 
    dplyr::select(k, exporter) |> 
    dplyr::summarize(
      .by = exporter, 
      nb = dplyr::n()
    ) |> 
    dplyr::arrange(dplyr::desc(nb))
  
  
  # Nombre de concurrents par produit
  nb_concu_by_product <-
    df_concu_luxe |> 
    dplyr::summarize(
      .by = k,
      nb_concurrents = dplyr::n()
    )
  
  
  # Nombre de produits sélectionné
  nb_products <- 
    df_products_luxes_fr |> 
    dplyr::summarize(nb_product = dplyr::n())
  
  
  # Chiffres indicatifs 
  unit_var <- list(
    nb_products, # nb produits
    part_produits_total, # part des produits sélectionnés dans le commerce fr
    part_produits_haut_gamme # part des produits sélectionnés dans le commerce haut de gamme fr
  )
  
  
  # Dataframes à ajouter dans la feuille excel
  variables <- list(
    nb_concu_by_product, # Nombre de concurrents par produits
    df_commerce_haut_gamme, # Données françaises sur les produits sélectionnés
    df_concu_luxe, # Données mondiales sur les produits sélectionnés
    concurrents # Nombre de produits par concurrents. 
  )
  
  
  # Titres pour les dataframes
  titles <- c(
    "Nb concurrents / produits",
    "Données françaises",
    "Données concurrents",
    "Nb produits / concurrents"
  )
  
  
  # Nom de la feuille excel où les données seront enregistrées (dépend de alpha)
  sheet_name <- paste("Seuil", alpha, sep = "_")
  
  # AJoute une feuille au workbook
  openxlsx::addWorksheet(wb, sheetName = sheet_name)
  
  # Colonnes de départ pour chaque titre/dataframes (hors chiffres indicatifs)
  num_cols <- c(3, 6, 13, 19)
  
  
  # AJouter les chiffres indicatifs dans la première colonne
  row <- 1 # permet de définir le numéro de ligne pour inscrire les données
  
  for (j in seq_along(unit_var)) {
    # Pour chaque valeur indicatives, l'inscrire dans le fichier excel
    openxlsx::writeData(wb, sheet = sheet_name, x = unit_var[[j]], startCol = 1, startRow = row)
    # Mettre un espace de Une ligne entre les données (1 ligne titre, 1 valeur, 1 espace = 3)
    row <- row + 3
  }
  
  
  # Ajouter les dataframes dans le classeur excel
  for (i in seq_along(titles)) {
    # Pour chaque dataframe : ajouter un titre ligne 1, colonne correspondante
    openxlsx::writeData(wb, sheet = sheet_name, x = titles[i], startCol = num_cols[i], startRow = 1)
    # Pour chaque dataframe : ajouter les données ligne 2, colonne correspondante
    openxlsx::writeData(wb, sheet = sheet_name, x = variables[[i]], startCol = num_cols[i], startRow = 2)
  }
  
  
  # Sauvegarder le classeur excel
  openxlsx::saveWorkbook(wb, 
               file = here::here(folder_output, 
                           stringr::str_glue("{doc_title}-{seuil_2}.xlsx")), 
               overwrite = TRUE)
  
  
  # Récupérer les données dnas une liste 
  return(list(variables, unit_var))
}


# Sous-fonction : df nb produits / chapitres -> pour graph -----------------
# Créer data frame pour le nombre de produits par chapitres selon les seuils
df_nb_product_by_seuil <- function(num, df, seuils){
  # Compter le nombre de produits par chapitre
  df_chapter <- 
    # sélectionner le df correspondant au nb de produist pour le seuil 'num'
    df[[num]][[1]][[1]] |> 
    # Sélectionner uniquement la colonne des produits
    dplyr::select(k) |> 
    # Associer chaque produit à son chapitre (2 premiers chiffres)
    dplyr::mutate(
      seuil = seuils[num],
      chapter = substr(k, 1, 2)
    ) |> 
    # Compter le nombre de produits par chapitre
    dplyr::summarize(
      .by = c(seuil, chapter),
      nb_products = dplyr::n()
    )
  
  # Compter le nombre de produit total
  df_total <- 
    # Sélectionner le df correspondant au nb de produits pour le seuil 'num'
    df[[num]][[1]][[1]] |> 
    # Sélectionner uniquement la colonne des produits
    dplyr::select(k) |> 
    # Créer la variable total pour le nombre de produits global
    dplyr::mutate(
      seuil = seuils[num],
      chapter = "total"
    )|> 
    # Compter le nombre de produits globaux
    dplyr::summarize(
      .by = c(seuil, chapter),
      nb_products = dplyr::n()
    )
  
  # Fusionner les deux df
  df <- 
    df_chapter |> 
    dplyr::bind_rows(df_total)
  
  # Retourner une liste de datframes (1 par seuil)
  return(df)
}


# Sous-fonction : df nb concu moyens -> pour graph ------------------------
# Créer data frame pour le nombre moyen de concurrents par chapitres selon les seuils
df_nb_concu_by_seuil <- function(num, df, seuils){
  # Faire la moyenne du nombre de concurrents par chapitres
  df_chapter <- 
    # Sélectionner le df correspondant au nombre de concurrents pour le seuil 'num'
    df[[num]][[1]][[1]] |> 
    # Associer chaque produit à son chapitre (2 premiers chiffres)
    dplyr::mutate(
      seuil = seuils[num],
      chapter = substr(k, 1, 2)
    ) |> 
    # Faire la moyenne du nombre de concurrents par chapitre
    dplyr::summarize(
      .by = c(seuil, chapter),
      nb_concurrents = mean(nb_concurrents, na.rm = TRUE)
    )
  
  # Faire la moyenne du nombre de concurrents total
  df_total <- 
    # Sélectionner le df correspondant au nombre de concurrents pour le seuil 'num'
    df[[num]][[1]][[1]] |> 
    # Créer la variable moyenne pour le nombre de concurrents en moyenne
    dplyr::mutate(
      seuil = seuils[num],
      chapter = "Moyenne"
    )|> 
    # Faire la moyenne du nombre de concurrents total
    dplyr::summarize(
      .by = c(seuil, chapter),
      nb_concurrents = mean(nb_concurrents, na.rm = TRUE)
    )
  
  # Fusionner les deux df
  df <- 
    df_chapter |> 
    dplyr::bind_rows(df_total)
  
  # Retourner une liste de datframes (1 par seuil)
  return(df)
}


# Sous-fonction : df part des produits sélectionnés dans commerce ---------
part_produit_total_function <- function(num, df, seuils){
  df_part_total <- 
    # Sélectionner le df correspondant à la part des produits pour le seuil 'num'
    # Dans le commerce total
    df[[num]][[2]][[2]] |> 
    # Créer la variable seuil et total pour le graphique
    dplyr::mutate(
      seuil = seuils[num],
      share_type = "Total"
    ) |> 
    # Renommer la variable de part pour permettre la fusion des df
    dplyr::rename(share = part_produit_total)
  
  df_part_haut_gamme <- 
    # Sélectionner le df correspondant à la part des produits pour le seuil 'num'
    # Dans le commerce haut de gamme
    df[[num]][[2]][[3]] |> 
    # Créer la variable seuil et haut de gamme pour le graphique
    dplyr::mutate(
      seuil = seuils[num],
      share_type = "Haut de game"
    ) |> 
    # Renommer la variable de part pour permettre la fusion des df
    dplyr::rename(share = part_produit_haut_gamme)
  
  # Fusionner les deux df
  df <- 
    df_part_total |> 
    dplyr::bind_rows(df_part_haut_gamme)
  
  # Retourner une liste de datframes (1 par seuil)
  return(df)
}


# Fonction pour créer le document d'analyse des seuils --------------------
file_exploration_seuils_function <- function(data_gammes, alpha_vector, seuil_2,
                                             df_product, folder_output, doc_title){
  
  # Créer un workbook pour enregistrer les données
  wb_concu <- openxlsx::createWorkbook()
  
  # Effectuer l'exploration pour chaque seuil pour un seuil 2 donné
  df <- 
    # Effectuer la fonction pour chaque seuil
    alpha_vector |> 
    purrr::map(
      \(alpha) exploration_seuil_haut_gamme(
        data_gammes = data_gammes,
        alpha, seuil_2 = seuil_2, wb_concu, df_product, folder_output, 
        doc_title = doc_title
      ), 
      .progress = TRUE
    )
  
  
  # Créer une feuille dans le workbook pour les graphiques 
  openxlsx::addWorksheet(wb_concu, sheetName = "Graphiques")
  
  # Graphique pour le nombre de produits par chapitre selon les seuils
  graph_nb_product <- 
    # Créer uen séquence de numérique correspondant au nombre de seuils
    # Pemret de sélectionner les df correspodnant aux bon seuils dans la fonction
    seq_along(alpha_vector) |>
    # Exécuter la fonction pour chaque seuil
    purrr::map(
      \(num) df_nb_product_by_seuil(num, df, alpha_vector)) |> 
    # Lier tous les df en un seul
    purrr::list_rbind() |> 
    # Transformer la variable seuil en caractère pour avoir un axe x lisible
    dplyr::mutate(seuil = as.character(seuil)) |> 
    # Créer le graphique
    ggplot2::ggplot(ggplot2::aes(x = seuil, y = nb_products, color = chapter)) +
    ggplot2::geom_line(ggplot2::aes(group = chapter), linewidth = 1.1) +
    ggplot2::labs(
      x = "Seuils de gamme",
      y = "Nombre de produits",
      title = "Nombre de produits par chapitre HS6 selon les seuils de gamme",
      color = "Chapitres HS6"
    ) +
    ggplot2::scale_color_brewer(palette = "Paired") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor.x = ggplot2::element_blank()
    )
  
  # Afficher le graphique : permet d'être inclus dans le fichier excel
  print(graph_nb_product)
  
  # Insérer le graphique dans le classeur excel
  openxlsx::insertPlot(wb_concu, sheet = "Graphiques", startRow = 1, startCol = 1, 
             width = 8, height = 5)
  
  # Sauvegarder le fichier excel
  openxlsx::saveWorkbook(wb_concu, 
               file = here::here(folder_output, 
                           stringr::str_glue("{doc_title}-{seuil_2}.xlsx")), 
               overwrite = TRUE)
  
  
  # Graphique pour le nombre moyen de concurrents par chapitre selon les seuils
  graph_nb_concu <- 
    # Créer une séquence de numérique correspondant au nombre de seuils
    # Permet de sélectionner les df correspondant aux bons seuils dans la fonction
    seq_along(alpha_vector) |> 
    # Exécuter la fonction pour chaque seuil
    purrr::map(
      \(num) df_nb_concu_by_seuil(num, df, alpha_vector)) |> 
    # Lier tous les df en un seul
    purrr::list_rbind() |> 
    # Transformer la variable seuil en caractère pour avoir un axe x lisible
    dplyr::mutate(seuil = as.character(seuil)) |> 
    # Créer le graphique
    ggplot2::ggplot(aes(x = seuil, y = nb_concurrents, color = chapter)) +
    ggplot2::geom_line(ggplot2::aes(group = chapter), linewidth = 1.1) +
    ggplot2::labs(
      x = "Seuils de gamme",
      y = "Nombre de concurrents moyen",
      title = "Nombre de concurrents en moyenne par chapitre HS6 selon les seuils de gamme",
      color = "Chapitres HS6"
    ) +
    ggplot2::scale_color_brewer(palette = "Paired") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor.x = ggplot2::element_blank()
    )
  
  # Afficher le graphique : permet d'être inclus dans le fichier excel 
  print(graph_nb_concu)
  
  # Insérer le graphique dans le classeur excel
  openxlsx::insertPlot(wb_concu, sheet = "Graphiques", startRow = 1, startCol = 12, 
             width = 8, height = 5)
  
  # Sauvegarder le fichier excel
  openxlsx::saveWorkbook(wb_concu, 
               file = here::here(folder_output, 
                           stringr::str_glue("{doc_title}-{seuil_2}.xlsx")), 
               overwrite = TRUE)
  
  
  # Graphique pour la part des produits sélectionnés dans le commerce fr
  graph_part_commerce <- 
    # Créer une séquence de numérique correspondant au nombre de seuils
    # Permet de sélectionner les df correspondant aux bons seuils dans la fonction
    seq_along(alpha_vector) |> 
    # Exécuter la fonction pour chaque seuil
    purrr::map(
      \(num) part_produit_total_function(num, df, alpha_vector)) |> 
    # Lier tous les df en un seul
    purrr::list_rbind() |> 
    # Créer le graphique
    ggplot2::ggplot(ggplot2::aes(x = as.character(seuil), y = share, color = share_type)) +
    ggplot2::geom_line(aes(group = share_type), linewidth = 1.1) +
    ggplot2::geom_point()+
    ggplot2::labs(
      x = "Seuils de gamme",
      y = "Part des produits dans le commerce français",
      title = "Part des produits sélectionnés dans le commerce français total et haut de gamme",
      color = "Type de commerce français"
    ) +
    ggplot2::scale_color_brewer(palette = "Paired") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor.x = ggplot2::element_blank()
    )
  
  # Afficher le graphique : permet d'être inclus dans le fichier excel
  print(graph_part_commerce)
  
  # Insérer le graphique dans le classeur excel
  openxlsx::insertPlot(wb_concu, sheet = "Graphiques", startRow = 28, startCol = 1,
             width = 8, height = 5)
  
  # Sauvegarder le fichier excel
  openxlsx::saveWorkbook(wb_concu, 
               file = here::here(folder_output, 
                           stringr::str_glue("{doc_title}-{seuil_2}.xlsx")), 
               overwrite = TRUE)
  
}



# Fonction pour créer le document après retirement des outliers -----------
exploration_haut_gamme_func <- function(baci, ponderate, 
                                        years = NULL, codes = NULL, 
                                        method_outliers = "classic", 
                                        seuil_H_outliers, seuil_L_outliers,
                                        alpha_H_gammes, seuil_2_gammes = 0.75,
                                        doc_title){
  # CLean les outliers
  analyse.competitivite::clean_uv_outliers(
    baci = baci,
    years = years,
    codes = codes,
    method = method_outliers,
    seuil_H = seuil_H_outliers,
    seuil_L = seuil_L_outliers,
    visualisation = FALSE,
    path_output = NULL,
    return_output = TRUE,
    return_pq = TRUE
  ) |> 
    # Calculer les gammes selon la méthode de Fontagné 1997
    # écarts des uv à la médianne pondérée
    analyse.competitivite::gamme_ijkt_fontagne_1997(
      alpha_H = alpha_H_gammes,
      ponderate = ponderate,
      years = NULL,
      codes = NULL,
      pivot = "longer", 
      return_output = TRUE,
      return_pq = TRUE,
      path_output = NULL,
      remove = FALSE
    ) |> 
    # Création du fichier d'exploration des seuils
    file_exploration_seuils_function(
      alpha_vector = alpha_H_gammes,
      seuil_2 = seuil_2_gammes,
      folder_output = path_df_exploration_folder,
      df_product = df_product,
      doc_title = doc_title
    )
}

