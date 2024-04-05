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
library(tidyverse)
library(arrow)
library(openxlsx)
library(here)
library(readxl)


# Sous-fonction pour écrire le fichier d'exploration de seuils ------------
# Fonction pour avoir des informations sur les données dans chaque seuil
exploration_seuil_haut_gamme <- function(df_gammes_path, alpha, seuil_2, wb,
                                         df_product, folder_output){
  
  # Dataframe avec la part de chaque gamme dans le commerce de chaque produit par exportateur
  df_gammes <- 
    df_gammes_path |>
    open_dataset() |>
    # Garder uniquement le seuil voulu
    filter(
      alpha_H == alpha
    ) |> 
    # Sommer tous les flux de la même catégorie pour un exportateur et un produit
    summarize(
      .by = c(t, k, exporter, gamme_fontagne_1997),
      total_v_tikg = sum(v, na.rm = TRUE) 
    ) |> 
    collect() |> 
    # Calculer la part de chaque gamme dans le commerce d'un produit par exportateur
    mutate(
      .by = c(t, k, exporter),
      share_total_v_gamme_tikg = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
    )
  
  
  # Dataframe pour sélectionner les produits de luxe fr
  df_products_luxes_fr <- 
    df_gammes |> 
    # Garder uniquement les données de haut de gamme
    filter(
      gamme_fontagne_1997 == "H"
    ) |> 
    # Calculer la part de marché sur le marché du haut de gamme
    mutate(
      .by = c(t,k), 
      market_share = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
    ) |> 
    # Garde uniquement les lignes françaises
    # Garde uniquement les lignes dont la part du haut de gamme est supérieure à un seuil
    filter(
      exporter == "FRA",
      share_total_v_gamme_tikg >= seuil_2 
    ) |> 
    select(t, k, share_total_v_gamme_tikg, market_share) |> 
    arrange(k)
  
  # Df sur les données  des produits de luxe français
  df_commerce_haut_gamme <- 
    df_gammes |> 
    # Garder uniquement les données françaises
    filter(
      exporter == "FRA"
    ) |>
    # Calculer la valeur totale du commerce français sur tous les biens
    mutate(
      .by = t,
      total_v_commerce = sum(total_v_tikg, na.rm = TRUE)
    ) |> 
    # Garder uniquement les produits sélectionnés précédemment
    # Garder que les données haut de gamme
    filter(
      k%in% unique(df_products_luxes_fr$k),
      gamme_fontagne_1997 == "H"
    ) |>
    # Calculer la part de chaque produit sélectionné par rapport au commerce fr total 
    mutate(
      .by = t,
      share_commerce_fr = total_v_tikg / total_v_commerce
    ) |> 
    # Trier la part dans le commerce fr par ordre décroissant
    arrange(desc(share_commerce_fr)) |> 
    select(!c(gamme_fontagne_1997, t, exporter))
  
  
  # Part des produits sélectionnés dans le commerce français total
  part_produits_total <- 
    df_commerce_haut_gamme|> 
    # Calculer la part des produits sélectionnés dans le commerce français total
    summarize(part_produit_total = sum(share_commerce_fr))
  
  
  # Part des produits sélectionnés dans le commerce français haut de gamme
  part_produits_haut_gamme <-
    df_gammes |> 
    # Garder seulement les biens français haut de gamme
    filter(
      gamme_fontagne_1997 == "H",
      exporter == "FRA"
    ) |>
    # Calculer la valeur commerciale totale des biens haut de gamme fr
    mutate(
      .by = t,
      total_v_haut_gamme = sum(total_v_tikg, na.rm = TRUE)
    ) |> 
    # Garder uniquement les biens sélectionnés
    filter(
      k%in% unique(df_products_luxes_fr$k)
    ) |>
    # Calculer la part des biens sélectionnés dans le commerce haut de gamme fr
    mutate(
      .by = t,
      share_commerce_fr_hg = total_v_tikg / total_v_haut_gamme
    ) |> 
    # Trier les données par ordre décroissant
    arrange(desc(share_commerce_fr_hg)) |>
    # Calculer la part ttale des produits sélectionnés dans le commerce haut de gamme fr
    summarize(part_produit_haut_gamme = sum(share_commerce_fr_hg))
  
  
  # Dataframe avec les données des concurrents sur les produits haut de gamme sélectionnés
  df_concu_luxe <- 
    df_gammes |> 
    # Garder les produits sélectionnés et les données haut de gamme
    filter(
      k %in% unique(df_products_luxes_fr$k),
      gamme_fontagne_1997 == "H",
    ) |> 
    # Calculer la part de marché de chaque concurrent sur chaque produit 
    # Part de marché sur le marché haut de gamme
    mutate(
      .by = c(k),
      market_share = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
    ) |> 
    # Garder les produits-pays dont la valeur du haut de gamme est >= à un seuil
    # Garder les produits-pays dont la part de marché est >= 5%
    # Garder tous les produits français haut de gamme dont la part dans le commerce du produit est >= seuil_2
    filter(
      share_total_v_gamme_tikg >= seuil_2,
      market_share >= 0.05 | exporter == "FRA"
    ) |> 
    # Garder uniquement les variables d'intérêt
    select(k, exporter, share_total_v_gamme_tikg, market_share) |> 
    # Trier les données
    arrange(k, desc(market_share)) |> 
    # Ajouter la description des produits HS6 retenus
    left_join(
      df_product |>
        select(HS92, description_HS92) |> distinct(),
      by = c("k" = "HS92")
    )
  
  
  # Nb de produits sur lesquels chaque concurrent est présent
  concurrents <- 
    df_concu_luxe |> 
    select(k, exporter) |> 
    summarize(
      .by = exporter, 
      nb = n()
    ) |> 
    arrange(desc(nb))
  
  
  # Nombre de concurrents par produit
  nb_concu_by_product <-
    df_concu_luxe |> 
    summarize(
      .by = k,
      nb_concurrents = n()
    )
  
  
  # Nombre de produits sélectionné
  nb_products <- 
    df_products_luxes_fr |> 
    summarize(nb_product = n())
  
  
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
  addWorksheet(wb, sheetName = sheet_name)
  
  # Colonnes de départ pour chaque titre/dataframes (hors chiffres indicatifs)
  num_cols <- c(3, 6, 13, 19)
  
  
  # AJouter les chiffres indicatifs dans la première colonne
  row <- 1 # permet de définir le numéro de ligne pour inscrire les données
  
  for (j in seq_along(unit_var)) {
    # Pour chaque valeur indicatives, l'inscrire dans le fichier excel
    writeData(wb, sheet = sheet_name, x = unit_var[[j]], startCol = 1, startRow = row)
    # Mettre un espace de Une ligne entre les données (1 ligne titre, 1 valeur, 1 espace = 3)
    row <- row + 3
  }
  
  
  # Ajouter les dataframes dans le classeur excel
  for (i in seq_along(titles)) {
    # Pour chaque dataframe : ajouter un titre ligne 1, colonne correspondante
    writeData(wb, sheet = sheet_name, x = titles[i], startCol = num_cols[i], startRow = 1)
    # Pour chaque dataframe : ajouter les données ligne 2, colonne correspondante
    writeData(wb, sheet = sheet_name, x = variables[[i]], startCol = num_cols[i], startRow = 2)
  }
  
  
  # Sauvegarder le classeur excel
  saveWorkbook(wb, 
               file = here(folder_output, 
                           str_glue("exploration-alpha-seuil-{seuil_2}.xlsx")), 
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
    select(k) |> 
    # Associer chaque produit à son chapitre (2 premiers chiffres)
    mutate(
      seuil = seuils[num],
      chapter = substr(k, 1, 2)
    ) |> 
    # Compter le nombre de produits par chapitre
    summarize(
      .by = c(seuil, chapter),
      nb_products = n()
    )
  
  # Compter le nombre de produit total
  df_total <- 
    # Sélectionner le df correspondant au nb de produits pour le seuil 'num'
    df[[num]][[1]][[1]] |> 
    # Sélectionner uniquement la colonne des produits
    select(k) |> 
    # Créer la variable total pour le nombre de produits global
    mutate(
      seuil = seuils[num],
      chapter = "total"
    )|> 
    # Compter le nombre de produits globaux
    summarize(
      .by = c(seuil, chapter),
      nb_products = n()
    )
  
  # Fusionner les deux df
  df <- 
    df_chapter |> 
    bind_rows(df_total)
  
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
    mutate(
      seuil = seuils[num],
      chapter = substr(k, 1, 2)
    ) |> 
    # Faire la moyenne du nombre de concurrents par chapitre
    summarize(
      .by = c(seuil, chapter),
      nb_concurrents = mean(nb_concurrents, na.rm = TRUE)
    )
  
  # Faire la moyenne du nombre de concurrents total
  df_total <- 
    # Sélectionner le df correspondant au nombre de concurrents pour le seuil 'num'
    df[[num]][[1]][[1]] |> 
    # Créer la variable moyenne pour le nombre de concurrents en moyenne
    mutate(
      seuil = seuils[num],
      chapter = "Moyenne"
    )|> 
    # Faire la moyenne du nombre de concurrents total
    summarize(
      .by = c(seuil, chapter),
      nb_concurrents = mean(nb_concurrents, na.rm = TRUE)
    )
  
  # Fusionner les deux df
  df <- 
    df_chapter |> 
    bind_rows(df_total)
  
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
    mutate(
      seuil = seuils[num],
      share_type = "Total"
    ) |> 
    # Renommer la variable de part pour permettre la fusion des df
    rename(share = part_produit_total)
  
  df_part_haut_gamme <- 
    # Sélectionner le df correspondant à la part des produits pour le seuil 'num'
    # Dans le commerce haut de gamme
    df[[num]][[2]][[3]] |> 
    # Créer la variable seuil et haut de gamme pour le graphique
    mutate(
      seuil = seuils[num],
      share_type = "Haut de game"
    ) |> 
    # Renommer la variable de part pour permettre la fusion des df
    rename(share = part_produit_haut_gamme)
  
  # Fusionner les deux df
  df <- 
    df_part_total |> 
    bind_rows(df_part_haut_gamme)
  
  # Retourner une liste de datframes (1 par seuil)
  return(df)
}


# Fonction pour créer le document d'analyse des seuils --------------------
file_exploration_seuils_function <- function(df_gammes_path, alpha_vector, seuil_2,
                                             df_product, folder_output){
  
  # Créer un workbook pour enregistrer les données
  wb_concu <- createWorkbook()
  
  # Effectuer l'exploration pour chaque seuil pour un seuil 2 donné
  df <- 
    # Effectuer la fonction pour chaque seuil
    alpha_vector |> 
    map(
      \(alpha) exploration_seuil_haut_gamme(
        df_gammes_path = df_gammes_path,
        alpha, seuil_2 = seuil_2, wb_concu, df_product, folder_output), 
      .progress = TRUE
    )
  
  
  # Créer une feuille dans le workbook pour les graphiques 
  addWorksheet(wb_concu, sheetName = "Graphiques")
  
  # Graphique pour le nombre de produits par chapitre selon les seuils
  graph_nb_product <- 
    # Créer uen séquence de numérique correspondant au nombre de seuils
    # Pemret de sélectionner les df correspodnant aux bon seuils dans la fonction
    seq_along(alpha_vector) |>
    # Exécuter la fonction pour chaque seuil
    map(
      \(num) df_nb_product_by_seuil(num, df, alpha_vector)) |> 
    # Lier tous les df en un seul
    list_rbind() |> 
    # Transformer la variable seuil en caractère pour avoir un axe x lisible
    mutate(seuil = as.character(seuil)) |> 
    # Créer le graphique
    ggplot(aes(x = seuil, y = nb_products, color = chapter)) +
    geom_line(aes(group = chapter), linewidth = 1.1) +
    labs(
      x = "Seuils de gamme",
      y = "Nombre de produits",
      title = "Nombre de produits par chapitre HS6 selon les seuils de gamme",
      color = "Chapitres HS6"
    ) +
    scale_color_brewer(palette = "Paired") +
    theme_bw() +
    theme(
      panel.grid.minor.x = element_blank()
    )
  
  # Afficher le graphique : permet d'être inclus dans le fichier excel
  print(graph_nb_product)
  
  # Insérer le graphique dans le classeur excel
  insertPlot(wb_concu, sheet = "Graphiques", startRow = 1, startCol = 1, 
             width = 8, height = 5)
  
  # Sauvegarder le fichier excel
  saveWorkbook(wb_concu, 
               file = here(folder_output, 
                           str_glue("exploration-alpha-seuil-{seuil_2}.xlsx")), 
               overwrite = TRUE)
  
  
  # Graphique pour le nombre moyen de concurrents par chapitre selon les seuils
  graph_nb_concu <- 
    # Créer une séquence de numérique correspondant au nombre de seuils
    # Permet de sélectionner les df correspondant aux bons seuils dans la fonction
    seq_along(alpha_vector) |> 
    # Exécuter la fonction pour chaque seuil
    map(
      \(num) df_nb_concu_by_seuil(num, df, alpha_vector)) |> 
    # Lier tous les df en un seul
    list_rbind() |> 
    # Transformer la variable seuil en caractère pour avoir un axe x lisible
    mutate(seuil = as.character(seuil)) |> 
    # Créer le graphique
    ggplot(aes(x = seuil, y = nb_concurrents, color = chapter)) +
    geom_line(aes(group = chapter), linewidth = 1.1) +
    labs(
      x = "Seuils de gamme",
      y = "Nombre de concurrents moyen",
      title = "Nombre de concurrents en moyenne par chapitre HS6 selon les seuils de gamme",
      color = "Chapitres HS6"
    ) +
    scale_color_brewer(palette = "Paired") +
    theme_bw() +
    theme(
      panel.grid.minor.x = element_blank()
    )
  
  # Afficher le graphique : permet d'être inclus dans le fichier excel 
  print(graph_nb_concu)
  
  # Insérer le graphique dans le classeur excel
  insertPlot(wb_concu, sheet = "Graphiques", startRow = 1, startCol = 12, 
             width = 8, height = 5)
  
  # Sauvegarder le fichier excel
  saveWorkbook(wb_concu, 
               file = here(folder_output, 
                           str_glue("exploration-alpha-seuil-{seuil_2}.xlsx")), 
               overwrite = TRUE)
  
  
  # Graphique pour la part des produits sélectionnés dans le commerce fr
  graph_part_commerce <- 
    # Créer une séquence de numérique correspondant au nombre de seuils
    # Permet de sélectionner les df correspondant aux bons seuils dans la fonction
    seq_along(alpha_vector) |> 
    # Exécuter la fonction pour chaque seuil
    map(
      \(num) part_produit_total_function(num, df, alpha_vector)) |> 
    # Lier tous les df en un seul
    list_rbind() |> 
    # Créer le graphique
    ggplot(aes(x = as.character(seuil), y = share, color = share_type)) +
    geom_line(aes(group = share_type), linewidth = 1.1) +
    geom_point()+
    labs(
      x = "Seuils de gamme",
      y = "Part des produits dans le commerce français",
      title = "Part des produits sélectionnés dans le commerce français total et haut de gamme",
      color = "Type de commerce français"
    ) +
    scale_color_brewer(palette = "Paired") +
    theme_bw() +
    theme(
      panel.grid.minor.x = element_blank()
    )
  
  # Afficher le graphique : permet d'être inclus dans le fichier excel
  print(graph_part_commerce)
  
  # Insérer le graphique dans le classeur excel
  insertPlot(wb_concu, sheet = "Graphiques", startRow = 28, startCol = 1,
             width = 8, height = 5)
  
  # Sauvegarder le fichier excel
  saveWorkbook(wb_concu, 
               file = here(folder_output, 
                           str_glue("exploration-alpha-seuil-{seuil_2}.xlsx")), 
               overwrite = TRUE)
  
}
