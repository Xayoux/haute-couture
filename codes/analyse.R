#  ------------------------------------------------------------------------
#
# Title : Etude de la compétitivité des produits français de la haute couture
#    By : Romain CAPLIEZ...
#  Date : 2024-03-18
#
#  ------------------------------------------------------------------------


# Importer les librairies utilisées ---------------------------------------

# Tester si devtools est installé. Si ce n'est pas le cas, l'installer
# Permet d'installer le package concordance de github pour pouvoir effectuer 
# Des correspondances avec la dernière version de la nomenclature.
if(!require(devtools)) install.packages("devtools")
if(!require(concordance)) install_github("insongkim/concordance", dependencies=TRUE)
library(tidyverse)
library(here)
library(analyse.competitivite)
library(readxl)
library(tictoc)
library(arrow)
library(xtable)
library(patchwork)
library(writexl)
library(openxlsx)


# Créer la liste des produits à utiliser ----------------------------------

# Vecteur contenant les numéros des chapitres / des sous-sections de la nomenclature pour les produits voulus
chapter_codes <- c(4202, 4203, 61, 62, 64, 6504, 6505, 6506, 7113, 7114, 7116, 7117)

# Créer un dataframe contenant les correspondances entre les codes produits de la nomenclature HS22 et HS92
df_product <- 
  extract_product(
    codes_vector = chapter_codes,
    path_output = here("processed-data", "codes-produits.xlsx"),
    revision_origin = "HS22",
    revision_destination = "HS92",
    export = TRUE,
    return_df = TRUE,
    correspondance = TRUE
  )

remove(chapter_codes)

# Créer la base de données BACI ------------------------------------------
# dl_baci(
#   dl_folder = here("..", "BACI2"),rm_csv = FALSE
# )


# Définition des produits de luxe -----------------------------------------

# Regarder la concu et le nb de produits pour différents seuils -----------
# Créer le dataframe contenant le calcul des gammes selon la méthode de Fontagné (1997)
# Seuil à 1.5 -> 50% supérieur à la médianne mondiale pondérée et 2.5
# Année de référence : 2010

# Définition de plusieurs seuils différents
seuils <- c(1.15, 1.25, 1.5, 1.75, 2, 2.5, 3)

# Calcul des gammes pour chaque seuil pour l'année 2010
gamme_ijkt_fontagne_1997(
  path_baci_parquet = here("..", "BACI2", "BACI-parquet"),
  alpha_H = seuils,
  years = c(2010),
  codes = unique(df_product$HS92),
  pivot = "longer", 
  return_output = TRUE,
  path_output = here("processed-data", "BACI-gamme"),
  remove = TRUE
) 

exploration_seuil_haut_gamme <- function(alpha, wb){
  # Dataframe avec la part de chaque gamme dans le commerce de chaque produit par exportateur
  df_gammes <- 
    here("processed-data", "BACI-gamme") |>
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
    # Calculer la part de chaque gamme
    mutate(
      .by = c(t, k, exporter),
      share_total_v_gamme_tikg = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
    )
  
  # Dataframe avec les produits de luxe français
  df_products_luxes_fr <- 
    df_gammes |> 
    filter(
      # exporter == "FRA",
      gamme_fontagne_1997 == "H",
      share_total_v_gamme_tikg >= 0.5
    ) |> 
    # Calculer la part de marché sur chaque produits pour tout le monde
    # Uniquement marché du haut de gamme
    # Elimine le produit si part de marché de France trop faible
    mutate(
      .by = k, 
      market_share = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
    ) |> 
    # Garde uniquement les lignes françaises si la part de marché est >= 5%
    filter(
      exporter == "FRA",
      market_share >= 0.05
    ) |> 
    select(t, k, share_total_v_gamme_tikg, market_share) |> 
    arrange(k)
  
  # Dataframe avec les concurrents sur les produits de luxe français
  df_concu_luxe <- 
    df_gammes |> 
    # Garder les produits de luxe français et les concurrents qui répondent aux critères
    filter(
      k %in% unique(df_products_luxes_fr$k),
      gamme_fontagne_1997 == "H",
      share_total_v_gamme_tikg >= 0.5
    ) |> 
    # Calculer la part de marché de chaque concurrent sur chaque produit 
    # !!!!! Uniquement marché du haut de gamme !!!!!
    mutate(
      .by = c(k),
      market_share = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
    ) |> 
    # Garder uniquement les concurrents qui ont plus de 5% de part de marché
    filter(
      market_share >= 0.05
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
  
  # Concurrents sur les produits haut de gamme + présence sur le nb de produits
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
  
  # Nombre de produits
  nb_products <- 
    df_products_luxes_fr |> 
    nrow()
  
  # Ajouter les données dans le classeur excel
  sheet_name <- paste("Seuil", alpha, sep = "_")
  addWorksheet(wb, sheetName = sheet_name)
  
  # Titres des données
  titles <- c(
    "Produits Sélectionnés ; Part du Haut de gamme pour la France",
    "Concurrents sur les produits sélectionnés",
    "Nombre de concurrents par produit",
    "Concurrents",
    "Nombre de produits"
  )
  
  # Variables à ajouter
  variables <- list(
    df_products_luxes_fr,
    df_concu_luxe,
    nb_concu_by_product,
    concurrents,
    nb_products
  )
  
  # Colonnes de départ pour chaque titre
  num_cols <- c(1, 7, 14, 18, 22)
  
  # Ajouter les données dans le classeur excel
  for (i in seq_along(titles)) {
    writeData(wb, sheet = sheet_name, x = titles[i], startCol = num_cols[i], startRow = 1)
    writeData(wb, sheet = sheet_name, x = variables[[i]], startCol = num_cols[i], startRow = 2)
  }
  
  # Sauvegarder le classeur excel
  saveWorkbook(wb, file = here("processed-data", "exploration-seuils.xlsx"), overwrite = TRUE)
  
  # Récupérer les données dans des variables
  return(variables)
}


# Créer un workbook pour enregistrer les données
wb_concu <- createWorkbook()
# Effectuer l'exploration pour chaque seuil
df <- 
  seuils |> 
  map(
    \(alpha) exploration_seuil_haut_gamme(alpha, wb_concu), 
    .progress = TRUE
  )



  
# Créer data frame pour le nombre de produits par chapitres selon les seuils
df_nb_product_by_seuil <- function(num){
  # Compter le nombre de produits par chapitre
  df_chapter <- 
    df[[num]][[1]] |> 
    select(k) |> 
    mutate(
      seuil = seuils[num],
      chapter = substr(k, 1, 2)
    ) |> 
    summarize(
      .by = c(seuil, chapter),
      nb_products = n()
    )
  
  # Compter le nombre de produit total
  df_total <- 
    df[[num]][[1]] |> 
    select(k) |> 
    mutate(
      seuil = seuils[num],
      chapter = "total"
    )|> 
    summarize(
      .by = c(seuil, chapter),
      nb_products = n()
    )
  
  # Fusionner les deux df
  df <- 
    df_chapter |> 
    bind_rows(df_total)
  
  return(df)
}

# Créer un dataframe par seuil puis fusionner les dataframes
1:7 |> 
  map(df_nb_product_by_seuil) |> 
  list_rbind() |> 
  mutate(seuil = as.character(seuil)) |> 
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

# Insérer le graphique dans le classeur excel
addWorksheet(wb_concu, sheetName = "Graphiques")
insertPlot(wb_concu, sheet = "Graphiques", startRow = 1, startCol = 1, width = 8, height = 5)
saveWorkbook(wb_concu, file = here("processed-data", "exploration-seuils.xlsx"), overwrite = TRUE)


# Créer data frame pour le nombre moyen de concurrents par chapitres selon les seuils
df_nb_concu_by_seuil <- function(num){
  # Faire la moyenne du nombre de concurrents par chapitres
  df_chapter <- 
    df[[num]][[3]] |> 
    mutate(
      seuil = seuils[num],
      chapter = substr(k, 1, 2)
    ) |> 
    summarize(
      .by = c(seuil, chapter),
      nb_concurrents = mean(nb_concurrents, na.rm = TRUE)
    )
  
  # Faire la moyenne du nombre de concurrents total
  df_total <- 
    df[[num]][[3]] |>  
    mutate(
      seuil = seuils[num],
      chapter = "Moyenne"
    )|> 
    summarize(
      .by = c(seuil, chapter),
      nb_concurrents = mean(nb_concurrents, na.rm = TRUE)
    )
  
  # Fusionner les deux df
  df <- 
    df_chapter |> 
    bind_rows(df_total)
  
  return(df)
}

# Créer un dataframe par seuil puis fusionner les dataframes
1:7 |> 
  map(df_nb_concu_by_seuil) |> 
  list_rbind() |> 
  mutate(seuil = as.character(seuil)) |> 
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

# Insérer le graphique dans le classeur excel
insertPlot(wb_concu, sheet = "Graphiques", startRow = 1, startCol = 12, width = 8, height = 5)
saveWorkbook(wb_concu, file = here("processed-data", "exploration-seuils.xlsx"), overwrite = TRUE)




# Analyse exploratoire des seuils sans 5% pour fr -------------------------
exploration_seuil_haut_gamme_2 <- function(alpha, wb){
  # Dataframe avec la part de chaque gamme dans le commerce de chaque produit par exportateur
  df_gammes <- 
    here("processed-data", "BACI-gamme") |>
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
    # Calculer la part de chaque gamme
    mutate(
      .by = c(t, k, exporter),
      share_total_v_gamme_tikg = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
    )
  
  # Dataframe avec les produits de luxe français
  df_products_luxes_fr <- 
    df_gammes |> 
    filter(
      # exporter == "FRA",
      gamme_fontagne_1997 == "H",
      share_total_v_gamme_tikg >= 0.5
    ) |> 
    # Calculer la part de marché sur chaque produits pour tout le monde
    # Uniquement marché du haut de gamme
    # Elimine le produit si part de marché de France trop faible
    mutate(
      .by = k, 
      market_share = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
    ) |> 
    # Garde uniquement les lignes françaises
    filter(
      exporter == "FRA"
    ) |> 
    select(t, k, share_total_v_gamme_tikg, market_share) |> 
    arrange(k)
  
  # Dataframe avec les concurrents sur les produits de luxe français
  df_concu_luxe <- 
    df_gammes |> 
    # Garder les produits de luxe français et les concurrents qui répondent aux critères
    filter(
      k %in% unique(df_products_luxes_fr$k),
      gamme_fontagne_1997 == "H",
      share_total_v_gamme_tikg >= 0.5
    ) |> 
    # Calculer la part de marché de chaque concurrent sur chaque produit 
    # !!!!! Uniquement marché du haut de gamme !!!!!
    mutate(
      .by = c(k),
      market_share = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
    ) |> 
    # Garder uniquement les concurrents qui ont plus de 5% de part de marché
    filter(
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
  
  # Concurrents sur les produits haut de gamme + présence sur le nb de produits
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
  
  # Nombre de produits
  nb_products <- 
    df_products_luxes_fr |> 
    nrow()
  
  # Ajouter les données dans le classeur excel
  sheet_name <- paste("Seuil", alpha, sep = "_")
  addWorksheet(wb, sheetName = sheet_name)
  
  # Titres des données
  titles <- c(
    "Produits Sélectionnés ; Part du Haut de gamme pour la France",
    "Concurrents sur les produits sélectionnés",
    "Nombre de concurrents par produit",
    "Concurrents",
    "Nombre de produits"
  )
  
  # Variables à ajouter
  variables <- list(
    df_products_luxes_fr,
    df_concu_luxe,
    nb_concu_by_product,
    concurrents,
    nb_products
  )
  
  # Colonnes de départ pour chaque titre
  num_cols <- c(1, 7, 14, 18, 22)
  
  # Ajouter les données dans le classeur excel
  for (i in seq_along(titles)) {
    writeData(wb, sheet = sheet_name, x = titles[i], startCol = num_cols[i], startRow = 1)
    writeData(wb, sheet = sheet_name, x = variables[[i]], startCol = num_cols[i], startRow = 2)
  }
  
  # Sauvegarder le classeur excel
  saveWorkbook(wb, file = here("processed-data", "exploration-seuils-2.xlsx"), overwrite = TRUE)
  
  # Récupérer les données dans des variables
  return(variables)
}


# Créer un workbook pour enregistrer les données
wb_concu <- createWorkbook()
# Effectuer l'exploration pour chaque seuil
df <- 
  seuils |> 
  map(
    \(alpha) exploration_seuil_haut_gamme_2(alpha, wb_concu), 
    .progress = TRUE
  )




# Créer data frame pour le nombre de produits par chapitres selon les seuils
df_nb_product_by_seuil <- function(num){
  # Compter le nombre de produits par chapitre
  df_chapter <- 
    df[[num]][[1]] |> 
    select(k) |> 
    mutate(
      seuil = seuils[num],
      chapter = substr(k, 1, 2)
    ) |> 
    summarize(
      .by = c(seuil, chapter),
      nb_products = n()
    )
  
  # Compter le nombre de produit total
  df_total <- 
    df[[num]][[1]] |> 
    select(k) |> 
    mutate(
      seuil = seuils[num],
      chapter = "total"
    )|> 
    summarize(
      .by = c(seuil, chapter),
      nb_products = n()
    )
  
  # Fusionner les deux df
  df <- 
    df_chapter |> 
    bind_rows(df_total)
  
  return(df)
}

# Créer un dataframe par seuil puis fusionner les dataframes
1:7 |> 
  map(df_nb_product_by_seuil) |> 
  list_rbind() |> 
  mutate(seuil = as.character(seuil)) |> 
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

# Insérer le graphique dans le classeur excel
addWorksheet(wb_concu, sheetName = "Graphiques")
insertPlot(wb_concu, sheet = "Graphiques", startRow = 1, startCol = 1, width = 8, height = 5)
saveWorkbook(wb_concu, file = here("processed-data", "exploration-seuils-2.xlsx"), overwrite = TRUE)


# Créer data frame pour le nombre moyen de concurrents par chapitres selon les seuils
df_nb_concu_by_seuil <- function(num){
  # Faire la moyenne du nombre de concurrents par chapitres
  df_chapter <- 
    df[[num]][[3]] |> 
    mutate(
      seuil = seuils[num],
      chapter = substr(k, 1, 2)
    ) |> 
    summarize(
      .by = c(seuil, chapter),
      nb_concurrents = mean(nb_concurrents, na.rm = TRUE)
    )
  
  # Faire la moyenne du nombre de concurrents total
  df_total <- 
    df[[num]][[3]] |>  
    mutate(
      seuil = seuils[num],
      chapter = "Moyenne"
    )|> 
    summarize(
      .by = c(seuil, chapter),
      nb_concurrents = mean(nb_concurrents, na.rm = TRUE)
    )
  
  # Fusionner les deux df
  df <- 
    df_chapter |> 
    bind_rows(df_total)
  
  return(df)
}

# Créer un dataframe par seuil puis fusionner les dataframes
1:7 |> 
  map(df_nb_concu_by_seuil) |> 
  list_rbind() |> 
  mutate(seuil = as.character(seuil)) |> 
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

# Insérer le graphique dans le classeur excel
insertPlot(wb_concu, sheet = "Graphiques", startRow = 1, startCol = 12, width = 8, height = 5)
saveWorkbook(wb_concu, file = here("processed-data", "exploration-seuils-2.xlsx"), overwrite = TRUE)





































