#  ------------------------------------------------------------------------
#
# Title : Exploration des données de BACI
#    By : Romain CAPLIEZ...
#  Date : 2024-05-06
#
#  ------------------------------------------------------------------------


# 1 - Préparation de l'analyse --------------------------------------------

# Tester si devtools est installé. Si ce n'est pas le cas, l'installer
# Permet d'installer le package concordance de github pour pouvoir effectuer 
# Des correspondances avec la dernière version de la nomenclature.
if(!require(devtools)) install.packages("devtools")
if(!require(analyse.competitivite)) install_github("Xayoux/analyse.competitivite", dependencies=TRUE)
if(!require(concordance)) install_github("insongkim/concordance", dependencies=TRUE)
if(!require(here)) install.packages("here")
if(!require(readxl)) install.packages("readxl")
if(!require(arrow)) install.packages("arrow")
if(!require(writexl)) install.packages("writexl")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(scales)) install.packages("scales")

options(scipen = 999)


# Créer l'arborescence des dossiers utilisés pour les différents outputs
# Importer les variables pour les chemins d'accès
source(here("02-codes", "R-codes", "00-creation-arborescence-folder.R"))


# Exploration des outliers ------------------------------------------------

# Définition des seuils pour les méthodes classiques et fh13
seuil_H_vector <- c(0.99, 0.975, 0.95, 0.90)
seuil_L_vector <- c(0.01, 0.025, 0.05, 0.10)

# Définition des seuils pour la méthode sd
seuil_H_vector_sd <- c(1, 2, 3, 4)
seuil_L_vector_sd <- c(1, 2, 3, 4)

# Analyse des share du commerce sans outliers avec la méthode classic
list_eval_outliers_classic <- 
  eval_outliers_share(
    baci = path_baci_folder_parquet_origine,
    years = 2010:2022,
    codes = unique(df_product$HS92),
    method = "classic",
    seuil_H_vector = seuil_H_vector,
    seuil_L_vector = seuil_L_vector,
    graph = TRUE,
    path_graph_output = here(path_graphs_exploration_folder, 
                             "outliers-methode-classic.png")
  )

gc()

# Analyse de la distribution des diff de vu avec la méthode classic
eval_outliers_dist(
  baci = path_baci_folder_parquet_origine,
  years = 2010:2022,
  codes = unique(df_product$HS92),
  method = "classic",
  seuil_H_vector = seuil_H_vector,
  seuil_L_vector = seuil_L_vector,
  graph_type = "density",
  ref = "kt",
  wrap = TRUE,
  print = TRUE,
  output_type = "xlsx",
  path_output = here(path_df_exploration_folder, "test-outliers-classic.xlsx") 
)

gc()

# Analyse des share du commerce sans outliers avec la méthode fh13
list_eval_outliers_fh13 <- 
  eval_outliers_share(
    baci = path_baci_folder_parquet_origine,
    years = 2010:2022,
    codes = unique(df_product$HS92),
    method = "fh13",
    seuil_H_vector = seuil_H_vector,
    seuil_L_vector = seuil_L_vector,
    graph = TRUE,
    path_graph_output = here(path_graphs_exploration_folder, 
                             "outliers-methode-fh13.png")
  )

gc()

# Analyse de la distribution des diff de vu avec la méthode fh13
eval_outliers_dist(
  baci = path_baci_folder_parquet_origine,
  years = 2010:2022,
  codes = unique(df_product$HS92),
  method = "fh13",
  seuil_H_vector = seuil_H_vector,
  seuil_L_vector = seuil_L_vector,
  graph_type = "density",
  ref = "kt",
  wrap = TRUE,
  print = TRUE,
  output_type = "xlsx",
  path_output = here(path_df_exploration_folder, "test-outliers-fh13.xlsx") 
)

gc()

# Analyse des share du commerce sans outlier avec la méthode sd
list_eval_outliers_sd <- 
  eval_outliers_share(
    baci = path_baci_folder_parquet_origine,
    years = 2010:2022,
    codes = unique(df_product$HS92),
    method = "sd",
    seuil_H_vector = seuil_H_vector_sd,
    seuil_L_vector = seuil_L_vector_sd,
    graph = TRUE,
    path_graph_output = here(path_graphs_exploration_folder, 
                             "outliers-methode-sd.png")
  )

gc()

# Analyse de la distribution des diff de vu avec la méthode sd
eval_outliers_dist(
  baci = path_baci_folder_parquet_origine,
  years = 2010:2022,
  codes = unique(df_product$HS92),
  method = "sd",
  seuil_H_vector = seuil_H_vector_sd,
  seuil_L_vector = seuil_L_vector_sd,
  graph_type = "density",
  ref = "kt",
  wrap = TRUE,
  print = TRUE,
  output_type = "xlsx",
  path_output = here(path_df_exploration_folder, "test-outliers-sd.xlsx") 
)

# Analyse des share du commerce sans outlier avec la méthode be11
list_eval_outliers_be11 <-
  eval_outliers_share(
    baci = path_baci_folder_parquet_origine,
    years = 2010:2022,
    codes = unique(df_product$HS92),
    method = "be11",
    seuil_H_vector = c(80, 100),
    seuil_L_vector = c(80, 100),
    graph = TRUE,
    path_graph_output = here(path_graphs_exploration_folder,
                             "outliers-methode-be11.png")
  )

gc()

# Analyse de la distribution des diff de vu avec la méthode be11
eval_outliers_dist(
  baci = path_baci_folder_parquet_origine,
  years = 2010:2022,
  codes = unique(df_product$HS92),
  method = "be11",
  seuil_H_vector = 100,
  seuil_L_vector = 100,
  graph_type = "density",
  ref = "kt",
  wrap = TRUE,
  print = TRUE,
  output_type = "xlsx",
  path_output = here(path_df_exploration_folder, "test-outliers-be11.xlsx")
)

remove(seuil_H_vector, seuil_L_vector, seuil_H_vector_sd, seuil_L_vector_sd,
       list_eval_outliers_classic, list_eval_outliers_fh13, list_eval_outliers_sd,
       list_eval_outliers_be11)

gc()


# Exploration des seuils sur nb prod et concu ------------------------------

# Importer fonction pour faire l'exploration des seuils
source(here("02-codes", "R-codes", "01-exploration-seuils-function.R"))

# Définition de plusieurs seuils différents : méthode fontagné 1997
seuils_haut_gamme <- c(1.15, 1.25, 1.5, 1.75, 2, 2.5, 2.75, 3)

# Définir l'année de référence à utiliser
year_ref <- 2022

# Outliers définis comme 1% et 99%
exploration_haut_gamme_func(
  baci = path_baci_folder_parquet_origine,
  ponderate = "q",
  years = year_ref,
  codes = unique(df_product$HS92),
  method_outliers = "classic",
  seuil_H_outliers = 0.99,
  seuil_L_outliers = 0.01,
  alpha_H_gammes = seuils_haut_gamme,
  seuil_2_gammes = 0.75,
  doc_title = str_glue("products-nb-concu-fontagne1997-outliers-classic99-seuil2-")
)

gc()

# Outliers définis comme 5% et 95%
exploration_haut_gamme_func(
  baci = path_baci_folder_parquet_origine,
  ponderate = "q",
  years = year_ref,
  codes = unique(df_product$HS92),
  method_outliers = "classic",
  seuil_H_outliers = 0.95,
  seuil_L_outliers = 0.05,
  alpha_H_gammes = seuils_haut_gamme,
  seuil_2_gammes = 0.75,
  doc_title = str_glue("products-nb-concu-fontagne1997-outliers-classic95-seuil2-")
)

gc()

# Outliers définis comme supérieurs à 1 sd
exploration_haut_gamme_func(
  baci = path_baci_folder_parquet_origine,
  ponderate = "q",
  years = year_ref,
  codes = unique(df_product$HS92),
  method_outliers = "sd",
  seuil_H_outliers = 1,
  seuil_L_outliers = 1,
  alpha_H_gammes = seuils_haut_gamme,
  seuil_2_gammes = 0.75,
  doc_title = str_glue("products-nb-concu-fontagne1997-outliers-sd1-seuil2-")
)

gc()

# Outliers définis comme supérieurs à 2 sd
exploration_haut_gamme_func(
  baci = path_baci_folder_parquet_origine,
  ponderate = "q",
  years = year_ref,
  codes = unique(df_product$HS92),
  method_outliers = "sd",
  seuil_H_outliers = 2,
  seuil_L_outliers = 2,
  alpha_H_gammes = seuils_haut_gamme,
  seuil_2_gammes = 0.75,
  doc_title = str_glue("products-nb-concu-fontagne1997-outliers-sd2-seuil2-")
)

gc()

# Outliers définis comme supérieurs à 3 sd
exploration_haut_gamme_func(
  baci = path_baci_folder_parquet_origine,
  ponderate = "q",
  years = year_ref,
  codes = unique(df_product$HS92),
  method_outliers = "sd",
  seuil_H_outliers = 3,
  seuil_L_outliers = 3,
  alpha_H_gammes = seuils_haut_gamme,
  seuil_2_gammes = 0.75,
  doc_title = str_glue("products-nb-concu-fontagne1997-outliers-sd3-seuil2-")
)

# Outliers définis avec méthode be11
exploration_haut_gamme_func(
  baci = path_baci_folder_parquet_origine,
  ponderate = "q",
  years = year_ref,
  codes = unique(df_product$HS92),
  method_outliers = "be11",
  seuil_H_outliers = 100,
  seuil_L_outliers = 100,
  alpha_H_gammes = seuils_haut_gamme,
  seuil_2_gammes = 0.75,
  doc_title = str_glue("products-nb-concu-fontagne1997-outliers-be11-seuil2-")
)

remove(
  exploration_seuil_haut_gamme, df_nb_concu_by_seuil, df_nb_product_by_seuil,
  part_produit_total_function, file_exploration_seuils_function, 
  exploration_haut_gamme_func, seuils_haut_gamme, year_ref
)

gc()


# Regarder nb produits en dynamique ---------------------------------------
source(here("02-codes", "R-codes", "02-nb-products-HG-by-years.R"))

nb_product_by_year_func(
  baci = path_baci_folder_parquet_origine,
  ponderate = "q",
  years = 2010:2022,
  codes = unique(df_product$HS92),
  method_outliers = 'sd',
  seuil_H_outliers = 3,
  seuil_L_outliers = 3,
  alpha_H_gamme = 3,
  seuil_2_HG = 0.75,
  path_output = here(path_df_exploration_folder, "list-products-HG-by-year.xlsx"),
  remove = TRUE
)

gc()


# Exploration des régions d'exportation -----------------------------------

# Importer les fonctions exports_by_sector_regions / imports_by_sector_regions
source(here("02-codes", "R-codes", "fonctions", "exportations_by_sector_regions.R"))

# 1 - Regarder quelles sont les régions importatrices de produits HG 

# Importer la liste des produits HG sélectionnés pour la France
df_products_HG <- 
  here(path_df_analyse_folder, "02-list_k_concu.xlsx") |>
  read_xlsx(sheet = "product_HG_france")

# Créer un workbook pour enregistrer les résultats
wb_export_regions <- createWorkbook()

# Nom de la première feuille
sheet_name <- "Export_regions"

# Créer le dataframe des parts de marché par région
df_export_regions_chelem <- 
  # Ouvrir BACI clean
  path_baci_processed |> 
  open_dataset() |> 
  collect() |> 
  # Regrouper les deux régions d'Afriques (question de nb de régions et de couleurs)
  mutate(
    exporter_name_region =
      case_when(
        exporter_name_region %in% c("North Africa", "Sub-Sahara Africa") ~ "Africa",
        .default = exporter_name_region
      )
  ) |>
  arrow_table() |> 
  # Calculer les parts de marché des régions selon les 4 secteurs
  market_share(
    summarize_k = "sector",
    summarize_v = "exporter_name_region",
    by = NULL,
    seuil = 0,
    years = 2010:2022,
    codes = unique(df_products_HG$k),
    path_output = NULL,
    return_output = TRUE,
    return_pq = FALSE
  ) |> 
  arrange(t, sector, desc(market_share_t_k_i)) |> 
  select(-q_t_k_i)

# Faire la représentation graphique en area
graph_export_regions_chelem <- 
  df_export_regions_chelem |>
  ggplot(aes(x = t, y = market_share_t_k_i, fill = exporter_name_region)) +
  geom_area() +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    x = "Année",
    y = "Parts de marché",
    title = "Exportations haut de gamme par régions de Chelem",
    fill = ""
  ) +
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
  ) +
  facet_wrap(~sector, scales = "free_y")


# Afficher le graphique pour le sauvegarder
print(graph_export_regions_chelem)

# Ajouter une feuille au workbook
addWorksheet(wb_export_regions, sheet_name)

# Ecrire les données et le graphique dans le workbook
writeData(wb_export_regions, sheet = sheet_name, x = df_export_regions_chelem)

insertPlot(wb_export_regions, sheet = sheet_name, startRow = 1, startCol = 7, 
           width = 15, height = 8)

# Sauvegarder le workbook
saveWorkbook(
  wb_export_regions, 
  here(path_df_exploration_folder, "regions-exports", "exportations_by_sector_regions.xlsx"), 
  overwrite = TRUE
)


# 2 - Données d'exportation des pays au sein de chaqe région et secteur 
# Définir la liste des régions étudiée
df_export_regions_chelem |> 
  pull(exporter_name_region) |>
  unique() |>
  # Pour chaque région, appliquer la fonction exportation_by_sector_regions
  # Regarder les parts de marché des pays au sein d'une région
  walk(
    \(region) export_by_sector_regions(
      path_baci_processed_parquet = path_baci_processed,
      exporter_region = region,
      year_ref = 2022,
      seuil_market_share = 3,
      path_output = here(path_df_exploration_folder, "regions-exports", "exportations_by_sector_regions.xlsx"),
      wb = wb_export_regions
    )
  )


# Exploration des régions d'importation -----------------------------------

# 1 - Regarder quelles sont les régions importatrices de produits HG 

# Importer la liste des produits HG sélectionnés pour la France
df_products_HG <- 
  here(path_df_analyse_folder, "02-list_k_concu.xlsx") |>
  read_xlsx(sheet = "product_HG_france")

# Créer un workbook pour enregistrer les résultats
wb_import_regions <- createWorkbook()

# Nom de la première feuille
sheet_name <- "Import_regions"

# Créer le dataframe des parts de marché par région
df_import_regions_chelem <- 
  # Ouvrir BACI clean
  path_baci_processed |> 
  open_dataset() |> 
  collect() |> 
  # Regrouper les deux régions d'Afriques (question de nb de régions et de couleurs)
  mutate(
    importer_name_region =
      case_when(
        importer_name_region %in% c("North Africa", "Sub-Sahara Africa") ~ "Africa",
        .default = importer_name_region
      )
  ) |>
  arrow_table() |> 
  # Calculer les parts de marché des régions selon les 4 secteurs
  market_share(
    summarize_k = "sector",
    summarize_v = "importer_name_region",
    by = NULL,
    seuil = 0,
    years = 2010:2022,
    codes = unique(df_products_HG$k),
    path_output = NULL,
    return_output = TRUE,
    return_pq = FALSE
  ) |> 
  arrange(t, sector, desc(market_share_t_k_i)) |> 
  select(-q_t_k_i)

# Faire la représentation graphique en area
graph_import_regions_chelem <- 
  df_import_regions_chelem |>
  ggplot(aes(x = t, y = market_share_t_k_i, fill = importer_name_region)) +
  geom_area() +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    x = "Année",
    y = "Parts de marché",
    title = "Importations haut de gamme par régions de Chelem",
    fill = ""
  ) +
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
  ) +
  facet_wrap(~sector, scales = "free_y")


# Afficher le graphique pour le sauvegarder
print(graph_import_regions_chelem)

# Ajouter une feuille au workbook
addWorksheet(wb_import_regions, sheet_name)

# Ecrire les données et le graphique dans le workbook
writeData(wb_import_regions, sheet = sheet_name, x = df_import_regions_chelem)

insertPlot(wb_import_regions, sheet = sheet_name, startRow = 1, startCol = 7, 
           width = 15, height = 8)

# Sauvegarder le workbook
saveWorkbook(
  wb_import_regions, 
  here(path_df_exploration_folder, "regions-imports", "importations_by_sector_regions.xlsx"), 
  overwrite = TRUE
)


# 2 - Données d'exportation des pays au sein de chaqe région et secteur 
# Définir la liste des régions étudiée
df_import_regions_chelem |> 
  pull(importer_name_region) |>
  unique() |>
  # Pour chaque région, appliquer la fonction exportation_by_sector_regions
  # Regarder les parts de marché des pays au sein d'une région
  walk(
    \(region) import_by_sector_regions(
      path_baci_processed_parquet = path_baci_processed,
      importer_region = region,
      year_ref = 2022,
      seuil_market_share = 5,
      path_output = here(path_df_exploration_folder, "regions-imports", "importations_by_sector_regions.xlsx"),
      wb = wb_import_regions
    )
  )

remove(df_products_HG, wb_export_regions, wb_import_regions, sheet_name, 
       df_export_regions_chelem, graph_export_regions_chelem, 
       df_import_regions_chelem, graph_import_regions_chelem)










