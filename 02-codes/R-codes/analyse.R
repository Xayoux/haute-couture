#  ------------------------------------------------------------------------
#
# Title : Etude de la compétitivité des produits français de la haute couture
#    By : Romain CAPLIEZ...
#  Date : 2024-03-18
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


# Créer l'arborescence des dossiers utilisés pour les différents outputs
# Importer les variables pour les chemins d'accès
source(here("02-codes", "R-codes", "00-creation-arborescence-folder.R"))


# Créer la liste des produits à utiliser ------------------------------------

# Vecteur contenant les numéros des chapitres / des sous-sections de la nomenclature pour les produits voulus
chapter_codes <- c(4202, 4203, 61, 62, 64, 6504, 6505, 6506, 7113, 7114, 7116, 7117)

# Créer un dataframe contenant les correspondances entre les codes produits de la nomenclature HS22 et HS92
df_product <- 
  extract_product(
    codes_vector = chapter_codes,
    path_output = here(path_df_analyse_folder, "01-codes-produits.xlsx"),
    revision_origin = "HS22",
    revision_destination = "HS92",
    export = TRUE,
    return_df = TRUE,
    correspondance = TRUE
  )

remove(chapter_codes)

# Télécahrger la base de données BACI -------------------------------------
# dl_baci(
#   dl_folder = path_baci_folder_parquet_origine, rm_csv = FALSE
# )


# Analyse des outliers -----------------------------------------------------

# Définition des seuils pour les méthodes classiques et fh13
seuil_H_vector <- c(0.99, 0.975, 0.95, 0.90)
seuil_L_vector <- c(0.01, 0.025, 0.05, 0.10)

# Définition des seuils pour la méthode sd
seuil_H_vector_sd <- c(1, 2, 3, 4)
seuil_L_vector_sd <- c(1, 2, 3, 4)

# Analyse des share du commerce sans outliers avec la méthode classic
list_eval_outliers_classic <- 
  eval_outliers_share(
    path_baci_parquet = path_baci_folder_parquet_origine,
    years = 2010:2022,
    codes = unique(df_product$HS92),
    method = "classic",
    seuil_H_vector = seuil_H_vector,
    seuil_L_vector = seuil_L_vector,
    graph = TRUE,
    path_graph_output = here(path_graphs_exploration_folder, 
                             "outliers-methode-classic.png")
  )

# Analyse de la distribution des diff de vu avec la méthode classic
eval_outliers_dist(
  path_baci_parquet = path_baci_folder_parquet_origine,
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

# Analyse des share du commerce sans outliers avec la méthode fh13
list_eval_outliers_fh13 <- 
  eval_outliers_share(
    path_baci_parquet = path_baci_folder_parquet_origine,
    years = 2010:2022,
    codes = unique(df_product$HS92),
    method = "fh13",
    seuil_H_vector = seuil_H_vector,
    seuil_L_vector = seuil_L_vector,
    graph = TRUE,
    path_graph_output = here(path_graphs_exploration_folder, 
                             "outliers-methode-fh13.png")
  )

# Analyse de la distribution des diff de vu avec la méthode fh13
eval_outliers_dist(
  path_baci_parquet = path_baci_folder_parquet_origine,
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

# Analyse des share du commerce sans outlier avec la méthode sd
list_eval_outliers_sd <- 
  eval_outliers_share(
    path_baci_parquet = path_baci_folder_parquet_origine,
    years = 2010:2022,
    codes = unique(df_product$HS92),
    method = "sd",
    seuil_H_vector = seuil_H_vector_sd,
    seuil_L_vector = seuil_L_vector_sd,
    graph = TRUE,
    path_graph_output = here(path_graphs_exploration_folder, 
                             "outliers-methode-sd.png")
  )

# Analyse de la distribution des diff de vu avec la méthode sd
eval_outliers_dist(
  path_baci_parquet = path_baci_folder_parquet_origine,
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

remove(seuil_H_vector, seuil_L_vector, seuil_H_vector_sd, seuil_L_vector_sd,
       list_eval_outliers_classic, list_eval_outliers_fh13, list_eval_outliers_sd)



# Exploration des != seuils sur le nb de produits et de concu ---------------
# Importer fonction pour faire l'exploration des seuils
source(here("02-codes", "R-codes", "01-exploration-seuils-function.R"))

# Définition de plusieurs seuils différents : méthode fontagné 1997
seuils_haut_gamme <- c(1.15, 1.25, 1.5, 1.75, 2, 2.5, 2.75, 3)

# Outliers définis comme 1% et 99%
exploration_haut_gamme_func(
  baci = path_baci_folder_parquet_origine,
  years = 2010,
  codes = unique(df_product$HS92),
  method_outliers = "classic",
  seuil_H_outliers = 0.99,
  seuil_L_outliers = 0.01,
  alpha_H_gammes = seuils_haut_gamme,
  seuil_2_gammes = 0.75,
  doc_title = str_glue("products-nb-concu-fontagne1997-outliers-classic99-seuil2-")
)

# Outliers définis comme 5% et 95%
exploration_haut_gamme_func(
  baci = path_baci_folder_parquet_origine,
  years = 2010,
  codes = unique(df_product$HS92),
  method_outliers = "classic",
  seuil_H_outliers = 0.95,
  seuil_L_outliers = 0.05,
  alpha_H_gammes = seuils_haut_gamme,
  seuil_2_gammes = 0.75,
  doc_title = str_glue("products-nb-concu-fontagne1997-outliers-classic95-seuil2-")
)

# Outliers définis comme supérieurs à 1 sd
exploration_haut_gamme_func(
  baci = path_baci_folder_parquet_origine,
  years = 2010,
  codes = unique(df_product$HS92),
  method_outliers = "sd",
  seuil_H_outliers = 1,
  seuil_L_outliers = 1,
  alpha_H_gammes = seuils_haut_gamme,
  seuil_2_gammes = 0.75,
  doc_title = str_glue("products-nb-concu-fontagne1997-outliers-sd1-seuil2-")
)

# Outliers définis comme supérieurs à 2 sd
exploration_haut_gamme_func(
  baci = path_baci_folder_parquet_origine,
  years = 2010,
  codes = unique(df_product$HS92),
  method_outliers = "sd",
  seuil_H_outliers = 2,
  seuil_L_outliers = 2,
  alpha_H_gammes = seuils_haut_gamme,
  seuil_2_gammes = 0.75,
  doc_title = str_glue("products-nb-concu-fontagne1997-outliers-sd2-seuil2-")
)

remove(
  exploration_seuil_haut_gamme, df_nb_concu_by_seuil, df_nb_product_by_seuil,
  part_produit_total_function, file_exploration_seuils_function, 
  exploration_haut_gamme_func, seuils_haut_gamme
)

gc()


# Création de la base BACI utilisée ---------------------------------------
source(here("02-codes", "R-codes", "02-create-baci-processed.R"))

create_baci_processed(
  baci = path_baci_folder_parquet_origine,
  years = 2010:2022,
  codes = unique(df_product$HS92),
  method_outliers = 'classic',
  seuil_H_outliers = 0.99,
  seuil_L_outliers = 0.01,
  alpha_H_gamme = 2,
  seuil_2_HG = 0.75,
  path_output = path_baci_processed,
  return_output = TRUE,
  return_pq = FALSE,
  remove = TRUE
)

remove(create_baci_processed)
gc()


# Parts de marché de chaque exportateur -----------------------------------


path_baci_processed |>
  market_share(
    summarize_v = "exporter",
    by = NULL,
    seuil = 0,
    path_output = NULL,
    return_output = TRUE,
    return_pq = FALSE
  ) 






















