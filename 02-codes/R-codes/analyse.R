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
source(here("02-codes", "R-codes", "00-creation-arborescence-folder.R"))

# Définir le nom du dossier contenant les données BACI
{name_BACI_folder <- "BACI-2024"
  path_baci_folder_parquet_origine <- 
    here("..", "BACI-2024", "BACI-parquet")

  path_graphs_tests_folder <- here("04-output", "01-graphs", "01-tests")
  path_graphs_finals_folder <- here("04-output", "01-graphs", "02-finals")

  path_tables_tests_folder <- here("04-output", "02-tables", "01-tests")
  path_tables_finals_folder <- here("04-output", "02-tables", "02-finals")}


# Créer la liste des produits à utiliser ------------------------------------

# Vecteur contenant les numéros des chapitres / des sous-sections de la nomenclature pour les produits voulus
chapter_codes <- c(4202, 4203, 61, 62, 64, 6504, 6505, 6506, 7113, 7114, 7116, 7117)

# Créer un dataframe contenant les correspondances entre les codes produits de la nomenclature HS22 et HS92
df_product <- 
  extract_product(
    codes_vector = chapter_codes,
    path_output = here("03-processed-data", "01-dataframes", "01-codes-produits.xlsx"),
    revision_origin = "HS22",
    revision_destination = "HS92",
    export = TRUE,
    return_df = TRUE,
    correspondance = TRUE
  )

remove(chapter_codes)

# Télécahrger la base de données BACI -------------------------------------
# dl_baci(
#   dl_folder = here("..", name_BACI_folder),rm_csv = FALSE
# )


# Analyse des outliers -----------------------------------------------------

# Définition des seuils pour les méthodes classiques et fh13
seuil_H_vector <- c(0.99, 0.975, 0.95, 0.90)
seuil_L_vector <- c(0.01, 0.025, 0.05, 0.10)

# Analyse des outliers avec la méthode classique
list_eval_outliers_classic <- eval_outliers(
  path_baci_parquet = here("..", name_BACI_folder, "BACI-parquet"),
  years = 2010:2022,
  codes = unique(df_product$HS92),
  method = "classic",
  seuil_H_vector = seuil_H_vector,
  seuil_L_vector = seuil_L_vector,
  graph = TRUE,
  path_graph_output = here(path_graphs_tests_folder, 
                           "01-outliers-methode-classic.png")
)

# Analyse des outliers avec la méthode fh13
list_eval_outliers_fh13 <- eval_outliers(
  path_baci_parquet = here("..", name_BACI_folder, "BACI-parquet"),
  years = 2010:2022,
  codes = unique(df_product$HS92),
  method = "fh13",
  seuil_H_vector = seuil_H_vector,
  seuil_L_vector = seuil_L_vector,
  graph = TRUE,
  path_graph_output = here(path_graphs_tests_folder, 
                           "02-outliers-methode-fh13.png")
)
remove(seuil_H_vector, seuil_L_vector)

# Création de la base BACI utilisée ---------------------------------------


# Suppression des outliers avec la méthode 'classic' et seuil 0.01 et 0.99
add_chelem_classification(
  path_baci_parquet = path_baci_folder_parquet_origine,
  years = 2010:2022,
  codes = unique(df_product$HS92),
  path_output = here("03-processed-data", "00-BACI"),
  return = TRUE
)

clean_uv_outliers(
  path_baci_parquet = here("03-processed-data", "00-BACI"),
  years = 2010:2022,
  codes = unique(df_product$HS92),
  method = "classic",
  seuil_H = 0.99,
  seuil_L = 0.01,
  path_output = here("03-processed-data", "00-BACI"),
  return_output = TRUE
)


# Exploration des != seuils sur le nb de produits et de concu ---------------
# Définition de plusieurs seuils différents
seuils <- c(1.15, 1.25, 1.5, 1.75, 2, 2.5, 2.75, 3)

# Calcul des gammes pour chaque seuil pour l'année 2010
# Calcul selon la méthode Fontagné 1997 : écart à la médianne pondérée 
gamme_ijkt_fontagne_1997(
  path_baci_parquet = here("03-processed-data", "00-BACI"),
  alpha_H = seuils,
  years = 2010,
  codes = unique(df_product$HS92),
  pivot = "longer", 
  return_output = FALSE,
  path_output = here("03-processed-data", "02-BACI-gammes-fontagne-1997-exploration"),
  remove = TRUE
) 

# Excel contenant les données pour chaque seuil
# Importer la fonction depuis le fichier 01-exploration-seuils-function.R
{source(here("02-codes", "R-codes", "01-exploration-seuils-function.R"))
  
file_exploration_seuils_function(
  df_gammes_path = here("03-processed-data", "02-BACI-gammes-fontagne-1997-exploration"),
  alpha_vector = seuils,
  seuil_2 = 0.75,
  folder_output = here("03-processed-data", "01-dataframes"),
  df_product = df_product,
  doc_title = "02-exploration-outliers-0.99-classic-seuils-HG"
)

remove(df_nb_concu_by_seuil, df_nb_product_by_seuil, exploration_seuil_haut_gamme,
       file_exploration_seuils_function, part_produit_total_function, seuils)

gc()}





















