#  ------------------------------------------------------------------------
#
# Title :  Création de l'arborescence des dossiers pour le projet
#    By : 
#  Date : 2024-04-09
#
#  ------------------------------------------------------------------------


# Chemins d'accès (partie à modifier si besoin) ---------------------------

# Chemin d'accès au dossier contenant les données brutes de BACI
path_baci_folder_origine <- 
  here::here("..", "BACI-2024")

# Chemin d'accès au dossier contenant les données brutes de BACI en format parquet
path_baci_folder_parquet_origine <- 
  here::here("..", "BACI-2024", "BACI-parquet")

#Chemin d'accès pour les données BACi processed
path_baci_processed <-
  here::here("03-processed-data", "00-BACI")

# Chemin d'accès pour le dossier de stockage des autres données brutes
path_raw_data_folder <- here::here("01-raw-data")

# Chemins d'accès aux sous-dossiers pour le stockage des dataframes
path_df_exploration_folder <- 
  here::here("03-processed-data", "01-dataframes", "01-exploration")

path_df_analyse_folder <- 
  here::here("03-processed-data", "01-dataframes", "02-analyse")

# Chemins d'accès aux sous-dossiers pour le stockage des graphiques
path_graphs_exploration_folder <- 
  here::here("04-output", "01-graphs", "01-exploration")

path_graphs_analyse_folder <- 
  here::here("04-output", "01-graphs", "02-analyse")

# Chemins d'accès aux sous-dossiers pour le stockage des tables
path_tables_exploration_folder <- 
  here::here("04-output", "02-tables", "01-exploration")

path_tables_analyse_folder <- 
  here::here("04-output", "02-tables", "02-analyse")


# Création des dossiers ---------------------------------------------------
# Créer le dossier pour les données brutes
dir.create(path_raw_data_folder, recursive = TRUE, showWarnings = FALSE)

# Créer le dossier pour les dataframes créés pour l'exploration des données
dir.create(path_df_exploration_folder, recursive = TRUE, showWarnings = FALSE)

# Créer le dossier pour les dataframes créés pour l'analyse
dir.create(path_df_analyse_folder, recursive = TRUE, showWarnings = FALSE)

# Création des dossiers pour les graphiques d'exploration des données
dir.create(path_graphs_exploration_folder, recursive = TRUE, showWarnings = FALSE)

# Création du dossier pour les graphiques de l'analyse
dir.create(path_graphs_analyse_folder, recursive = TRUE, showWarnings = FALSE)

# Création des dossiers pour les tables d'exploration des données
dir.create(path_tables_exploration_folder, recursive = TRUE, showWarnings = FALSE)

# Création du dossier pour les tables de l'analyse
dir.create(path_tables_analyse_folder, recursive = TRUE, showWarnings = FALSE)