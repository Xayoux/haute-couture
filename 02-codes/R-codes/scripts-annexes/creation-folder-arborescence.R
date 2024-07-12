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

# Chemin d'accès au dossier contenant les données brutes de gravity
path_gravity_folder_origine <- 
  here::here("..", "Gravity")

# Chemin d'accès au dossier contenant les données brutes de gravity au format parquet
path_gravity_parquet_folder <- 
  here::here("..", "Gravity", "Gravity_csv_V202211", "Gravity-parquet")

# Chemin d'accès au dossier contenant les données brutes de gravity au format parquet
# Données avec le GDP mis à jour par la banque mondiale
path_gravity_gdp_maj_parquet_folder <- 
  here::here("..", "Gravity", "Gravity-gdp-maj-parquet")

# Chemin d'accès pour le dossier de stockage des autres données brutes
path_raw_data_folder <- here::here("01-raw-data")

# Chemin d'accès pour le dossier d'exploration des données
path_exploration_folder <- here::here("03-exploration")

# Chemin d'accès pour les données BACI processed (sans outlier, avec gammes et régions)
path_baci_processed <-
  here::here("04-processed-data", "00-BACI")

# Chemin d'accès pour les données BACI mi-brute (sans outliers, avec gamme, sans régions, tous les produits initiaux)
path_baci_mi_brute <-
  here::here("04-processed-data", "00-BACI-mi-brute")

# Chemin d'accès pour les données BACI-total (tous les flux)
path_baci_total <-
  here::here("04-processed-data", "00-BACI-total")

# Chemin d'accès pour les données utilisées pour l'équation de khandelwal
path_gravity_khandelwal <-
  here::here("04-processed-data", "02-Gravity-khandelwal")

# Chemin d'accès pour les donénes de qualité calculées par khandelwal
path_quality_khandelwal <-
  here::here("04-processed-data", "03-quality-khandelwal")

# Chemin d'accès aux sous-dossiers pour le stockage des dataframes
path_df_folder <- 
  here::here("04-processed-data", "01-dataframes")

# Chemin d'accès aux sous-dossiers pour le stockage des graphiques "finaux"
path_graphs_folder <- 
  here::here("05-output", "01-graphs")

# Liste de chemins d'accès aux sous-dossiers des graphiques finaux
list_path_graphs_folder <- 
  list(
    introduction = here::here(path_graphs_folder, "introduction"),
    market_share = here::here(path_graphs_folder, "market-share"),
    direction_exportations = here::here(path_graphs_folder, "direction-exportations"),
    demande_adressee = here::here(path_graphs_folder, "demande-adressee"),
    valeurs_unitaires = here::here(path_graphs_folder, "valeurs-unitaires"),
    quality = here::here(path_graphs_folder, "competitivite-hors-prix"),
    ms_uv_hp = here::here(path_graphs_folder, "ms-uv-hp"),
    share_HG = here::here(path_graphs_folder, "share_HG"),
    marge_extensive = here::here(path_graphs_folder, "marge-extensive"),
    balance_commerciale = here::here(path_graphs_folder, "balance-commerciale"),
    valeur_importations = here::here(path_graphs_folder, "valeur-importations")
  )

# Chemin d'accès aux sous-dossiers pour le stockage des tables "finales"
path_tables_folder <- 
  here::here("05-output", "02-tables")

# Chemin d'accès vers les fonctions R
path_functions_folder <- 
  here::here("02-codes", "R-codes", "fonctions")

# Chemin d'accès vers les fonctions R servant à la création des données
path_functions_create_data_folder <- 
  here::here("02-codes", "R-codes", "fonctions", "fonctions-creation-data")

# Chemin d'accès vers les fonctions R servant à l'exploration des données
path_functions_exploration_folder <- 
  here::here("02-codes", "R-codes", "fonctions", "fonctions-exploration")

# Chemin d'accès au fichier excel contenant toutes les données chiffrées
path_excel_results <-
  here::here("05-output", "results.xlsx")


# Création des dossiers ---------------------------------------------------
# Créer le dossier pour les données brutes
dir.create(path_raw_data_folder, recursive = TRUE, showWarnings = FALSE)

# Créer les dossier pour l'exploration des outliers
dir.create(
  here::here(path_exploration_folder, "outliers"), 
  recursive = TRUE, showWarnings = FALSE
)

# Créer les dossier pour l'exploration des produits et concurrents selon les seuils
dir.create(
  here::here(path_exploration_folder, "products-concu"), 
  recursive = TRUE, showWarnings = FALSE
)

# Créer les dossier pour l'exploration des régions pour l'import et l'export
dir.create(
  here::here(path_exploration_folder, "regions-export-import"), 
  recursive = TRUE, showWarnings = FALSE
)

# Créer le dossier pour les dataframes créés
dir.create(path_df_folder, recursive = TRUE, showWarnings = FALSE)

# Création des dossiers pour les graphiques "finaux"
dir.create(path_graphs_folder, recursive = TRUE, showWarnings = FALSE)

# Création des sous-dossiers des graphiques finaux
for (sub_folder in list_path_graphs_folder){
  dir.create(sub_folder, recursive = TRUE, showWarnings = FALSE)
}

remove(sub_folder)

# Création des dossiers pour les tables "finales
dir.create(path_tables_folder, recursive = TRUE, showWarnings = FALSE)
