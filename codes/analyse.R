#  ------------------------------------------------------------------------
#
# Title : Etude de la compétitivité des produits français de la haute couture
#    By : Romain CAPLIEZ...
#  Date : 2024-03-18
#
#  ------------------------------------------------------------------------


# 1 - Importer les librairies utilisées ---------------------------------------

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


# 2 - Créer la liste des produits à utiliser ----------------------------------

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

# 3 - Créer la base de données BACI ------------------------------------------
# dl_baci(
#   dl_folder = here("..", "BACI2"),rm_csv = FALSE
# )


# 3 - Définition des produits de luxe -----------------------------------------
# Exploration des différents seuils 

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


{source(here("codes", "01-exploration-seuils-function.R"))
file_exploration_seuils_function(
  df_gammes_path = here("processed-data", "BACI-gamme"),
  alpha_vector = seuils,
  seuil_2 = 0.75,
  folder_output = here("processed-data"),
  df_product = df_product
)
remove(df_nb_concu_by_seuil, df_nb_product_by_seuil, exploration_seuil_haut_gamme,
       file_exploration_seuils_function, part_produit_total_function)}























