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

# Créer la base de données BACI ------------------------------------------

# Extraire les codes retenus pour la nomenclature HS92
codes_hs <- unique(df_product$HS92)

# Créer la base de données BACI de 2010 à 2022, en ajoutant les codes iso3 et en calculant les valeurs unitaires
create_baci_db(
  folder_baci = here("..", "BACI", "BACI_HS92_V202401"),
  year_start = 2010,
  year_end = 2022,
  hs_codes = codes_hs,
  calc_uv = TRUE,
  path_output = here("processed-data", "baci_db.csv"),
  return_output = FALSE
)
