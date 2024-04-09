#  ------------------------------------------------------------------------
#
# Title :  Création de l'arborescence des dossiers pour le projet
#    By : 
#  Date : 2024-04-09
#
#  ------------------------------------------------------------------------


# Importation des librairies utilisées ------------------------------------
library(here)


# Création des dossiers ---------------------------------------------------
# Créer le dossier pour les données brutes
dir.create(
  here("01-raw-data"),
  recursive = TRUE)

# Créer le dossier pour les dataframes créés pour l'analyse
dir.create(
  here("03-processed-data", "01-dataframes"),
  recursive = TRUE)

# Création des dossiers pour les graphiques de tests/observations
dir.create(
  here("04-output","01-graphs","01-tests"),
  recursive = TRUE)

# Création du dossier pour les graphiques finaux (à mettre dans le rapport)
dir.create(
  here("04-output","01-graphs","02-finals"),
  recursive = TRUE)

# Création des dossiers pour les tables de tests/observations
dir.create(
  here("04-output","02-tables","01-tests"),
  recursive = TRUE)

# Création du dossier pour les tables finales (à mettre dans le rapport)
dir.create(
  here("04-output","02-tables","02-finals"),
  recursive = TRUE)