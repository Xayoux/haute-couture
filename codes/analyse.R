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
dl_baci(
  dl_folder = here("..", "BACI2"),
)

# Définition des produits de luxe -----------------------------------------

# Gammes des marchés : méthode de Fontagné et al (1997)

gamme_ijkt_fontagne_1997(
  path_baci_parquet = here("..", "BACI2", "BACI-parquet"),
  alpha = 1,
  years = 2010:2022,
  codes = unique(df_product$HS92),
  return_output = FALSE,
  path_output = here("processed-data", "BACI-gamme")
) 

products_luxes_fr <- 
  here("processed-data", "BACI-gamme") |> 
  open_dataset() |> 
  summarize(
    .by = c(t, exporter, k, gamme_fontagne_1997),
    total_v_tik = sum(v, na.rm = TRUE)
  ) |> 
  arrange(t, exporter, k, gamme_fontagne_1997) |> 
  collect() |> 
  mutate(
    .by = c(t, exporter, k),
    share_total_v_gamme = total_v_tik / sum(total_v_tik)
  ) |> 
  filter(
    t == 2010,
    exporter == "FRA",
    gamme_fontagne_1997 == "H",
    share_total_v_gamme >= 0.5
  ) |> 
  select(k, share_total_v_gamme)
  
nb_concu <- 
  here("processed-data", "BACI-gamme") |> 
  open_dataset() |> 
  summarize(
    .by = c(t, exporter, k, gamme_fontagne_1997),
    total_v_tik = sum(v, na.rm = TRUE)
  ) |> 
  arrange(t, exporter, k, gamme_fontagne_1997) |> 
  collect() |> 
  mutate(
    .by = c(t, exporter, k),
    share_total_v_gamme = total_v_tik / sum(total_v_tik)
  ) |>
  filter(
    t == 2010,
    share_total_v_gamme >= 0.5,
    k %in% unique(products_luxes_fr$k),
    gamme_fontagne_1997 == "H"
  ) |> 
  group_by(k) |> 
  count()
  
products_luxes_fr |> 
  left_join(
    nb_concu,
    by = "k"
  ) 

