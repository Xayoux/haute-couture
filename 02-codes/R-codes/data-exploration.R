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


# Exploration des seuils --------------------------------------------------






# Exploration des régions d'exportation -----------------------------------
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
# Importer la fonction exportation_by_sector_regions
source(here("02-codes", "R-codes", "fonctions", "exportations_by_sector_regions.R"))

# Définir la liste des régions étudiée
df_export_regions_chelem |> 
  pull(exporter_name_region) |>
  unique() |>
  # Pour chaque région, appliquer la fonction exportation_by_sector_regions
  # Regarder les parts de marché des pays au sein d'une région
  walk(
    \(region) exportation_by_sector_regions(
      path_baci_processed_parquet = path_baci_processed,
      exporter_region = region,
      year_ref = 2022,
      seuil_market_share = 2,
      path_output = here(path_df_exploration_folder, "regions-exports", "exportations_by_sector_regions.xlsx"),
      wb = wb_export_regions
    )
  )

















