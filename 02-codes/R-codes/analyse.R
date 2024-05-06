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
if(!require(scales)) install.packages("scales")

options(scipen = 999)


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
#   dl_folder = path_baci_folder_origine, rm_csv = TRUE
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

# Exploration des != seuils sur le nb de produits et de concu ---------------
# Importer fonction pour faire l'exploration des seuils
source(here("02-codes", "R-codes", "01-exploration-seuils-function.R"))

# Définition de plusieurs seuils différents : méthode fontagné 1997
seuils_haut_gamme <- c(1.15, 1.25, 1.5, 1.75, 2, 2.5, 2.75, 3)

# Outliers définis comme 1% et 99%
exploration_haut_gamme_func(
  baci = path_baci_folder_parquet_origine,
  ponderate = "q",
  years = 2010,
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
  years = 2010,
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
  years = 2010,
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
  years = 2010,
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
  years = 2010,
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
  years = 2010,
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
  exploration_haut_gamme_func, seuils_haut_gamme
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


# Création de la base BACI utilisée ---------------------------------------
source(here("02-codes", "R-codes", "02-create-baci-processed.R"))

# Crée la base BACI sans les outliers et avec uniquement les gammes H
# Crée un fichier excel contenant les produits et concurrents sélectionnés
create_baci_processed(
  baci = path_baci_folder_parquet_origine,
  ponderate = "q",
  years = 2010:2022,
  codes = unique(df_product$HS92),
  method_outliers = 'sd',
  seuil_H_outliers = 3,
  seuil_L_outliers = 3,
  year_ref = 2022,
  alpha_H_gamme = 3,
  seuil_2_HG = 0.5,
  path_output = path_baci_processed,
  return_output = TRUE,
  return_pq = TRUE,
  remove = TRUE,
  name_xlsx_k_concu = "02-list_k_concu.xlsx"
) |> 
  # Création des secteurs utilisés pour l'analyse
  mutate(
    sector = substr(k, 1, 2),
    sector = 
      dplyr::case_when(
        sector %in% c("61", "62", "65") ~ "Habillement",
        sector == "42" ~ "Maroquinerie",
        sector == "64" ~ "Chaussures",
        sector == "71" ~ "Bijouterie"
      ) 
  ) |> 
  # Ecrire la base de données
  group_by(t) |> 
  write_dataset(path_baci_processed)

remove(create_baci_processed)
gc()





# Parts de marché de chaque exportateur -----------------------------------
# Importer la liste des produits HG sélectionnés pour la France
df_products_HG <- 
  here(path_df_analyse_folder, "02-list_k_concu.xlsx") |>
  read_xlsx(sheet = "product_HG_france")

# Importer la liste des concurrents sélectionnés sur chaque secteur
df_concurrents_HG <- 
  here(path_df_analyse_folder, "02-list_k_concu.xlsx") |>
  read_xlsx(sheet = "sector_concurrents") |> 
  select(exporter, sector) |> 
  distinct() 

# A MODIFIER CORRECTEMENT POUR RENDRE CLEAN

# Définir l'ordre des pays dans le graphique
ordre_pays <- c("Reste du monde", "Amérique","Moyen-Orient", 
                "Turquie",  "Reste de l'Asie" , "Japon, Corée, Hong Kong", "Chine",   
                "Suisse", "Reste de Union européenne", "Italie", "France")

# Définir la couleur des pays dans le graphique
couleurs_pays <- c("France" = "#006CA5",
                   "Italie" = "#04BADE",
                   "Reste de Union européenne" = "#48CAE4",
                   "Suisse" = "#90E0EF",
                   "Chine" = "#ae4d4d",
                   "Japon, Corée, Hong Kong" = "#F46D75",
                   "Reste de l'Asie" = "#F7B4BB",
                   "Turquie" = "#008270",
                   "Moyen-Orient" = "#3AB0AA",
                   "Amérique" = "#d499ed",
                   "Reste du monde" = "#D9D9D9"
)
  
# Créer le graph représentant les exportateurs (pays/régions) sur chaque secteur
graph <- 
  path_baci_processed |> 
  open_dataset() |> 
  collect() |> 
  # Définir les pays et régions concurrents (provient de l'analyse exploratoire des régions pour l'export)
  mutate(
    exporter_name_region = 
      case_when(
        exporter == "FRA" ~ "France",
        exporter == "ITA" ~ "Italie",
        exporter == "CHN" ~ "Chine",
        exporter == "CHE" ~ "Suisse",
        exporter == "TUR" ~ "Turquie",
        exporter_name_region == "North America" ~ "Amérique",
        exporter_name_region == "South America, Central America and Caribbean" ~ "Amérique",
        exporter == "GBR" ~ "Reste de Union européenne",
        exporter_name_region == "European Union" ~ "Reste de Union européenne",
        exporter %in% c("HKG", "JPN", "KOR") ~ "Japon, Corée, Hong Kong",
        exporter_name_region %in% c("South-East Asia", "South Asia and Pacific") ~ "Reste de l'Asie",
        exporter_name_region == "Near and Middle East" ~ "Moyen-Orient",
        .default = "Reste du monde"
      ),
    # Appliquer l'ordre d'apprition aux régions
    exporter_name_region = factor(exporter_name_region, levels = ordre_pays)
  ) |> 
  arrow_table() |> 
  # Calculer les parts de marché sur ces nouvelles régions par secteur
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
  # Créer le graphique
  ggplot(aes(x = t, y = market_share_t_k_i, fill = exporter_name_region)) +
  geom_area() +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  # scale_fill_brewer(palette = "Paired") +
  scale_fill_manual(values = couleurs_pays) +
  labs(
    x = "Année",
    y = "Part de marché",
    title = "Exportations haut de gamme",
    fill = ""
  ) +
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(colour = "black", fill = "#D9D9D9"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~sector, scales = "free_y")

print(graph)

# Sauvegarder le graphique
ggsave(
  here(path_graphs_exploration_folder, "market_share-hg-exporter-regions-good.png"), 
  graph, width = 15, height = 8
)

