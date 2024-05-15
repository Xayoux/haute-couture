#  ------------------------------------------------------------------------
#
# Title : Etude de la compétitivité des produits français de la haute couture
#    By : Romain CAPLIEZ...
#  Date : 2024-03-18
#
#  ------------------------------------------------------------------------

# **************************************************************** --------

# Préparation de l'analyse --------------------------------------------

# Tester si devtools est installé. Si ce n'est pas le cas, l'installer
# Permet d'installer le package concordance de github pour pouvoir effectuer 
# Des correspondances avec la dernière version de la nomenclature.
# Si concordance est déjà installé à partir de CRAN, le désinstaller, fermer R, puis réinstaller avec la commande ci-dessous
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
if(!require(xtable)) install.packages("xtable")
if(!require(janitor)) install.packages("janitor")

options(scipen = 999)


# Créer l'arborescence des dossiers utilisés pour les différents outputs
# Importer les variables pour les chemins d'accès
source(
  here(
    "02-codes", 
    "R-codes", 
    "00-creation-arborescence-folder.R"
  )
)
 

# Créer la liste des produits à utiliser ------------------------------------

# Vecteur contenant les numéros des chapitres / des sous-sections de la nomenclature pour les produits voulus
chapter_codes <- c(4202, 4203, 61, 62, 64, 6504, 6505, 6506, 7113, 7114, 7116, 7117)

# Créer un dataframe contenant les correspondances entre les codes produits de la nomenclature HS22 et HS92
df_product <- 
  extract_product(
    codes_vector         = chapter_codes,
    path_output          = here(path_df_folder, "01-codes-produits.xlsx"),
    revision_origin      = "HS22",
    revision_destination = "HS92",
    export               = TRUE,
    return_df            = TRUE,
    correspondance       = TRUE
  )

remove(chapter_codes)

# Télécharger la base de données BACI -------------------------------------
# dl_baci(
#   dl_folder = path_baci_folder_origine, rm_csv = TRUE
# )

# Création de la base BACI mi-brute ---------------------------------------
analyse.competitivite::clean_uv_outliers(
  baci = path_baci_folder_parquet_origine,
  years = 2010:2022,
  codes = unique(df_product$HS92),
  method = "sd",
  seuil_H = 3,
  seuil_L = 3,
  path_output = NULL,
  return_output = TRUE,
  return_pq = TRUE
) |> 
  # Calcul des gammes
  analyse.competitivite::gamme_ijkt_fontagne_1997(
    ponderate = "q",
    alpha_H = 3,
    pivot = "longer",
    path_output = path_baci_mi_brute,
    return_output = FALSE,
    return_pq = FALSE
  )

# Création de la base BACI utilisée et les documents associés -------------
source(
  here(
    path_functions_folder,
    "create_baci_processed.R"
  )
)


# Crée la base BACI sans les outliers et avec uniquement les gammes H
# Crée un fichier excel contenant les produits et concurrents sélectionnés
create_baci_processed(
  baci              = path_baci_folder_parquet_origine,
  ponderate         = "q",
  years             = 2010:2022,
  codes             = unique(df_product$HS92),
  method_outliers   = 'sd',
  seuil_H_outliers  = 3,
  seuil_L_outliers  = 3,
  year_ref          = 2022,
  alpha_H_gamme     = 3,
  seuil_2_HG        = 0.75,
  path_list_k_concu = here(path_df_folder, "02-list_k_concu.xlsx"),
  path_output       = path_baci_processed,
  return_output     = TRUE,
  return_pq         = TRUE,
  remove            = TRUE
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
      ),
    exporter_name_region = 
      case_when(
        # Catégories pour la Bijouterie
        exporter == "TUR" & sector == "Bijouterie" ~ "Turquie",
        exporter == "USA" & sector == "Bijouterie" ~ "USA",
        exporter_name_region %in%
          c("South America, Central America and Caribbean", "North America") & 
          sector == "Bijouterie" ~ "Reste du monde",
        
        # Catégories générales
        exporter == "FRA" ~ "France",
        exporter == "ITA" ~ "Italie",
        exporter == "GBR" ~ "Reste de Union européenne",
        exporter_name_region == "European Union" ~ "Reste de Union européenne",
        exporter == "CHE" ~ "Suisse",
        exporter %in% c("CHN", "HKG") ~ "Chine et Hong Kong",
        exporter_name_region %in% 
          c("South-East Asia", "South Asia and Pacific", "North-East Asia") & 
          !exporter %in% c("CHN", "HKG") ~ "Reste de l'Asie",
        exporter_name_region == "Near and Middle East" ~ "Moyen-Orient",
        exporter_name_region %in%
          c("South America, Central America and Caribbean", "North America") ~ "Amérique",
        
        # Par défaut dans Reste du monde
        .default = "Reste du monde"
      ),
    importer_name_region =
      case_when(
        # Catégories générales
        importer == "FRA" ~ "France",
        importer == "ITA" ~ "Italie",
        importer == "GBR" ~ "Reste de Union européenne",
        importer_name_region == "European Union" ~ "Reste de Union européenne",
        importer == "CHE" ~ "Suisse",
        importer %in% c("CHN", "HKG") ~ "Chine et Hong Kong",
        importer %in% c("JPN", "KOR") ~ "Japon et Corée",
        importer_name_region %in% 
          c("South-East Asia", "South Asia and Pacific", "North-East Asia") ~ "Reste de l'Asie",
        importer == "ARE" ~ "ARE",
        importer_name_region == "Near and Middle East" ~ "Moyen-Orient",
        importer == "USA" ~ "USA",
        importer_name_region %in% 
          c("South America, Central America and Caribbean", "North America") ~ "Amérique",
        
        # Par défaut : reste du monde
        .default = "Reste du monde"
        
      )
  ) |> 
  # Ecrire la base de données
  group_by(t) |> 
  write_dataset(path_baci_processed)

remove(create_baci_processed)
gc()


# **************************************************************** --------


# Table LaTeX des produits sélectionnés -----------------------------------
table  <-
  df_product |> 
  # Garder que les codes : à voir comment faire pour les noms
  select(HS22, HS92) |> 
  # Faire 3 groupes de colones pour réduire la taille de la table
  mutate(
    # asocier chaque code à un groupe
    n = row_number(),
    group = 
      case_when(
        n <= max(n) / 3 ~ 1,
        n >= max(n) / 3 * 2 ~ 3,
        .default = 2
      )
  ) |> 
  # Ajouter des lignes vides dans les deux 1er groupes pour permettre le binding
  add_row(HS22 = NA, HS92 = NA, group = 1) |>
  add_row(HS22 = NA, HS92 = NA, group = 2) |>
  select(-n) |> 
  # Séparer chaque groupe dans un df différent dans une liste
  group_nest(group) |> 
  # Sortir la liste de dataframes
  pull(data) |> 
  # Bind les df au niveau des colones
  list_cbind() |> 
  clean_names() |>
  # Transformer le df en table LaTeX (attention format longtable)
  xtable() |> 
  print.xtable(
    type             = "latex",
    # Enlever les noms des lignes et colonnes
    include.rownames = FALSE,
    include.colnames = FALSE,
    # Garder uniquement les valeurs
    only.contents    = TRUE,
    # Supprimer les lignes horizontales
    hline.after      = NULL,
    tabular.environment = "longtable",
    floating = FALSE
  )

# Supprimer les deux derniers \\
writeLines(
  substr(table, 1, nchar(table)-7), 
  here(path_tables_folder, "table-products-init.tex")
)

remove(table)

gc()

# Nombre de produits sélectionné selon l'année de référence ---------------
df_nb_product_by_year_ref <- 
  path_baci_mi_brute |>
  open_dataset() |> 
  # Garder uniquement les flux français de l'année de référence
  dplyr::filter(
    exporter == "FRA"
  ) |>
  # Calculer la somme des flux de chaque produit pour chaque gamme
  dplyr::summarize(
    .by = c(t, k, gamme_fontagne_1997),
    total_v_tikg = sum(v, na.rm = TRUE)
  ) |>
  dplyr::collect() |>
  # Calculer la part que représente chaque gamme par produit
  dplyr::mutate(
    .by = c(t, k),
    share_total_v_gamme_tikg = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
  ) |>
  # Garder uniquement les produits dont la gamme H est supérieure au seuil
  dplyr::filter(
    gamme_fontagne_1997 == "H",
    share_total_v_gamme_tikg >= 0.75
  ) |>
  # Renvoyer un vecteur avec les codes produits uniquement
  dplyr::arrange(t, k) |> 
  dplyr::mutate(
    sector = substr(k, 1, 2),
    sector = 
      dplyr::case_when(
        sector %in% c("61", "62", "65") ~ "Habillement",
        sector == "42" ~ "Maroquinerie",
        sector == "64" ~ "Chaussures",
        sector == "71" ~ "Bijouterie"
      )
  ) |> 
  summarize(
    .by = c(t, sector),
    n = n()
  )

product_HG_france_total <- 
  df_nb_product_by_year_ref |>
  summarize(
    .by = t,
    n = sum(n)
  ) |> 
  mutate(sector = "Total") |> 
  rbind(df_nb_product_by_year_ref)

graph <- 
  product_HG_france_total |> 
  ggplot(
    aes(
      x = t,
      y = n,
      color = sector
    )
  ) +
  geom_line(linewidth = 1) +
  labs(
    # title = "Nombre de produits français dans le haut de gamme par année et secteur",
    title = "",
    x = "Année",
    y = "Nombre de produits",
    color = ""
  ) +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text.x = 
      element_text(
        angle = 45,
        color = "black",
        size = 18,
        hjust = 1
      ),
    axis.title.x = 
      element_text(
        color = "black",
        size = 22,
        vjust = -0.5
        
      ),
    axis.text.y = 
      element_text(
        color = "black",
        size = 18
      ),
    axis.title.y =
      element_text(
        color = "black",
        size = 22
      ),
    legend.text = 
      element_text(
        color = "black",
        size = 18
      )
  )

print(graph)

ggsave(
  here(
    path_graphs_folder, 
    "nb-product-by-year-ref.png"
  ), 
  graph, width = 15, height = 8
)

remove(graph, df_nb_product_by_year_ref, product_HG_france_total)

gc()


# Evolution des valeurs unitaires mondiales et françaises -----------------



# **************************************************************** --------

# Parts de marché -----------------------------------------------------

# a) Préparation des données ----------------------------------------------

# Importer la liste des produits HG sélectionnés pour la France
df_products_HG <- 
  here(path_df_folder, "02-list_k_concu.xlsx") |>
  read_xlsx(sheet = "product_HG_france")

# Importer la liste des concurrents sélectionnés sur chaque secteur
df_concurrents_HG <- 
  here(path_df_folder, "02-list_k_concu.xlsx") |>
  read_xlsx(sheet = "sector_concurrents") |> 
  select(exporter, sector) |> 
  distinct() 


# Définir l'ordre des pays exportateurs dans le graphique
ordre_pays_exporter <- 
  list(
    general    = c("Reste du monde", "Amérique","Moyen-Orient",
                   "Reste de l'Asie", "Chine et Hong Kong",   
                   "Suisse", "Reste de Union européenne", "Italie", "France"),
    
    bijouterie = c("Reste du monde", "Amérique", "USA", "Moyen-Orient",
                    "Turquie", "Reste de l'Asie", "Chine et Hong Kong",   
                    "Suisse", "Reste de Union européenne", "Italie", "France")
  )

# Définir la couleur des pays exportateurs dans le graphique
couleurs_pays_exporter <- 
  list(
    general = 
      c(
        "France"                    = "#006CA5",
        "Italie"                    = "#04B2DE",
        "Reste de Union européenne" = "#48CAE4",
        "Suisse"                    = "#90E0EF",
        "Chine et Hong Kong"        = "#ae4d4d",
        "Reste de l'Asie"           = "#F7B4BB",
        "Moyen-Orient"              = "#3AB0AA",
        "Amérique"                  = "#d499ed",
        "Reste du monde"            = "#D9D9D9"
      ),
    
    bijouterie = 
      c(
        "France"                    = "#006CA5",
        "Italie"                    = "#04B2DE",
        "Reste de Union européenne" = "#48CAE4",
        "Suisse"                    = "#90E0EF",
        "Chine et Hong Kong"        = "#ae4d4d",
        "Reste de l'Asie"           = "#F7B4BB",
        "Turquie"                   = "#008270",
        "Moyen-Orient"              = "#3AB0AA",
        "USA"                       = "#7600bc",
        "Amérique"                  = "#d499ed",
        "Reste du monde"            = "#D9D9D9"
      )
  )


# Définir l'ordre des pays importateurs dans le graphique
ordre_pays_importer <- 
  list(
    general = 
      c("Reste du monde", "Amérique", "USA", "Moyen-Orient", "ARE",
        "Reste de l'Asie" , "Japon et Corée", "Chine et Hong Kong", 
        "Suisse", "Reste de Union européenne", "Italie", "France"),
    
    bijouterie = 
      c("Reste du monde", "Amérique", "USA", "Moyen-Orient", "ARE",
        "Reste de l'Asie" , "Japon et Corée", "Chine et Hong Kong", 
        "Suisse", "Reste de Union européenne", "Italie", "France")
  )

# Définir la couleur des pays exportateurs  dans le graphique
couleurs_pays_importer <-
  list(
    general = 
      c(
        "France"                    = "#006CA5",
        "Italie"                    = "#04B2DE",
        "Reste de Union européenne" = "#48CAE4",
        "Suisse"                    = "#90E0EF",
        "Chine et Hong Kong"        = "#ae4d4d",
        "Japon et Corée"            = "#F46D75",
        "Reste de l'Asie"           = "#F7B4BB",
        "ARE"                       = "#008259",
        "Moyen-Orient"              = "#3AB0AA",
        "USA"                       = "#7600bc",
        "Amérique"                  = "#d499ed",
        "Reste du monde"            = "#D9D9D9"
      ),
    
    bijouterie = 
      c(
        "France"                    = "#006CA5",
        "Italie"                    = "#04B2DE",
        "Reste de Union européenne" = "#48CAE4",
        "Suisse"                    = "#90E0EF",
        "Chine et Hong Kong"        = "#ae4d4d",
        "Japon et Corée"            = "#F46D75",
        "Reste de l'Asie"           = "#F7B4BB",
        "ARE"                       = "#008259",
        "Moyen-Orient"              = "#3AB0AA",
        "USA"                       = "#7600bc",
        "Amérique"                  = "#d499ed",
        "Reste du monde"            = "#D9D9D9"
      )
  )
  

# b) Parts de marché des exportateurs -------------------------------------

# b-1) Fichier des parts de marché des exportateurs -----------------------

# Table des parts de marché par pays et secteurs exportateurs
df_market_share_country_exporter <- 
  path_baci_processed |> 
  market_share(
    summarize_k   = "sector",
    summarize_v   = "exporter",
    by            = NULL,
    seuil         = 5,
    years         = 2010:2022,
    codes         = unique(df_products_HG$k),
    path_output   = NULL,
    return_output = TRUE,
    return_pq     = FALSE
  ) |> 
  arrange(desc(t), sector, desc(market_share_t_k_i))

# Table des parts de marché par région/pays exportateurs
df_market_share_country_region_exporter <- 
  path_baci_processed |> 
  market_share(
    summarize_k   = "sector",
    summarize_v   = "exporter_name_region",
    by            = NULL,
    seuil         = 5,
    years         = 2010:2022,
    codes         = unique(df_products_HG$k),
    path_output   = NULL,
    return_output = TRUE,
    return_pq     = FALSE
  ) |> 
  arrange(desc(t), sector, desc(market_share_t_k_i))


# Enregistrer les deux tables dans un fichier excel
writexl::write_xlsx(
  list(
    "Pays" = df_market_share_country_exporter,
    "Pays-Régions" = df_market_share_country_region_exporter
  ),
  here(path_df_folder, "03-market-share-exporter.xlsx")
)


# Table Latex des parts de marché des pays en 2010 et 2022 >= 5%
# Calculer les parts de marche des pays exportateurs par secteur pour 2010, 2022
df_table_market_share_country_exporter <- 
  path_baci_processed |> 
  market_share(
    summarize_k   = "sector",
    summarize_v   = "exporter",
    by            = NULL,
    seuil         = 0,
    years         = c(2010,2022),
    codes         = unique(df_products_HG$k),
    path_output   = NULL,
    return_output = TRUE,
    return_pq     = FALSE
  ) |> 
  arrange(desc(t), sector, desc(market_share_t_k_i)) 

# Garder les noms des pays qui dépassent 5% dans un secteur au moins une fois
country_names <- 
  df_table_market_share_country_exporter |>
  filter(market_share_t_k_i >= 5) |> 
  select(sector, exporter) |> 
  distinct()

# Créer la table LaTeX
table_latex <- 
  df_table_market_share_country_exporter |>
  # Supprimer les variables non voulues
  select(-c(v_t_k_i, q_t_k_i)) |> 
  # Garder uniqueùent les pays passant 5% dans un secteur
  right_join(
    country_names,
    by = c("sector", "exporter")
  ) |> 
  # Mettre les années en colonnes pour limiter le nombre de lignes
  pivot_wider(
    names_from  = t,
    values_from = market_share_t_k_i
  ) |> 
  # Renommer les colonnes des années
  rename(
    "year_2010" = `2010`,
    "year_2022" = `2022`
  ) |> 
  # Changer l'ordre d'apparition des colonnes
  relocate(sector, exporter, year_2010, year_2022) |> 
  # Passer le table en format latex
  xtable() |> 
  print.xtable(
    type             = "latex",
    # Enlever les noms des lignes et colonnes
    include.rownames = FALSE,
    include.colnames = FALSE,
    # Garder uniquement les valeurs
    only.contents    = TRUE,
    # Supprimer les lignes horizontales
    hline.after      = NULL
  )

# Ecrire le fichier LaTeX en enlevant les derniers \\ (meilleure présentation)
writeLines(
  # Supprimer les deux derniers \\
  substr(table_latex, 1, nchar(table_latex)-7), 
  # Chemin d'exportation
  here(path_tables_folder, "table-market-share-country-exporter.tex")
)

remove(df_market_share_country_exporter, df_market_share_country_region_exporter, 
       df_table_market_share_country_exporter, table_latex, country_names)

gc()


# b-2) Graphiques ---------------------------------------------------------

# Graph des parts de marché des exportateurs (pays/régions) sur chaque secteur
# Sauf Bijouterie
graph <- 
  path_baci_processed |> 
  open_dataset() |> 
  collect() |> 
  # Définir les pays et régions concurrents (provient de l'analyse exploratoire des régions pour l'export)
  mutate(
    # Appliquer l'ordre d'apprition aux régions
    exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter[["general"]])
  ) |> 
  arrow_table() |> 
  # Calculer les parts de marché sur ces nouvelles régions par secteur
  market_share(
    summarize_k   = "sector",
    summarize_v   = "exporter_name_region",
    by            = NULL,
    seuil         = 0,
    years         = 2010:2022,
    codes         = unique(df_products_HG$k),
    path_output   = NULL,
    return_output = TRUE,
    return_pq     = FALSE
  ) |>  
  filter(sector != "Bijouterie") |> 
  # Créer le graphique
  ggplot(aes(x = t, y = market_share_t_k_i, fill = exporter_name_region)) +
  geom_area() +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  # scale_fill_brewer(palette = "Paired") +
  scale_fill_manual(values = couleurs_pays_exporter[["general"]]) +
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
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "right"
  ) +
  facet_wrap(~sector, scales = "free_y") 

print(graph)

# Sauvegarder le graphique
ggsave(
  here(
    path_graphs_folder, 
    "market-share-hg-exporter-regions-general.png"
  ), 
  graph, width = 15, height = 8
)


# Graph des parts de marché des exportateurs (pays/régions) sur chaque secteur
# Seulement la bijouterie
graph <- 
  path_baci_processed |> 
  open_dataset() |> 
  collect() |> 
  # Définir les pays et régions concurrents (provient de l'analyse exploratoire des régions pour l'export)
  mutate(
    # Appliquer l'ordre d'apprition aux régions
    exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter[["bijouterie"]])
  ) |> 
  arrow_table() |> 
  # Calculer les parts de marché sur ces nouvelles régions par secteur
  market_share(
    summarize_k   = "sector",
    summarize_v   = "exporter_name_region",
    by            = NULL,
    seuil         = 0,
    years         = 2010:2022,
    codes         = unique(df_products_HG$k),
    path_output   = NULL,
    return_output = TRUE,
    return_pq     = FALSE
  ) |>  
  filter(sector == "Bijouterie") |> 
  # Créer le graphique
  ggplot(aes(x = t, y = market_share_t_k_i, fill = exporter_name_region)) +
  geom_area() +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  # scale_fill_brewer(palette = "Paired") +
  scale_fill_manual(values = couleurs_pays_exporter[["bijouterie"]]) +
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
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "right"
  ) +
  facet_wrap(~sector, scales = "free_y") 

print(graph)

# Sauvegarder le graphique
ggsave(
  here(
    path_graphs_folder, 
    "market-share-hg-exporter-regions-bijouterie.png"
  ), 
  graph, width = 15, height = 8
)


remove(graph)

gc()



# c) Parts de marché des importateurs -------------------------------------

# c-1) Fichier des parts de marché des importateurs -----------------------

# Table des parts de marché par pays et secteurs importateurs
df_market_share_country_importer <- 
  path_baci_processed |> 
  market_share(
    summarize_k   = "sector",
    summarize_v   = "importer",
    by            = NULL,
    seuil         = 5,
    years         = 2010:2022,
    codes         = unique(df_products_HG$k),
    path_output   = NULL,
    return_output = TRUE,
    return_pq     = FALSE
  ) |> 
  arrange(desc(t), sector, desc(market_share_t_k_i))

# Table des parts de marché par région/pays importateurs
df_market_share_country_region_importer <- 
  path_baci_processed |> 
  market_share(
    summarize_k   = "sector",
    summarize_v   = "importer_name_region",
    by            = NULL,
    seuil         = 5,
    years         = 2010:2022,
    codes         = unique(df_products_HG$k),
    path_output   = NULL,
    return_output = TRUE,
    return_pq     = FALSE
  ) |> 
  arrange(desc(t), sector, desc(market_share_t_k_i))


# Enregistrer les deux tables dans un fichier excel
writexl::write_xlsx(
  list(
    "Pays" = df_market_share_country_importer,
    "Pays-Régions" = df_market_share_country_region_importer
  ),
  here(path_df_folder, "03-market-share-importer.xlsx")
)


# Table Latex des parts de marché des pays en 2010 et 2022 >= 5%
# Calculer les parts de marche des pays importateurs par secteur pour 2010, 2022
df_table_market_share_country_importer <- 
  path_baci_processed |> 
  market_share(
    summarize_k   = "sector",
    summarize_v   = "importer",
    by            = NULL,
    seuil         = 0,
    years         = c(2010,2022),
    codes         = unique(df_products_HG$k),
    path_output   = NULL,
    return_output = TRUE,
    return_pq     = FALSE
  ) |> 
  arrange(desc(t), sector, desc(market_share_t_k_i)) 

# Garder les noms des pays qui dépassent 5% dans un secteur au moins une fois
country_names <- 
  df_table_market_share_country_importer |>
  filter(market_share_t_k_i >= 5) |> 
  select(sector, importer) |> 
  distinct()

# Créer la table LaTeX
table_latex <- 
  df_table_market_share_country_importer |>
  # Supprimer les variables non voulues
  select(-c(v_t_k_i, q_t_k_i)) |> 
  # Garder uniqueùent les pays passant 5% dans un secteur
  right_join(
    country_names,
    by = c("sector", "importer")
  ) |> 
  # Mettre les années en colonnes pour limiter le nombre de lignes
  pivot_wider(
    names_from  = t,
    values_from = market_share_t_k_i
  ) |> 
  # Renommer les colonnes des années
  rename(
    "year_2010" = `2010`,
    "year_2022" = `2022`
  ) |> 
  # Changer l'ordre d'apparition des colonnes
  relocate(sector, importer, year_2010, year_2022) |> 
  # Passer le table en format latex
  xtable() |> 
  print.xtable(
    type             = "latex",
    # Enlever les noms des lignes et colonnes
    include.rownames = FALSE,
    include.colnames = FALSE,
    # Garder uniquement les valeurs
    only.contents    = TRUE,
    # Supprimer les lignes horizontales
    hline.after      = NULL
  )

# Ecrire le fichier LaTeX en enlevant les derniers \\ (meilleure présentation)
writeLines(
  # Supprimer les deux derniers \\
  substr(table_latex, 1, nchar(table_latex)-7), 
  # Chemin d'exportation
  here(path_tables_folder, "table-market-share-country-importer.tex")
)

remove(df_market_share_country_importer, df_market_share_country_region_importer, 
       df_table_market_share_country_importer, table_latex, country_names)

gc()


# c-2) Graphiques ---------------------------------------------------------

# Graph des parts de marché des importateurs (pays/régions) sur chaque secteur
# Sauf la bijouterie
graph <- 
  path_baci_processed |> 
  open_dataset() |> 
  collect() |> 
  # Définir les pays et régions concurrents (provient de l'analyse exploratoire des régions pour l'export)
  mutate(
    # Appliquer l'ordre d'apprition aux régions
    importer_name_region = factor(importer_name_region, levels = ordre_pays_importer[["general"]])
  ) |> 
  arrow_table() |> 
  # Calculer les parts de marché sur ces nouvelles régions par secteur
  market_share(
    summarize_k   = "sector",
    summarize_v   = "importer_name_region",
    by            = NULL,
    seuil         = 0,
    years         = 2010:2022,
    codes         = unique(df_products_HG$k),
    path_output   = NULL,
    return_output = TRUE,
    return_pq     = FALSE
  ) |>  
  filter(sector != "Bijouterie") |>
  # Créer le graphique
  ggplot(aes(x = t, y = market_share_t_k_i, fill = importer_name_region)) +
  geom_area() +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  # scale_fill_brewer(palette = "Paired") +
  scale_fill_manual(values = couleurs_pays_importer$general) +
  labs(
    x = "Année",
    y = "Part de marché",
    title = "Importations haut de gamme",
    fill = ""
  ) +
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(colour = "black", fill = "#D9D9D9"),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~sector, scales = "free_y")

print(graph)

# Sauvegarder le graphique
ggsave(
  here(
    path_graphs_folder, 
    "market-share-hg-importer-regions-general.png"
  ), 
  graph, width = 15, height = 8
)


# Graph des parts de marché des importateurs (pays/régions) sur chaque secteur
# Sauf la bijouterie
graph <- 
  path_baci_processed |> 
  open_dataset() |> 
  collect() |> 
  # Définir les pays et régions concurrents (provient de l'analyse exploratoire des régions pour l'export)
  mutate(
    # Appliquer l'ordre d'apprition aux régions
    importer_name_region = factor(importer_name_region, levels = ordre_pays_importer$bijouterie)
  ) |> 
  arrow_table() |> 
  # Calculer les parts de marché sur ces nouvelles régions par secteur
  market_share(
    summarize_k   = "sector",
    summarize_v   = "importer_name_region",
    by            = NULL,
    seuil         = 0,
    years         = 2010:2022,
    codes         = unique(df_products_HG$k),
    path_output   = NULL,
    return_output = TRUE,
    return_pq     = FALSE
  ) |>  
  filter(sector == "Bijouterie") |>
  # Créer le graphique
  ggplot(aes(x = t, y = market_share_t_k_i, fill = importer_name_region)) +
  geom_area() +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  # scale_fill_brewer(palette = "Paired") +
  scale_fill_manual(values = couleurs_pays_importer$bijouterie) +
  labs(
    x = "Année",
    y = "Part de marché",
    title = "Importations haut de gamme",
    fill = ""
  ) +
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_rect(colour = "black", fill = "#D9D9D9"),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~sector, scales = "free_y")

print(graph)

# Sauvegarder le graphique
ggsave(
  here(
    path_graphs_folder, 
    "market-share-hg-importer-regions-bijouterie.png"
  ), 
  graph, width = 15, height = 8
)

remove(graph)

gc()














