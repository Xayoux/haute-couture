#  ------------------------------------------------------------------------
#
# Title : Etude de la compétitivité des produits français de la haute couture
#    By : Romain CAPLIEZ...
#  Date : 2024-03-18
#
#  ------------------------------------------------------------------------

# **************************************************************** --------

# Importer les éléments obligatoires --------------------------------------
source(
  here::here(
    "02-codes", 
    "R-codes", 
    "00-elements-obligatoires.R"
  )
)

# **************************************************************** --------

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

# Supprimer dossier BACI mi-brute si existe déjà
if(dir.exists(path_baci_mi_brute)) unlink(path_baci_mi_brute, recursive = TRUE)

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
    path_output = NULL,
    return_output = TRUE,
    return_pq = TRUE
  ) |> 
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
  group_by(t) |> 
  write_dataset(path_baci_mi_brute)

gc()

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
  year_ref          = 2010,
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
        exporter == "GBR" ~ "Reste de l'UE",
        exporter_name_region == "European Union" ~ "Reste de l'UE",
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
        importer == "GBR" ~ "Reste de l'UE",
        importer_name_region == "European Union" ~ "Reste de l'UE",
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


# Produits hauts de gamme et concurrents ----------------------------------
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

# **************************************************************** --------


# Table LaTeX des produits sélectionnés initialement ----------------------
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
    .by = c(t, sector, k, gamme_fontagne_1997),
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

# Calculer la médiane de la médiane de référence pour chaque secteur
# Permet de regarder l'évolution des "prix" médians au niveau mondial
df_monde <- 
  path_baci_mi_brute |> 
  open_dataset() |> 
  select(t, k, med_ref_t_k, sector) |>
  # Une seule median de référence par année donc on peut prendre les valeurs uniques
  distinct() |> 
  collect() |> 
  arrange(t, k) |> 
  # Calculer la médiane de la médiane de référence
  summarize(
    .by = c(t, sector),
    med_ref_t_k = median(med_ref_t_k, na.rm = TRUE)
  ) 

# Même chose mais pour la France pour comparer les deux
df_fra <- 
  path_baci_mi_brute |> 
  open_dataset() |> 
  filter(exporter == "FRA") |> 
  select(t, k, sector, uv, q) |> 
  collect() |> 
  # Besoin de créer la valeur de référence
  # Même méthodologie que la valeur de référence pour les gammes
  summarize(
    .by = c(t, sector, k),
    median_uv = matrixStats::weightedMedian(uv, q, na.rm = TRUE)
  ) |> 
  # Faire la médiane de ces valeur de référence par secteur
  summarize(
    .by = c(t, sector),
    median_uv = median(median_uv, na.rm = TRUE)
  )


# Regrouper les deux dataframe en un seul pour la représentation graphique
df <- 
  df_monde |> 
  mutate(type = "Monde") |> 
  rename(value = med_ref_t_k) |>
  rbind(
    df_fra |> 
      mutate(type = "France") |> 
      rename(value = median_uv)
  )

# Représenter l'évolution des valeurs unitaires sur un graphique
graph <- 
  ggplot(data = df) +
  geom_line(aes(x = t, y = log(value), color = sector, linetype = type), linewidth = 1) +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  # Arrondir les valeurs des labels à 1 chiffre
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
  scale_color_brewer(palette = "Paired") +
  labs(
    x = "Années",
    y = "logarithme des valeurs unitaires",
    # title = "Valeurs unitaires par secteur"
    title = "",
    color = "",
    linetype = ""
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = 
      element_text(
        angle = 45, 
        hjust = 1,
        color = "black",
        size = 18),
    axis.title.x =
      element_text(
        color = "black",
        size = 22,
        vjust = -0.5
      ),
    axis.text.y =
      element_text(
        color = "black",
        size = 18,
        angle = 90,
        hjust = 0.5
      ),
    axis.title.y =
      element_text(
        color = "black",
        size = 22,
        vjust = 1.5
      ),
    legend.text =
      element_text(
        color = "black",
        size = 18
      ),
    legend.key.spacing.y = unit(0.3, "cm"),
    strip.text.x =
      element_text(
        color = "black",
        size = 18
      )
  ) +
  facet_wrap(~sector, scales = "free_y")

print(graph)

ggsave(
  here(path_graphs_folder, "evolution-ecart-uv-monde-france.png"),
  graph,
  width = 15,
  height = 8
)

remove(df, df_monde, df_fra, graph)

gc()


# Nombre de produits par concurrents 2010 VS 2022 -------------------------
table <- 
  path_baci_mi_brute |>
  open_dataset() |>
  filter(
    t %in% c(2010, 2022),
    k %in% unique(df_products_HG$k)
  ) |> 
  summarize(
    .by = c(t, exporter, k, gamme_fontagne_1997),
    total_v_tikg  = sum(v, na.rm = TRUE)
  ) |> 
  collect() |> 
  mutate(
    .by = c(t, exporter, k),
    share_total_v_gamme_tikg = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
  ) |> 
  filter(
    gamme_fontagne_1997 == "H",
  ) |> 
  mutate(
    .by = c(t, k),
    market_share_HG = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
  ) |> 
  filter(
    exporter != "FRA",
    (share_total_v_gamme_tikg >= 0.75 & market_share_HG >= 0.05) |
      (market_share_HG >= 0.10)
  ) |> 
  summarize(
    .by = c(t, exporter),
    n = n()
  ) |> 
  arrange(t, desc(n)) |> 
  pivot_wider(
    names_from = t, 
    values_from = n
  )  |> 
  filter(`2010` >= 10 | `2022` >= 10) |> 
  xtable() |> 
  print.xtable(
    type = "latex",
    only.contents = TRUE,
    include.colnames = FALSE, 
    include.rownames = FALSE,
    hline.after = NULL
    # file = here(path_tables_folder, "table-nb-product-by-concu.tex")
  )
  
writeLines(
  substr(table, 1, nchar(table)-7), 
  here(path_tables_folder, "table-nb-product-by-concu.tex")
)


# **************************************************************** --------
# Parts de marché des exportateurs ----------------------------------------

# 1) Fichier des parts de marché des exportateurs -------------------------

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
  arrange(desc(t), sector, desc(market_share))

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
  arrange(desc(t), sector, desc(market_share))


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
  arrange(desc(t), sector, desc(market_share)) 

# Garder les noms des pays qui dépassent 5% dans un secteur au moins une fois
country_names <- 
  df_table_market_share_country_exporter |>
  # Garder toutes les observations >= 5% : tout secteur et temps
  filter(market_share >= 5) |> 
  # Garder uniquement les variables secteur et exporter 
  # (peu importe le temps il suffit qu'il soit là une fois)
  select(sector, exporter) |> 
  # Garder les valeurs uniques : si jamais un pays est présent les deux années
  distinct()

# Créer la table LaTeX
table_latex <- 
  df_table_market_share_country_exporter |>
  # Supprimer les variables non voulues
  select(-c(v, q)) |> 
  # Garder uniqueùent les pays passant 5% dans un secteur
  right_join(
    country_names,
    by = c("sector", "exporter")
  ) |> 
  # Mettre les années en colonnes pour limiter le nombre de lignes
  pivot_wider(
    names_from  = t,
    values_from = market_share
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


# 2) Graphiques ---------------------------------------------------------

# Graph des parts de marché des exportateurs (pays/régions) sur chaque secteur
# Sauf Bijouterie
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
  graph_market_share(
    x = "t",
    y = "market_share",
    graph_type = "area",
    var_fill = "exporter_name_region",
    manual_color = couleurs_pays_exporter$general,
    percent = TRUE,
    na.rm = TRUE,
    x_breaks = seq(2010, 2022, 2),
    y_breaks = seq(0, 100, 25),
    x_title = "Années",
    y_title = "Parts de marché",
    type_theme = "bw",
    var_facet = "sector",
    path_output = here(path_graphs_folder, "market-share-hg-exporter-regions-general.png"),
    width = 15,
    height = 8,
    print = TRUE,
    return_output = FALSE
  )

gc()


# Graph des parts de marché des exportateurs (pays/régions) sur chaque secteur
# Seulement la bijouterie
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
  graph_market_share(
    x = "t",
    y = "market_share",
    graph_type = "area",
    var_fill = "exporter_name_region",
    manual_color = couleurs_pays_exporter$bijouterie,
    percent = TRUE,
    na.rm = TRUE,
    x_breaks = seq(2010, 2022, 2),
    y_breaks = seq(0, 100, 25),
    x_title = "Années",
    y_title = "Parts de marché",
    type_theme = "bw",
    var_facet = "sector",
    path_output = here(path_graphs_folder, "market-share-hg-exporter-regions-bijouterie.png"),
    width = 15,
    height = 8,
    print = TRUE,
    return_output = FALSE
  )

gc()


# Parts de marché des importateurs -------------------------------------

# 1) Fichier des parts de marché des importateurs -----------------------

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
  arrange(desc(t), sector, desc(market_share))

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
  arrange(desc(t), sector, desc(market_share))


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
  arrange(desc(t), sector, desc(market_share)) 

# Garder les noms des pays qui dépassent 5% dans un secteur au moins une fois
country_names <- 
  df_table_market_share_country_importer |>
  filter(market_share >= 5) |> 
  select(sector, importer) |> 
  distinct()

# Créer la table LaTeX
table_latex <- 
  df_table_market_share_country_importer |>
  # Supprimer les variables non voulues
  select(-c(v, q)) |> 
  # Garder uniqueùent les pays passant 5% dans un secteur
  right_join(
    country_names,
    by = c("sector", "importer")
  ) |> 
  # Mettre les années en colonnes pour limiter le nombre de lignes
  pivot_wider(
    names_from  = t,
    values_from = market_share
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


# 2) Graphiques ---------------------------------------------------------

# Graph des parts de marché des importateurs (pays/régions) sur chaque secteur
# Sauf la bijouterie
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
  graph_market_share(
    x = "t",
    y = "market_share",
    graph_type = "area",
    var_fill = "importer_name_region",
    manual_color = couleurs_pays_importer$general,
    percent = TRUE,
    na.rm = TRUE,
    x_breaks = seq(2010, 2022, 2),
    y_breaks = seq(0, 100, 25),
    x_title = "Années",
    y_title = "Parts de marché",
    type_theme = "bw",
    var_facet = "sector",
    path_output = here(path_graphs_folder, "market-share-hg-importer-regions-general.png"),
    width = 15,
    height = 8,
    print = TRUE,
    return_output = FALSE
  )

gc()

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
  graph_market_share(
    x = "t",
    y = "market_share",
    graph_type = "area",
    var_fill = "importer_name_region",
    manual_color = couleurs_pays_importer$bijouterie,
    percent = TRUE,
    na.rm = TRUE,
    x_breaks = seq(2010, 2022, 2),
    y_breaks = seq(0, 100, 25),
    x_title = "Années",
    y_title = "Parts de marché",
    type_theme = "bw",
    var_facet = "sector",
    path_output = here(path_graphs_folder, "market-share-hg-importer-regions-bijouterie.png"),
    width = 15,
    height = 8,
    print = TRUE,
    return_output = FALSE
  )

gc()


# **************************************************************** --------
# Demande adressée --------------------------------------------------------

# Calcul de la demande adressée en base 100 comparée avec la France comme pays
# de référence, avec 2010 comme année de référence
df_da <- 
  path_baci_processed |> 
  open_dataset() |>
  adressed_demand(
    year_ref = 2010,
    var_exporter = "exporter_name_region",
    var_k = "sector",
    exporter_ref = "France",
    base_100 = TRUE,
    compare = TRUE,
    return_output = TRUE,
    return_pq = FALSE,
    path_output = NULL
  ) 

# Calcul de la demande adressée en base 100 filtrée pour ne contenir que la 
# France
df_da_france <- 
  path_baci_processed |> 
  open_dataset() |>
  adressed_demand(
    year_ref = 2010,
    var_exporter = "exporter_name_region",
    var_k = "sector",
    exporter_ref = "France",
    base_100 = TRUE,
    compare = FALSE,
    return_output = TRUE,
    return_pq = FALSE,
    path_output = NULL
  ) |> 
  filter(exporter_name_region == "France")


# Représentation graphique de l'évolution de la demande adressée de la France
# par secteur
df_da_france |> 
  graph_lines_comparison(
    x = "t",
    y = "DA_100",
    var_color = "sector",
    palette_color = "Paired",
    x_title = "Année",
    y_title = "Demande adressée en base 100",
    title = "",
    subtitle = "",
    caption = "Source : BACI",
    color_legend = "",
    type_theme = "classic",
    path_output = here(path_graphs_folder, "demande-adressee-france.png"),
    width = 15,
    height = 8,
    print = TRUE,
    return_output = TRUE
  )


# Représentation graphique de l'évolution de la comparaison de la demande 
# adressée des différents pays/régions par rapport à la France
# Graphiques pour tous les secteurs sauf la bijouterie : régions différentes
graph <- 
  df_da |>
  mutate(
    exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter$general)
  ) |>
  filter(sector != "Bijouterie", exporter_name_region != "France") |> 
  graph_lines_comparison(
    x = "t",
    y = "DA_diff",
    linewidth = 0.7,
    var_linetype = "exporter_name_region",
    manual_linetype = linetype_exporter$general,
    var_color = "exporter_name_region",
    manual_color = couleurs_pays_exporter$general,
    x_title = "Année",
    y_title = "Ratio de demande adressée",
    title = "",
    subtitle = "",
    caption = "Source : BACI",
    color_legend = "",
    type_theme = "bw",
    path_output = NULL,
    width = 15,
    height = 8,
    print = FALSE,
    return_output = TRUE
  ) +
  facet_wrap(~sector) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  theme(legend.key.size = unit(1, "cm"))

print(graph)

ggsave(
  here(path_graphs_folder, "demande-adressee-comparaison-with-france-general.png"),
  graph,
  width = 15,
  height = 8
)


# Graphiques pour la bijouterie : régions différentes
graph <-
  df_da |>
  mutate(
    exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter$bijouterie)
  ) |>
  filter(sector == "Bijouterie", exporter_name_region != "France") |> 
  graph_lines_comparison(
    x = "t",
    y = "DA_diff",
    linewidth = 0.7,
    var_linetype = "exporter_name_region",
    manual_linetype = linetype_exporter$bijouterie,
    var_color = "exporter_name_region",
    manual_color = couleurs_pays_exporter$bijouterie,
    x_title = "Année",
    y_title = "Ratio de demande adressée",
    title = "",
    subtitle = "",
    caption = "Source : BACI",
    color_legend = "",
    type_theme = "bw",
    path_output = NULL,
    width = 15,
    height = 8,
    print = FALSE,
    return_output = TRUE,
    var_facet = "sector"
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black")+
  theme(legend.key.size = unit(1, "cm"))

print(graph)

ggsave(
  here(path_graphs_folder, "demande-adressee-comparaison-with-france-bijouterie.png"),
  graph,
  width = 15,
  height = 8
)

remove(df_da, df_da_france, graph)


# Evolution des valeurs unitaires -----------------------------------------

# Valeurs unitaires nominales par secteur
df_uv_nominal <- 
  path_baci_processed |> 
  open_dataset() |> 
  uv_comp(
    year_ref = 2010,
    var_exporter = "exporter_name_region",
    var_k = "sector",
    exporter_ref = "France",
    base_100 = FALSE,
    compare = FALSE,
    return_output = TRUE,
    return_pq = FALSE,
    path_output = NULL
  )

# Valeurs unitaires en base 100 par secteur : comparaison avec France
df_uv_100 <- 
  path_baci_processed |> 
  open_dataset() |> 
  uv_comp(
    year_ref = 2010,
    var_exporter = "exporter_name_region",
    var_k = "sector",
    exporter_ref = "France",
    base_100 = TRUE,
    compare = TRUE,
    return_output = TRUE,
    return_pq = FALSE,
    path_output = NULL
  )

# Evolution des uv par secteur pour la France
df_uv_100_france <- 
  path_baci_processed |> 
  open_dataset() |> 
  uv_comp(
    year_ref = 2010,
    var_exporter = "exporter_name_region",
    var_k = "sector",
    exporter_ref = "France",
    base_100 = TRUE,
    compare = FALSE,
    return_output = TRUE,
    return_pq = FALSE,
    path_output = NULL
  ) |> 
  filter(exporter_name_region == "France")

# Graphique de l'évolution des valeurs unitaires nominales par secteur
df_uv_nominal |> 
  mutate(
    exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter$bijouterie)
  ) |> 
  graph_lines_comparison(
    x = "t",
    y = "uv_mean",
    var_color = "exporter_name_region",
    manual_color = couleurs_pays_exporter$bijouterie,
    var_linetype = "exporter_name_region",
    manual_linetype = linetype_exporter$bijouterie,
    x_title = "Année",
    y_title = "Valeurs unitaires",
    title = "",
    subtitle = "",
    caption = "Source : BACI",
    color_legend = "",
    type_theme = "bw",
    path_output = here(path_graphs_folder, "evolution-uv-nominal.png"),
    width = 15,
    height = 8,
    print = TRUE,
    return_output = TRUE,
    var_facet = "sector"
  )

# Graphique de l'évolution des valeurs unitaires en base 100 par secteur
# Comparaison avec la France
graph <- 
  df_uv_100 |> 
  mutate(
    exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter$bijouterie)
  ) |> 
  graph_lines_comparison(
    x = "t",
    y = "uv_100_diff",
    var_color = "exporter_name_region",
    manual_color = couleurs_pays_exporter$bijouterie,
    var_linetype = "exporter_name_region",
    manual_linetype = linetype_exporter$bijouterie,
    x_title = "Année",
    y_title = "Valeurs unitaires en base 100",
    title = "",
    subtitle = "",
    caption = "Source : BACI",
    color_legend = "",
    type_theme = "bw",
    width = 15,
    height = 8,
    print = FALSE,
    return_output = TRUE,
    var_facet = "sector"
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  theme(legend.key.size = unit(1, "cm"))

print(graph)

ggsave(
  here(path_graphs_folder, "evolution-uv-100-comparison-with-france.png"),
  graph,
  width = 15,
  height = 8
)


# Graphique de l'évolution des valeurs unitaires en base 100 pour la France
df_uv_100_france |> 
  graph_lines_comparison(
    x = "t",
    y = "uv_100",
    var_color = "sector",
    palette_color = "Paired",
    x_title = "Année",
    y_title = "Valeurs unitaires en base 100",
    title = "",
    subtitle = "",
    caption = "Source : BACI",
    color_legend = "",
    type_theme = "classic",
    path_output = here(path_graphs_folder, "evolution-uv-100-france.png"),
    width = 15,
    height = 8,
    print = FALSE,
    return_output = TRUE
  ) 

remove(df_uv_nominal, df_uv_100, df_uv_100_france, graph)
