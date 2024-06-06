# Importer les éléments obligatoires --------------------------------------
if(!require(here)) install.packages("here")

source(
  here::here(
    "02-codes", 
    "R-codes", 
    "00-elements-obligatoires.R"
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
# Sans outliers, gammes calculées, secteurs définis, tous les flux

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
  # Définition des secteurs
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
          sector == "Bijouterie" ~ "RDM",
        
        # Catégories générales
        exporter == "FRA" ~ "France",
        exporter == "ITA" ~ "Italie",
        exporter == "GBR" ~ "Reste de l'UE",
        exporter_name_region == "European Union" ~ "Reste de l'UE",
        exporter == "CHE" ~ "Suisse",
        exporter %in% c("CHN", "HKG") ~ "Chine et HK",
        exporter_name_region %in% 
          c("South-East Asia", "South Asia and Pacific", "North-East Asia") & 
          !exporter %in% c("CHN", "HKG") ~ "Reste de l'Asie",
        exporter_name_region == "Near and Middle East" ~ "Moyen-Orient",
        exporter_name_region %in%
          c("South America, Central America and Caribbean", "North America") ~ "Amérique",
        
        # Par défaut dans RDM
        .default = "RDM"
      ),
    importer_name_region =
      case_when(
        # Catégories générales
        importer == "FRA" ~ "France",
        importer == "ITA" ~ "Italie",
        importer == "GBR" ~ "Reste de l'UE",
        importer_name_region == "European Union" ~ "Reste de l'UE",
        importer == "CHE" ~ "Suisse",
        importer %in% c("CHN", "HKG") ~ "Chine et HK",
        importer %in% c("JPN", "KOR") ~ "Japon et Corée",
        importer_name_region %in% 
          c("South-East Asia", "South Asia and Pacific", "North-East Asia") ~ "Reste de l'Asie",
        importer == "ARE" ~ "ARE",
        importer_name_region == "Near and Middle East" ~ "Moyen-Orient",
        importer == "USA" ~ "USA",
        importer_name_region %in% 
          c("South America, Central America and Caribbean", "North America") ~ "Amérique",
        
        # Par défaut : reste du monde
        .default = "RDM"
        
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


# Télécharger la base de données Gravity ----------------------------------
# dl_gravity(dl_folder = here::here("..", "Gravity"), dl_zip = FALSE)



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
    list_path_graphs_folder$introduction,
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
  here(list_path_graphs_folder$introduction, "evolution-ecart-uv-monde-france.png"),
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


# Parts de marché des exportateurs ----------------------------------------
## Données ----------------------------------------------------------------

# Df des parts de marché des pays exportateurs par secteur
df_market_share_country_exporter <- 
  path_baci_processed |> 
  market_share(
    summarize_k   = "sector",
    summarize_v   = "exporter",
    by            = NULL,
    seuil         = 0,
    years         = 2010:2022,
    codes         = unique(df_products_HG$k),
    path_output   = NULL,
    return_output = TRUE,
    return_pq     = FALSE
  ) |> 
  arrange(desc(t), sector, desc(market_share))


# Df des pars de marché des régions exportatrices par secteur
df_market_share_country_region_exporter <- 
  path_baci_processed |> 
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
  arrange(desc(t), sector, desc(market_share))

## Fichier de résultats ---------------------------------------------------
sheet_name <- "Market share exporter"
addWorksheet(wb_results, sheet_name)

# Ecriture des parts de marché des pays exportateurs
writeData(wb_results, sheet_name, "Parts de marché des pays exportateurs",
          rowNames = FALSE, startRow = 1, startCol = 1)

writeData(wb_results, sheet_name, df_market_share_country_exporter,
          rowNames = FALSE, startRow = 2, startCol = 1)


# Ecriture des parts de marché des régions exportatrices
writeData(wb_results, sheet_name, "Parts de marché des régions exportatrices",
          rowNames = FALSE, startRow = 1, 
          startCol = ncol(df_market_share_country_exporter) + 3)

writeData(wb_results, sheet_name, df_market_share_country_region_exporter,
          rowNames = FALSE, startRow = 2, 
          startCol = ncol(df_market_share_country_exporter) + 3)

saveWorkbook(wb_results, path_excel_results, overwrite = TRUE)


## Tables LaTeX -----------------------------------------------------------
# Récupérer les noms des pays qui dépassent 5% dans un secteur au moins une fois
# en 2010 ou 2022
country_names <- 
  df_market_share_country_exporter |> 
  filter(
    market_share >= 5,
    t %in% c(2010, 2022)
  ) |>
  select(sector, exporter) |>
  distinct()


# Créer la table LaTeX
table_latex <- 
  df_market_share_country_exporter |>
  filter(
    t %in% c(2010, 2022)
  ) |>
  # Supprimer les variables non voulues
  select(-c(v, q)) |> 
  # Garder uniquement les pays passant 5% dans un secteur
  right_join(
    country_names,
    by = join_by("sector", "exporter")
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

## Graphiques ---------------------------------------------------------

# Graph des parts de marché des exportateurs (pays/régions) sur chaque secteur
# Sauf Bijouterie
df_market_share_country_region_exporter |> 
  filter(sector != "Bijouterie") |>
  mutate(
    # Appliquer l'ordre d'apprition aux régions
    exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter[["general"]])
  ) |>
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
    x_title = "Années",
    y_title = "Parts de marché",
    type_theme = "bw",
    var_facet = "sector",
    path_output = here(list_path_graphs_folder$market_share,
                       "market-share-hg-exporter-countries-general.png"),
    width = 15,
    height = 8,
    print = TRUE,
    return_output = FALSE
  )


# Graph des parts de marché des exportateurs (pays/régions) : bijouterie
df_market_share_country_region_exporter |> 
  filter(sector == "Bijouterie") |> 
  mutate(
    # Appliquer l'ordre d'apprition aux régions
    exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter[["bijouterie"]])
  ) |> 
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
    x_title = "Années",
    y_title = "Parts de marché",
    type_theme = "bw",
    var_facet = "sector",
    path_output = here(list_path_graphs_folder$market_share,
                       "market-share-hg-exporter-countries-bijouterie.png"),
    width = 15,
    height = 8,
    print = TRUE,
    return_output = FALSE
  )


# Evolution commerce des secteurs -----------------------------------------
# Valeurs commerciales totales par secteur
df_v_sector <- 
  df_market_share_country_region_exporter |> 
  summarize(
    .by = c(t, sector),
    total_v = sum(v, na.rm = TRUE)
  ) |> 
  arrange(desc(t), desc(total_v))


## Fichier de résultats ---------------------------------------------------
# Enregistrer dans le document excel
sheet_name <- "Evolution commerce"
addWorksheet(wb_results, sheet_name)

# Enregistrer les valeurs d'exportations totales de chaque secteur
writeData(wb_results, sheet_name, 
          "Valeurs commerciales totales par secteur", 
          rowNames = FALSE, startRow = 1, startCol = 1)

writeData(wb_results, sheet_name, df_v_sector,
          rowNames = FALSE, startRow = 2, startCol = 1)

saveWorkbook(wb_results, path_excel_results, overwrite = TRUE)


## Graphiques -----------------------------------------------------------
# Graphique de l'évolution du commerce par secteur (sauf bijouterie)
df_market_share_country_region_exporter |>
  filter(sector != "Bijouterie") |>
  mutate(
    exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter$bijouterie)
  ) |>
  graph_market_share(
    x = "t",
    y = "v",
    graph_type = "area",
    var_fill = "exporter_name_region",
    manual_color = couleurs_pays_exporter$general,
    percent = FALSE,
    na.rm = TRUE,
    x_breaks = seq(2010, 2022, 2),
    x_title = "Années",
    y_title = "Parts de marché", 
    title = "",
    type_theme = "bw",
    var_facet = "sector",
    path_output = here(list_path_graphs_folder$market_share,
                       "evolution-market-share-hg-exporter-regions-general.png"),
    width = 15,
    height = 8,
    print = TRUE,
    return_output = FALSE
  )

# Graphique de l'évolution du commerce par secteur (sauf bijouterie)
df_market_share_country_region_exporter |>
  filter(sector == "Bijouterie") |>
  mutate(
    exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter$bijouterie)
  ) |>
  graph_market_share(
    x = "t",
    y = "v",
    graph_type = "area",
    var_fill = "exporter_name_region",
    manual_color = couleurs_pays_exporter$bijouterie,
    percent = FALSE,
    na.rm = TRUE,
    x_breaks = seq(2010, 2022, 2),
    x_title = "Années",
    y_title = "Parts de marché", 
    title = "",
    type_theme = "bw",
    var_facet = "sector",
    path_output = here(list_path_graphs_folder$market_share,
                       "evolution-market-share-hg-exporter-regions-bijouterie.png"),
    width = 15,
    height = 8,
    print = TRUE,
    return_output = FALSE
  )



# Parts de marché des importateurs ----------------------------------------

## Données ----------------------------------------------------------------
# Df des parts de marché des pays exportateurs par secteur
df_market_share_country_importer <- 
  path_baci_processed |> 
  market_share(
    summarize_k   = "sector",
    summarize_v   = "importer",
    by            = NULL,
    seuil         = 0,
    years         = 2010:2022,
    codes         = unique(df_products_HG$k),
    path_output   = NULL,
    return_output = TRUE,
    return_pq     = FALSE
  ) |> 
  arrange(desc(t), sector, desc(market_share))


# Df des pars de marché des régions exportatrices par secteur
df_market_share_country_region_importer <- 
  path_baci_processed |> 
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
  arrange(desc(t), sector, desc(market_share))

## Fichier de résultats ---------------------------------------------------
sheet_name <- "Market share importer"
addWorksheet(wb_results, sheet_name)

# Ecriture des parts de marché des pays importateurs
writeData(wb_results, sheet_name, "Parts de marché des pays importateurs",
          rowNames = FALSE, startRow = 1, startCol = 1)

writeData(wb_results, sheet_name, df_market_share_country_importer,
          rowNames = FALSE, startRow = 2, startCol = 1)

# Ecrire des parts de marché des régions importatrices
writeData(wb_results, sheet_name, "Parts de marché des régions importatrices",
          rowNames = FALSE, startRow = 1, 
          startCol = ncol(df_market_share_country_importer) + 3)

writeData(wb_results, sheet_name, df_market_share_country_region_importer,
          rowNames = FALSE, startRow = 2, 
          startCol = ncol(df_market_share_country_importer) + 3)

saveWorkbook(wb_results, path_excel_results, overwrite = TRUE)


## Tables LaTeX -----------------------------------------------------------
# Récupérer les noms des pays qui dépassent 5% dans un secteur au moins une fois
# en 2010 ou 2022
country_names <- 
  df_market_share_country_importer |> 
  filter(
    market_share >= 5,
    t %in% c(2010, 2022)
  ) |>
  select(sector, importer) |>
  distinct()


# Créer la table LaTeX
table_latex <- 
  df_market_share_country_importer |>
  filter(
    t %in% c(2010, 2022)
  ) |>
  # Supprimer les variables non voulues
  select(-c(v, q)) |> 
  # Garder uniquement les pays passant 5% dans un secteur
  right_join(
    country_names,
    by = join_by("sector", "importer")
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


## Graphiques ---------------------------------------------------------

# Graph des parts de marché des exportateurs (pays/régions) sur chaque secteur
# Sauf Bijouterie
df_market_share_country_region_importer |> 
  filter(sector != "Bijouterie") |>
  mutate(
    # Appliquer l'ordre d'apprition aux régions
    importer_name_region = factor(importer_name_region, levels = ordre_pays_importer$general)
  ) |>
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
    x_title = "Années",
    y_title = "Parts de marché",
    type_theme = "bw",
    var_facet = "sector",
    path_output = here(list_path_graphs_folder$market_share,
                       "market-share-hg-importer-countries-general.png"),
    width = 15,
    height = 8,
    print = TRUE,
    return_output = FALSE
  )


# Graph des parts de marché des exportateurs (pays/régions) : bijouterie
df_market_share_country_region_importer |> 
  filter(sector == "Bijouterie") |> 
  mutate(
    # Appliquer l'ordre d'apprition aux régions
    importer_name_region = factor(importer_name_region, levels = ordre_pays_importer$bijouterie)
  ) |> 
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
    x_title = "Années",
    y_title = "Parts de marché",
    type_theme = "bw",
    var_facet = "sector",
    path_output = here(list_path_graphs_folder$market_share,
                       "market-share-hg-importer-countries-bijouterie.png"),
    width = 15,
    height = 8,
    print = TRUE,
    return_output = FALSE
  )









# Direction des exportations ----------------------------------------------

# Destination des exportations
df_destination_exports <- 
  path_baci_processed |> 
  market_share(
    summarize_k   = "sector",
    summarize_v   = "exporter_name_region",
    by            = "importer_name_region",
    seuil         = 0,
    years         = 2010:2022,
    codes         = unique(df_products_HG$k),
    path_output   = NULL,
    return_output = TRUE,
    return_pq     = FALSE
  ) |> 
  arrange(desc(t), sector, desc(market_share)) |>
  # Garder uniquement le France, l'Italie et la Chine
  filter(
    exporter_name_region %in% c("France", "Italie", "Chine et HK")
  )


# Fonction pour créer les graphiques pour chaque secteur 
market_share_by_exporter <- function(df, secteur){
  df |>
    # Garder uniquement le secteur voulu
    filter(
      sector == secteur
    ) |> 
    # Ordonner les pays/régions pour une meilleure visibilité
    mutate(
      importer_name_region = factor(importer_name_region, levels = ordre_pays_importer$bijouterie)
    ) |>
    # Créer le graphique et l'enregistrer
    graph_market_share(
      x = "t",
      y = "market_share",
      graph_type = "area",
      var_fill = "importer_name_region",
      manual_color = couleurs_pays_importer$bijouterie,
      percent = TRUE,
      na.rm = TRUE,
      x_breaks = seq(2010, 2022, 2),
      x_title = "Années",
      y_title = "Parts de marché", 
      title = "",
      type_theme = "bw",
      var_facet = "exporter_name_region",
      path_output = here(list_path_graphs_folder$direction_exportations,
                         str_glue("market-share-hg-exporter-regions-{secteur}.png")),
      width = 15,
      height = 8,
      print = TRUE,
      return_output = FALSE
    )
}

# Obtenir les différents secteurs 
sector_vector <- 
  path_baci_processed |> 
  open_dataset() |> 
  filter(t == 2022) |> 
  collect() |> 
  pull(sector) |> 
  unique()

# Exécuter la fonction de représentation pour chaque grphique
walk(
  sector_vector, 
  \(sector_vector) market_share_by_exporter(df_destination_exports, sector_vector)
)

remove(df_destination_exports, sector_vector, market_share_by_exporter)


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
    path_output = here(list_path_graphs_folder$demande_adressee,
                       "demande-adressee-france.png"),
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
  filter(
    sector != "Bijouterie", 
    exporter_name_region != "France",
  ) |> 
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
  here(
    list_path_graphs_folder$demande_adressee,
    "demande-adressee-comparaison-with-france-general.png"
  ),
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
  here(
    list_path_graphs_folder$demande_adressee,   
    "demande-adressee-comparaison-with-france-bijouterie.png"),
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
    formula = "median_pond",
    var_pond = "q",
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
    formula = "median_pond",
    var_pond = "q",
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
    formula = "median_pond",
    var_pond = "q",
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
graph <- 
  df_uv_nominal |> 
  mutate(
    exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter$bijouterie)
  ) |> 
  graph_lines_comparison(
    x = "t",
    y = "uv",
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
    path_output = NULL,
    width = 15,
    height = 8,
    print = FALSE,
    return_output = TRUE,
    var_facet = "sector"
  ) +
  theme(legend.key.size = unit(1, "cm"))

print(graph)

ggsave(
  here(
    list_path_graphs_folder$valeurs_unitaires, 
    "evolution-uv-nominal.png"
  ),
  graph,
  width = 15,
  height = 8
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
  here(
    list_path_graphs_folder$valeurs_unitaires,
    "evolution-uv-100-comparison-with-france.png"
  ),
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
    path_output = here(list_path_graphs_folder$valeurs_unitaires,
                       "evolution-uv-100-france.png"),
    width = 15,
    height = 8,
    print = FALSE,
    return_output = TRUE
  ) 


# Graph bar comparaison uv début et fin
# Sans bijouterie
df_uv_nominal |> 
  filter(sector != "Bijouterie") |> 
  mutate(
    exporter_name_region = factor(exporter_name_region, 
                                  levels = ordre_pays_exporter$bijouterie)
  ) |> 
  graph_bar_comp_year(
    x = "exporter_name_region",
    y = "uv",
    var_fill = "exporter_name_region",
    var_t = "t",
    stack = TRUE, 
    year_1 = 2010,
    year_2 = 2022,
    color_1 = "black",
    color_2 = "black",
    alpha = 0.6,
    manual_fill = couleurs_pays_exporter$general,
    x_title = "Exportateurs",
    y_title = "Valeurs unitaires en 2010 et 2022",
    title = "",
    subtitle = "",
    caption = "Source : BACI",
    fill_legend = "",
    type_theme = "bw",
    path_output = here(list_path_graphs_folder$valeurs_unitaires,
                       "evolution-uv-nominal-bar-general.png"),
    width = 15,
    height = 8,
    print = TRUE,
    return_output = FALSE,
    var_facet = "sector"
  )

# Que bijouterie
df_uv_nominal |> 
  filter(
    sector == "Bijouterie",
    exporter_name_region != "Turquie"  
  ) |> 
  mutate(
    exporter_name_region = factor(exporter_name_region, 
                                  levels = ordre_pays_exporter$bijouterie)
  ) |> 
  graph_bar_comp_year(
    x = "exporter_name_region",
    y = "uv",
    var_fill = "exporter_name_region",
    var_t = "t",
    stack = TRUE, 
    year_1 = 2010,
    year_2 = 2022,
    color_1 = "black",
    color_2 = "black",
    alpha = 0.6,
    manual_fill = couleurs_pays_exporter$general,
    x_title = "Exportateurs",
    y_title = "Valeurs unitaires en 2010 et 2022",
    title = "",
    subtitle = "",
    caption = "Source : BACI",
    fill_legend = "",
    type_theme = "bw",
    path_output = here(list_path_graphs_folder$valeurs_unitaires,
                       "evolution-uv-nominal-bar-bijouterie.png"),
    width = 15,
    height = 8,
    print = TRUE,
    return_output = FALSE,
    var_facet = "sector"
  )


remove(df_uv_nominal, df_uv_100, df_uv_100_france)


# Tests Khandelwal --------------------------------------------------------

# Définir les variables de gravité à inclure dans la base
gravity_variables <-
  c(
    "year", "iso3_o", "iso3_d", "dist", "contig", "distw_harmonic",
    "comlang_off", "comlang_ethno", "comcol", "col45", "col_dep_ever", "pop_o",
    "pop_d", "gdp_o", "gdp_d", "gdpcap_o", "gdpcap_d"
  )


# Créer BACI_total qui contient les données baci traités avec tous les flux
# toute qualité pour les produits et années voulues
source(here(path_functions_folder, "create_baci_total.R"))

path_baci_mi_brute |>
  create_baci_total(codes = df_products_HG$k, path_output = path_baci_total)


# Importer les données totales de baci (toutes les gammes de flux)
df_baci_total <-
  path_baci_total |>
  open_dataset()


# Extraire les x pays les plus gros commercialement
# Enlever les petits pays à même de biaiser l'analyse
biggest_countries <-
  df_baci_total |>
  filter(t == 2022) |>
  summarize(
    .by = c(exporter),
    total_v = sum(v, na.rm = TRUE)
  ) |>
  collect() |>
  slice_max(order_by = total_v, n = 300) |>
  pull(exporter)


# Créer le dataframe à utiliser pour l'estimation de la qualité
create_quality_df(
  baci = df_baci_total |> filter(exporter %in% biggest_countries),
  gravity = path_gravity_parquet_folder,
  years = 2010:2022,
  codes = df_products_HG$k,
  gravity_variables = gravity_variables,
  baci_variables = c("exporter_name_region", "sector", "gamme_fontagne_1997", "importer_name_region"),
  revision_codes = "HS92",
  print = FALSE,
  return_output = FALSE,
  return_parquet = FALSE,
  path_output = path_gravity_khandelwal,
  format = "parquet"
  )


# Ouvrir la base contenant les données à utiliser pour l'estimation de qualité
df_quality <-
  path_gravity_khandelwal |>
  open_dataset()


# Définir les variables indépendantes à utiliser
x_formula <-
  "gdp_o + contig + dist + comlang_off + col_dep_ever"

# Estimer la régression de khandelwal
res_quality <-
  khandelwal_quality_eq(
    data_reg = df_quality  |> collect(), 
    y_var = "demand",
    x_var = x_formula,
    fe_var = "k^importer^t",
    path_latex_output = NULL,
    title_latex = NULL,
    label_latex = NULL,
    print_reg_output = TRUE,
    return_output = TRUE
  )


### Comparer mesures agrégations de la qualité ----------------------------
# Calculer une mesure agrégée de la qualité par secteur par gamme et exporter
res_quality$data_reg <-
  res_quality$data_reg |>
  filter(exporter_name_region == "France")

source(here(path_functions_folder, "comp_quality_aggregate.R"))

comp_quality_aggregate(
   data = res_quality$data_reg,
   method_aggregate_vector = c("mean", "median", "weighted.mean", "weighted.median", "weighted.mean", "weighted.median"),
   weighted_var_vector = c("NULL", "NULL", "q", "q", "v", "v"),
   fixed_weight = FALSE,
   year_ref = 2010,
   year_display = c(2010),
   path_output = here(path_exploration_folder, "comparaison-aggregation-quality.xlsx")
  )


### Tester méthodes pour les poids fixes --------------------------------------
var_des <- c("t", "exporter", "importer", "k")
var_des_2 <- c("exporter", "importer", "k")

df_poids <-
  res_quality$data_reg  |>
  select(all_of(var_des), "q")  |>
  filter(t == 2010)  |>
  rename(q_2010 = q) |>
  select(-"t") |>
  print()

res_quality$data_reg |>
  left_join(
    df_poids,
    by = var_des_2
  ) 

# 4, 3, 3, 2
df_pas_fixe <-
  res_quality$data_reg |>
  quality_aggregate(
    var_aggregate = c("t", "exporter_name_region", "sector"),
    method_aggregate = "weighted.median",
    weighted_var = "q",
    fixed_weight = FALSE,
    year_ref = 2010,
    print_output = FALSE,
    return_output = TRUE
  ) |>
  filter(t ==2021)  |>
  arrange(sector, desc(quality)) |>
  ## print(n = 100)
  mutate(
    .by = c(t, sector),
    classement_pas_fixe = row_number()
  ) |>
  rename(quality_pas_fixe = quality) |>
  print(n = 100)

# 5, 5, 4, 1
df_fixe <- 
  res_quality$data_reg |>
  quality_aggregate(
    var_aggregate = c("t", "exporter_name_region", "sector"),
    method_aggregate = "weighted.median",
    weighted_var = "q",
    fixed_weight = TRUE,
    year_ref = 2010,
    print_output = FALSE,
    return_output = TRUE
  ) |>
  filter(t ==2021)  |>
  arrange(sector, desc(quality)) |>
  ## print(n = 100) |>
  mutate(
    .by = c(t, sector),
    classement_fixe = row_number()
  ) |>
  rename(quality_fixe = quality) |>
  print(n = 100)

df_pas_fixe |>
  left_join(
    df_fixe,
    join_by(t, exporter_name_region, sector)
  ) |>
  mutate(
    classement_diff = classement_fixe - classement_pas_fixe
  ) |>
  print(n = 100)


res_quality$data_reg |>
  quality_aggregate(
    var_aggregate = c("t", "exporter_name_region", "k"),
    method_aggregate = "weighted.median",
    weighted_var = "q",
    fixed_weight = TRUE,
    year_ref = 2010,
    print_output = FALSE,
    return_output = TRUE
  ) |>
  filter(t ==2021)  |>
  arrange(k, desc(quality)) |>
  slice_max(
    by = c(t, k),
    quality, n = 1
  ) |>
  summarize(
    .by = c(t, exporter_name_region),
    nb_premier = n()
  )  |>
  arrange(desc(nb_premier))

res_quality$data_reg |>
  quality_aggregate(
    var_aggregate = c("t", "exporter_name_region", "k"),
    method_aggregate = "weighted.median",
    weighted_var = "q",
    fixed_weight = FALSE,
    year_ref = 2010,
    print_output = FALSE,
    return_output = TRUE
  ) |>
  filter(t ==2021)  |>
  arrange(k, desc(quality)) |>
  slice_max(
    by = c(t, k),
    quality, n = 1
  ) |>
  summarize(
    .by = c(t, exporter_name_region),
    nb_premier = n()
  )|>
  arrange(desc(nb_premier))



### analyse exploratoire sur les données de compétitivité hors-prix----------

res_quality$data_reg |>
  filter(t ==2021) |>
  ## filter(exporter_name_region == "France", t == 2021) |>
  summarize(
    .by = c(gamme_fontagne_1997, sector),
    moyenne = mean(quality, na.rm = TRUE)
  ) |>
  arrange(sector, desc(moyenne)) |>
  print(n = 100)


res_quality$data_reg |>
quality_aggregate(
    var_aggregate_k = c("sector", "gamme_fontagne_1997"),
    var_aggregate_i = "exporter_name_region",
    method_aggregate = "mean",
    weighted_var = "q",
    fixed_weight = FALSE,
    year_ref = 2010,
    print_output = FALSE,
    return_output = TRUE
) |>
  filter(exporter_name_region == "France", t == 2021) |>
  arrange(sector, desc(quality)) |>
  print(n = 100)






### Tests graphiques ---------------------------------------------------
df_uv_agg <-
  path_baci_processed |>
  uv_comp(
    years = 2010:2021,
    formula = "mean",
    var_pond = "q",
    var_exporter = "exporter_name_region",
    var_k = "sector",
    base_100 = FALSE
  )


# Graphique x-y avec déplacement entre 2010 et 2022
df_quality_uv <-
  df_quality_agg |>
  left_join(
    df_uv_agg,
    join_by(t, exporter_name_region, sector)
  ) |>
   mutate(
    exporter_name_region = factor(exporter_name_region, 
                                   levels = ordre_pays_exporter$bijouterie)
   ) |>
  filter(
    ## t %in% c(2010, 2021),
    exporter_name_region %in% c("France", "Italie", "Chine et HK", "Reste de l'UE")
  )

df_market_share <-
  path_baci_processed |>
  market_share(
    years = 2010:2021,
    summarize_k = "sector",
    summarize_v = "exporter_name_region",
    return_output = TRUE
  )

df_quality_uv_ms <-
  df_quality_uv  |>
  left_join(
    df_market_share,
    join_by(t, exporter_name_region, sector)
  )

segments <-
  df_quality_uv  |>
  summarise(
    .by = c(exporter_name_region, sector),
    uv_start = uv[t == 2010],
    quality_start = quality[t == 2010],
    uv_end = uv[t == 2021],
    quality_end = quality[t == 2021]
  )

df_quality_uv_ms |>
  filter(t %in% c(2010, 2021)) |>
  ggplot(aes(x = uv, y = quality, color = exporter_name_region)) +
  geom_point(size = 5, alpha = 0.8) +
  geom_segment(data = segments,
               aes(
                 x = uv_start,
                 y = quality_start,
                 xend = uv_end,
                 yend = quality_end), 
               arrow = arrow(length = unit(0.3, "cm")), size = 1) +
  scale_color_manual(values = couleurs_pays_exporter$bijouterie) +
  facet_wrap(~sector, scales = "free") +
  theme_bw()






df_quality_uv |>
  ggplot(aes(x = uv, y = quality))+
  geom_point()

cor.test(df_quality_uv$uv, df_quality_uv$quality)

feols(
  log(uv) ~ log(quality) | t, data = df_quality_uv
)

df_log_diff <-
  df_quality_uv_ms |>
  mutate(
    .by = c(exporter_name_region, sector),
    lag_ms = dplyr::lead(market_share, 1),
    market_share = market_share - lag(market_share, 1),
    market_uv = log(uv) - log(lag(uv, 1)),
    quality = log(quality) - log(lag(quality, 1))
  )  |>print()

feols(market_share ~ quality | t^sector, data = df_quality_uv_ms)

feols(
  market_share ~ uv + quality | t^sector,
  data = df_log_diff, panel.id = c("t", "exporter_name_region")
)

feols(
  market_share ~ I(uv-quality) + quality | t^sector, data = df_quality_uv_ms
)

df_quality_uv_ms |>
  ggplot(aes(x = uv, y = market_share)) +
  geom_point() +
  geom_smooth()

?feols

# Graphique barres stackées
df_quality_agg |>
  mutate(
    exporter_name_region = factor(exporter_name_region, 
                                   levels = ordre_pays_exporter$bijouterie)
  ) |>
  graph_bar_comp_year(
    x = "exporter_name_region",
    y = "quality",
    stack = TRUE,
    var_t = "t",
    year_1 = 2010,
    year_2 = 2021,
    color_1 = "black",
    color_2 = "black",
    alpha = 0.7,
    var_fill = "exporter_name_region",
    manual_fill = couleurs_pays_exporter$bijouterie,
    var_facet = "sector",
    x_title  = "Exportateurs",
    y_title = "Compétitivité hors prix en 2010 et 2021",
    caption = "Source : BACI et Gravity",
    path_output = here(path_graphs_folder, "hors-prix", "hors-prix-bar.png")
  )

# Graphique lines
df_quality_agg |>
  mutate(
    exporter_name_region = factor(exporter_name_region, 
                                   levels = ordre_pays_exporter$bijouterie)
  ) |>
  graph_lines_comparison(
    x = "t",
    y = "quality",
    linewidth = 1,
    var_linetype = "exporter_name_region",
    manual_linetype = linetype_exporter$bijouterie,
    var_color = "exporter_name_region",
    manual_color = couleurs_pays_exporter$bijouterie,
    var_facet = "sector",
    x_title = "Années",
    y_title = "Compétitivité hors-prix",
    caption = "Source : BACI et Gravity",
    path_output = here(path_graphs_folder, "hors-prix", "horx-prix-courbes.png")
  )
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
