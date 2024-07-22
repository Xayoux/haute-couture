
# Importer les éléments obligatoires ----------------------------------------
library(here)
source(
  here::here(
    "02-codes", 
    "R-codes",
    "scripts-annexes",
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

# Téléchargement des bases à utiliser ---------------------------------------
## Télécharger la base de données BACI -------------------------------------
dl_baci(
  dl_folder = path_baci_folder_origine, rm_csv = TRUE
)


## Télécharger la base de données Gravity ----------------------------------
dl_gravity(dl_folder = here::here("..", "Gravity"), dl_zip = FALSE)


# Création des bases de données à utiliser ----------------------------------
## Création de la base BACI mi-brute ---------------------------------------
# Sans outliers, gammes calculées, secteurs définis, tous les flux

# Supprimer dossier BACI mi-brute si existe déjà
if(dir.exists(path_baci_mi_brute)) unlink(path_baci_mi_brute, recursive = TRUE)


path_baci_folder_parquet_origine |>
  open_dataset() |>
  filter(
    !(exporter %in% c("CHN", "HKG") & importer %in% c("CHN", "HKG"))
  ) |>
  analyse.competitivite::clean_uv_outliers(
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


# Charger baci_mi_brute en format arrow
df_baci_mi_brute <-
  path_baci_mi_brute |>
  open_dataset()


## Création de la base BACI utilisée et les documents associés -------------
# Importer la fonction pour créer la base baci-processed
source(
  here(
    path_functions_create_data_folder,
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
  return_output     = FALSE,
  remove            = TRUE
)


# Charger baci_processed en format arrow
df_baci_processed <-
  path_baci_processed |>
  open_dataset()

# Importer les données des produits et concurrents du haut de gamme
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


## Création de la base BACI-total ------------------------------------------
# Créer la base BACI-total qui contient les données de BACI traités mais
# contenant tous les flux "H", "M", "L".

# Importer la fonction pour créer BACI-total
source(here(path_functions_create_data_folder, "create_baci_total.R"))


# Créer BACI-total
path_baci_mi_brute |>
  create_baci_total(codes = df_products_HG$k, path_output = path_baci_total)


# Charger baci_total en format arrow
df_baci_total <-
  path_baci_total |>
  open_dataset()


## Création de gravity avec PIB mis à jour --------------------------------
# Importer la fonction
source(here(path_functions_create_data_folder, "create_gravity_gdp_maj.R"))

# Définir les variables de gravité à garder
gravity_variables <-
  c(
    "year", "iso3_o", "iso3_d", "dist", "contig", "distw_harmonic",
    "comlang_off", "comlang_ethno", "comcol", "col45", "col_dep_ever",
    "gdp_o", "gdp_d"
  )

# Mettre à jour les données de PIB
create_gravity_gdp_maj(
  path_raw_data_folder = path_raw_data_folder,
  last_year_gravity = 2021,
  gravity_variables = gravity_variables,
  path_gravity_parquet_folder = path_gravity_parquet_folder,
  path_output = path_gravity_gdp_maj_parquet_folder 
)


## Création de la base Gravity-Khandelwal ----------------------------------
# Utilisation de gravity avec PIB maj avec la banque mondiale
# Définir les variables de gravité à prendre dans la base
gravity_variables <-
  c(
    "year", "iso3_o", "iso3_d", "dist", "contig", "distw_harmonic",
    "comlang_off", "comlang_ethno", "comcol", "col45", "col_dep_ever",
    "gdp_o", "gdp_d"
  )

# Définir les variables de BACI à prendre dans la base
baci_variables <-
  c(
    "exporter_name_region", "sector", "gamme_fontagne_1997", "importer_name_region"
  )

# Base combinant BACI et Gravity pour calculer la compté hors-prix
# Utilisation de gravity avec les données du PIB mise à jour
path_baci_total  |>
  create_quality_df(
    gravity = path_gravity_gdp_maj_parquet_folder,
    years = 2010:2022,
    codes = df_products_HG$k,
    gravity_variables = gravity_variables,
    baci_variables = baci_variables,
    revision_codes = "HS92",
    print = FALSE,
    return_output = FALSE,
    return_parquet = FALSE,
    path_output = path_gravity_khandelwal,
    format = "parquet"
  )

df_gravity_khandelwal <-
  path_gravity_khandelwal |>
  open_dataset()


# Table LaTeX des produits sélectionnés initialement ------------------------
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


# Nombre de produits sélectionnés selon l'année de référence ----------------
df_nb_product_by_year_ref <- 
  df_baci_mi_brute |> 
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


# Evolution des valeurs unitaires mondiales et françaises -------------------
# Calculer la médiane de la médiane de référence pour chaque secteur
# Permet de regarder l'évolution des "prix" médians au niveau mondial
df_monde <- 
  df_baci_mi_brute  |>
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
  df_baci_mi_brute |> 
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


# Nombre de produits par concurrents 2010 VS 2022 ---------------------------
table <- 
  df_baci_mi_brute |>
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


# Evolution du commerce mondial HG ------------------------------------------
graph <-
  df_baci_processed |>
  summarize(
    .by = c(t, sector),
    v = sum(v, na.rm = TRUE)
  ) |>
  collect() |>
  ggplot(aes(x = t, y = v)) +
  geom_line() +
  labs(
    x = "Années",
    y = "Valeur commerciale en milliers de dollars courants"
  ) +
  scale_x_continuous(breaks = seq(2010,2022, 2)) + 
  facet_wrap(~sector, scales = "free") +
  theme_bw() +
  ggplot2::theme(
    # Option des gridlines : les enlever
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    # Option du texte de l'axe des X
    axis.text.x =
      ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = 18,
        color = "black"
      ),
    axis.title.x =
      ggplot2::element_text(
        size = 22,
        vjust = -0.5
      ),
    # Option du texte de l'axe des Y
    axis.text.y =
      ggplot2::element_text(
        size = 18,
        color = "black"
      ),
    axis.title.y =
      ggplot2::element_text(
        size = 22
      ),
    # Options des facettes
    strip.background =
      ggplot2::element_rect(
        colour = "black",
        fill = "#D9D9D9"
      ),
    strip.text =
      ggplot2::element_text(
        size = 18,
        color = "black"
      )
  )

graph

ggsave(here(list_path_graphs_folder$introduction, "commerce-mondial-HG.png"),
       graph, width = 15, height = 8)

# Part du HG dans le commerce -----------------------------------------------
## Données ------------------------------------------------------------------
# Valeurs et quantités pour chaque secteur par gamme dans le monde
df_commerce_sector_gamme <-
  df_baci_total |>
  # Retirer les NA des gammes
  filter(!is.na(gamme_fontagne_1997)) |>
  # Calculer la somme de v et q pour chaque année, secteur et gamme
  summarize(
    .by = c(t, sector, gamme_fontagne_1997),
    q = sum(q, na.rm = TRUE),
    v = sum(v, na.rm = TRUE)
  ) |>
  collect() |>
  # Ordre pour les graphiques
  mutate(
    gamme_fontagne_1997 = factor(gamme_fontagne_1997, levels = ordre_gammes)
  ) |>
  # Calculer % que chaque gamme représente dans t et secteur
  mutate(
    .by = c(t, sector),
    total_q = sum(q, na.rm = TRUE),
    total_v = sum(v, na.rm = TRUE),
    share_q = q / total_q * 100,
    share_v = v / total_v * 100
  )

write_csv(df_commerce_sector_gamme, here(path_df_folder, "10-commerce-sector-gamme-monde.csv"))


# Valeurs et quantités pour chaque secteur par gamme par pays
df_commerce_sector_gamme_pays <-
  df_baci_total |>
  # Retirer les NA des gammes
  filter(!is.na(gamme_fontagne_1997)) |>
  # Calculer la somme de v et q pour chaque année, secteur et gamme
  summarize(
    .by = c(t, sector, gamme_fontagne_1997, exporter_name_region),
    q = sum(q, na.rm = TRUE),
    v = sum(v, na.rm = TRUE)
  ) |>
  collect() |>
  # Ordre pour les graphiques
  mutate(
    gamme_fontagne_1997 = factor(gamme_fontagne_1997, levels = ordre_gammes)
  ) |>
  # Calculer % que chaque gamme représente dans t et secteur
  mutate(
    .by = c(t, sector, exporter_name_region),
    total_q = sum(q, na.rm = TRUE),
    total_v = sum(v, na.rm = TRUE),
    share_q = q / total_q * 100,
    share_v = v / total_v * 100
  )

write_csv(df_commerce_sector_gamme_pays, here(path_df_folder, "10-commerce-sector-gamme-pays.csv"))


## Fichier de résultats -----------------------------------------------------
sheet_name <- "share_HG_commerce"
if (!sheet_name %in% getSheetNames(path_excel_results)){
  addWorksheet(wb_results, sheet_name)
}


# Ecriture des parts de chaque gamme dans les secteurs
writeData(wb_results, sheet_name, "Parts des gammes dans chaque secteur : Monde",
          rowNames = FALSE, startRow = 1, startCol = 1)

writeData(wb_results, sheet_name, df_commerce_sector_gamme,
          rowNames = FALSE, startRow = 2, startCol = 1)

# Ecriture des parts de chaque gamme dans les secteurs pour chaque pays
writeData(wb_results, sheet_name, "Parts des gammes dans chaque secteur pour chaque région",
          rowNames = FALSE, startRow = 1, startCol = ncol(df_commerce_sector_gamme) + 3)

writeData(wb_results, sheet_name, df_commerce_sector_gamme_pays,
          rowNames = FALSE, startRow = 2, startCol = ncol(df_commerce_sector_gamme) + 3)


saveWorkbook(wb_results, path_excel_results, overwrite = TRUE)


## Graphiques monde ---------------------------------------------------------
# Représentation par secteur : quantités
df_commerce_sector_gamme |>
  mutate(
    gamme_fontagne_1997 = factor(gamme_fontagne_1997, levels = ordre_gammes)
  ) |>
  graph_market_share(
    x = "t",
    y = "share_q",
    graph_type = "area",
    var_fill_color = "gamme_fontagne_1997",
    palette_color = "Paired",
    percent = TRUE,
    x_title = "Années",
    y_title = "Quantités en tonnes métriques",
    var_facet = "sector",
    path_output = here(list_path_graphs_folder$share_HG, "share-HG-quantity-monde.png")
  )

# Représentation par secteur : valeur
df_commerce_sector_gamme |>
  mutate(
    gamme_fontagne_1997 = factor(gamme_fontagne_1997, levels = ordre_gammes)
  ) |>
  graph_market_share(
    x = "t",
    y = "share_v",
    graph_type = "area",
    var_fill_color = "gamme_fontagne_1997",
    palette_color = "Paired",
    percent = TRUE,
    x_title = "Années",
    y_title = "Valeur commerciale (milliers de dollars courants)",
    var_facet = "sector",
    path_output = here(list_path_graphs_folder$share_HG, "share-HG-value-monde.png")
  )


## Graphiques France --------------------------------------------------------
# Représentation par secteur : quantités
df_commerce_sector_gamme_pays |>
  filter(exporter_name_region == "France") |>
  mutate(
    gamme_fontagne_1997 = factor(gamme_fontagne_1997, levels = ordre_gammes)
  )  |>
  graph_market_share(
    x = "t",
    y = "share_q",
    graph_type = "area",
    var_fill_color = "gamme_fontagne_1997",
    palette_color = "Paired",
    percent = TRUE,
    x_title = "Années",
    y_title = "Quantités en tonnes métriques",
    var_facet = "sector",
    path_output = here(list_path_graphs_folder$share_HG, "share-HG-quantity-france.png")
  )

# Représentation par secteur : valeur
df_commerce_sector_gamme_pays |>
  filter(exporter_name_region == "France") |>
  mutate(
    gamme_fontagne_1997 = factor(gamme_fontagne_1997, levels = ordre_gammes)
  )  |>
  graph_market_share(
    x = "t",
    y = "share_v",
    graph_type = "area",
    var_fill_color = "gamme_fontagne_1997",
    palette_color = "Paired",
    percent = TRUE,
    x_title = "Années",
    y_title = "Valeur commerciale (milliers de dollars courants)",
    var_facet = "sector",
    path_output = here(list_path_graphs_folder$share_HG, "share-HG-value-france.png")
  )


## Graphiques Italie --------------------------------------------------------
# Représentation par secteur : quantités
df_commerce_sector_gamme_pays |>
  filter(exporter_name_region == "Italie") |>
  mutate(
    gamme_fontagne_1997 = factor(gamme_fontagne_1997, levels = ordre_gammes)
  ) |>
  graph_market_share(
    x = "t",
    y = "share_q",
    graph_type = "area",
    var_fill_color = "gamme_fontagne_1997",
    palette_color = "Paired",
    percent = TRUE,
    x_title = "Années",
    y_title = "Quantités en tonnes métriques",
    var_facet = "sector",
    path_output = here(list_path_graphs_folder$share_HG, "share-HG-quantity-italie.png")
  )

# Représentation par secteur : valeur
df_commerce_sector_gamme_pays |>
  filter(exporter_name_region == "Italie") |>
  mutate(
    gamme_fontagne_1997 = factor(gamme_fontagne_1997, levels = ordre_gammes)
  ) |>
  graph_market_share(
    x = "t",
    y = "share_v",
    graph_type = "area",
    var_fill_color = "gamme_fontagne_1997",
    palette_color = "Paired",
    percent = TRUE,
    x_title = "Années",
    y_title = "Valeur commerciale (milliers de dollars courants)",
    var_facet = "sector",
    path_output = here(list_path_graphs_folder$share_HG, "share-HG-value-italie.png")
  )


## Graphiques Chine ---------------------------------------------------------
# Représentation par secteur : quantités
df_commerce_sector_gamme_pays |>
  filter(exporter_name_region == "Chine et HK") |>
  mutate(
    gamme_fontagne_1997 = factor(gamme_fontagne_1997, levels = ordre_gammes)
  ) |>
  graph_market_share(
    x = "t",
    y = "share_q",
    graph_type = "area",
    var_fill_color = "gamme_fontagne_1997",
    palette_color = "Paired",
    percent = TRUE,
    x_title = "Années",
    y_title = "Quantités en tonnes métriques",
    var_facet = "sector",
    path_output = here(list_path_graphs_folder$share_HG, "share-HG-quantity-chine.png")
  )

# Représentation par secteur : valeur
df_commerce_sector_gamme_pays |>
  filter(exporter_name_region == "Chine et HK") |>
  mutate(
    gamme_fontagne_1997 = factor(gamme_fontagne_1997, levels = ordre_gammes)
  ) |>
  graph_market_share(
    x = "t",
    y = "share_v",
    graph_type = "area",
    var_fill_color = "gamme_fontagne_1997",
    palette_color = "Paired",
    percent = FALSE,
    x_title = "Années",
    y_title = "Valeur commerciale (milliers de dollars courants)",
    var_facet = "sector",
    path_output = here(list_path_graphs_folder$share_HG, "share-HG-value-chine.png")
  )


# Part de la mode dans le commerce français ---------------------------------
## Données ------------------------------------------------------------------
# Parts de la mode dans le commerce français total
df_share_mode_commerce_fr <-
  # Prendre les données de baci complètes
  path_baci_folder_parquet_origine |>
  open_dataset()  |>
  # Garder uniquement la France et les années étudiées
  filter(exporter == "FRA", t %in% 2010:2022)  |>
  # Clean les outliers
  analyse.competitivite::clean_uv_outliers(
      method = "sd",
      seuil_H = 3,
      seuil_L =3,
      path_output = NULL,
      return_output = TRUE,
      return_pq = TRUE
  )  |>
  # Définir les gammes
  analyse.competitivite::gamme_ijkt_fontagne_1997(
      ponderate = "q",
      alpha_H = 3,
      pivot = "longer",
      path_output = NULL,
      return_output = TRUE,
      return_pq = TRUE
  )|>
  # Définir ce qui est de la mode et haute couture ou non
  mutate(
    sector = substr(k, 1, 2),
    sector = 
      dplyr::case_when(
        sector %in% c("61", "62", "65") ~ "Habillement",
        sector == "42" ~ "Maroquinerie",
        sector == "64" ~ "Chaussures",
        sector == "71" ~ "Bijouterie",
        .default = "Autre"
      ),
    # Haute couture si dans secteurs que l'on veut + haut de gamme
    sector_big =
      case_when(
        sector != "Autre" & gamme_fontagne_1997 == "H" ~ "Mode et HC",
        .default = "Autre"
      )
  )  |>
  # Sommer les valeurs et quantités des flux de mode et les autres
  summarize(
    .by = c(sector_big, t),
    total_v = sum(v, na.rm = TRUE),
    total_q = sum(q, na.rm = TRUE)
  ) |>
  collect() |>
  # Calculer la part que représente le commerce de haute-couture
  mutate(
    .by = c(t),
    share_v = total_v / sum(total_v, na.rm = TRUE) * 100,
    share_q = total_q / sum(total_q, na.rm = TRUE) * 100
  )  |>
  arrange(desc(t))


## Fichier de résultats ----------------------------------------------------
sheet_name <- "Part mode commerce français"
if (!sheet_name %in% getSheetNames(path_excel_results)){
  addWorksheet(wb_results, sheet_name)
}

# Ecriture de la part 
writeData(wb_results, sheet_name, "Part du commerce de mode et de haute-couture dans le commerce français",
          rowNames = FALSE, startRow = 1, startCol = 1)

writeData(wb_results, sheet_name, df_share_mode_commerce_fr,
          rowNames = FALSE, startRow = 2, startCol = 1)

saveWorkbook(wb_results, path_excel_results, overwrite = TRUE)


# Marge extensive -----------------------------------------------------------
## Données ------------------------------------------------------------------
# df avec le nombre de produits par secteurs
# Sert à calculer le nombre total de marchés possibles
df_nb_k_sector <-
  df_baci_processed |>
  filter(t == 2022, exporter == "FRA") |>
  select(sector, k) |>
  distinct() |>
  summarize(
    .by = c(sector),
    nb_k = n()
  ) |>
  collect()

# Nombre de pays au total : sert à calculer nb marchés possibles
nb_countries <-
  df_baci_processed  |>
  filter(t == 2022) |>
  collect() |>
  pull(exporter) |>
  unique() |>
  length()


# Nombre de marchés par pays
df_nb_market <-
  df_baci_processed |>
  summarize(
    .by = c(t, exporter, sector),
    nb_market = n()
  ) |>
  collect() |>
  arrange(desc(t), sector, desc(nb_market)) |>
  # Calculer le % de marchés atteint
  left_join(
    df_nb_k_sector,
    join_by(sector)
  ) |>
  # Enlever 1 au nb de pays pour ne pas compter le pays exportateur
  mutate(
    share_nb_market = nb_market / (nb_k * (nb_countries - 1)) * 100
  )

write_csv(df_nb_market, here(path_df_folder, "11-nb-market.csv"))


# Nombre de marchés où le pays est premier
df_nb_market_first <-
  df_baci_processed |>
  # Calculer les parts de marché par rapport à chaque importer
  market_share(
    summarize_k = "k",
    summarize_v = "importer",
    by = "exporter",
    return_output = TRUE,
    return_pq = TRUE
  ) |>
  collect() |>
  # Garder uniquement la part de marché la plus élevée
  slice_max(
    by = c(t, k, importer),
    market_share,
    n = 1
  )  |>
  # Remettre les secteurs
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
  # Compter le nombre de fois où le pays est premier
  summarize(
    .by = c(t, sector, exporter),
    nb_first = n()
  ) |>
  arrange(desc(t), sector, desc(nb_first))

write_csv(df_nb_market_first, here(path_df_folder, "11-nb-market-first.csv"))


# Nb moyen de produits envoyés par pays dans chaque secteur
df_nb_mean_k <-
  df_baci_processed |>
  # Compter le nombre de produits envoys dans chaque pays par secteur
  summarize(
    .by = c(t, sector, exporter, importer),
    nb_produits = n()
  )  |>
  collect() |>
  # Moyenne du nombre de produits envoyés
  summarize(
    .by = c(t, sector, exporter),
    mean_k = mean(nb_produits, na.rm = TRUE)
  ) |>
  # Trier
  arrange(desc(t), sector, desc(mean_k)) |>
  # Garder uniquement 2010 et 2022
  filter(t %in% c(2010, 2022)) |>
  # Mettre les années en colonne : meilleure présentation
  pivot_wider(
    names_from = t,
    values_from = mean_k
  ) |>
  clean_names()  

write_csv(df_nb_mean_k, here(path_df_folder, "11-nb-mean-k.csv"))


## Fichier de résultats -----------------------------------------------------
sheet_name <- "Nombre marchés"
if (!sheet_name %in% getSheetNames(path_excel_results)){
  addWorksheet(wb_results, sheet_name)
}


# Ecrire le nombre de marchés sur lesquels sont présents chaque pays
writeData(wb_results, sheet_name, "Nombre de marchés par secteurs par pays",
          rowNames = FALSE, startRow = 1, startCol = 1)

writeData(wb_results, sheet_name, df_nb_market,
          rowNames = FALSE, startRow = 2, startCol = 1)

# Ecrire le nombre de marchés sur lesquels les pays sont premiers en MS
writeData(wb_results, sheet_name, "Nombre de marchés où les pays sont premiers en MS",
          rowNames = FALSE, startRow = 1, startCol = ncol(df_nb_market) + 3)

writeData(wb_results, sheet_name, df_nb_market_first,
          rowNames = FALSE, startRow = 2, startCol = ncol(df_nb_market) + 3)


# Ecrire le nb moyen de produits envoyés par destination
writeData(wb_results, sheet_name, "Nombre de produits moyens envoyés par pays",
          rowNames = FALSE, startRow = 1, startCol = ncol(df_nb_market) + 3 + ncol(df_nb_market_first) + 3)

writeData(wb_results, sheet_name, df_nb_mean_k,
          rowNames = FALSE, startRow = 2, startCol = ncol(df_nb_market) + 3 + ncol(df_nb_market_first) + 3)

saveWorkbook(wb_results, path_excel_results, overwrite = TRUE)


## Graphiques ---------------------------------------------------------------
# Graphiques lignes du nombre de marché par pays
df_nb_market |>
  filter(exporter %in% c("FRA", "ITA", "CHN"))  |>
  graph_lines_comparison(
    x = "t",
    y = "nb_market",
    var_color = "exporter",
    manual_color = couleurs_pays_exporter$marge_extensive,
    x_title = "Années",
    y_title = "Nombre de marchés",
    var_facet = "sector",
    path_output = here(list_path_graphs_folder$marge_extensive, "nb-market.png")
  )


# Graphique bar du nombre de marché par pays
graph <-
  df_nb_market |>
  filter(exporter %in% c("FRA", "ITA", "CHN"))  |>
  graph_bar_comp_year(
    x = "exporter",
    y = "nb_market",
    stack = TRUE,
    double_bar = FALSE,
    var_t = "t",
    year_1 = 2022,
    year_2 = 2010,
    color_1 = "black",
    color_2 = "black",
    var_fill = "exporter",
    manual_fill = couleurs_pays_exporter$marge_extensive,
    shape = 22,
    size_shape = 5,
    var_fill_shape = "exporter",
    alpha = 1.5,
    na.rm = TRUE,
    x_title = "Exportateurs",
    y_title = "Nombre de marchés",
    type_theme = "bw",
    var_facet = "sector",
    print = FALSE,
    return_output = TRUE,
    path_output = NULL
  ) +
  scale_x_discrete(labels = c("FRA" = "France", "ITA" = "Italie", "CHN" = "Chine")) +
  theme(legend.position = "none")

graph

ggsave(
  here(list_path_graphs_folder$marge_extensive, "nb-market-bar.png"),
  graph,
  width = 15,
  height = 8
)


# Graphique bar du nombre de marché par pays en % du nb de marchés possibles
graph <-
  df_nb_market |>
  filter(exporter %in% c("FRA", "ITA", "CHN"))  |>
  graph_bar_comp_year(
    x = "exporter",
    y = "share_nb_market",
    stack = TRUE,
    double_bar = FALSE,
    var_t = "t",
    year_1 = 2022,
    year_2 = 2010,
    color_1 = "black",
    color_2 = "black",
    var_fill = "exporter",
    manual_fill =  couleurs_pays_exporter$marge_extensive,
    shape = 22,
    size_shape = 5,
    var_fill_shape = "exporter",
    alpha = 1.5,
    na.rm = TRUE,
    x_title = "Exportateurs",
    y_title = "Pourcentage du nombre de marchés possibles",
    type_theme = "bw",
    var_facet = "sector",
    print = FALSE,
    return_output = TRUE,
    path_output = NULL
  ) +
  scale_x_discrete(labels = c("FRA" = "France", "ITA" = "Italie", "CHN" = "Chine")) +
  theme(legend.position = "none")

graph

ggsave(
  here(list_path_graphs_folder$marge_extensive, "share-nb-market-bar.png"),
  graph,
  width = 15,
  height = 8
)


# Graphique bar du nombre de marché où le pays est premier
graph <-
  df_nb_market_first |>
  filter(exporter %in% c("FRA", "ITA", "CHN"))  |>
  graph_bar_comp_year(
    x = "exporter",
    y = "nb_first",
    stack = TRUE,
    double_bar = FALSE,
    var_t = "t",
    year_1 = 2022,
    year_2 = 2010,
    color_1 = "black",
    color_2 = "black",
    var_fill = "exporter",
    manual_fill = couleurs_pays_exporter$marge_extensive,
    shape = 22,
    size_shape = 5,
    var_fill_shape = "exporter",
    alpha = 1.5,
    na.rm = TRUE,
    x_title = "Exportateurs",
    y_title = "Nombre de marchés",
    type_theme = "bw",
    var_facet = "sector",
    print = TRUE,
    return_output = TRUE,
    path_output = NULL
  ) +
  scale_x_discrete(labels = c("FRA" = "France", "ITA" = "Italie", "CHN" = "Chine")) +
  theme(legend.position = "none")

graph

ggsave(
  here(list_path_graphs_folder$marge_extensive, "nb-market-first-bar.png"),
  graph,
  width = 15,
  height = 8
)


## Table LaTeX --------------------------------------------------------------
#Table du nombre de produits moyen exporté par pays par secteur
table <-
  df_nb_mean_k |>
  # Garder que les pays d'intérets de l'étude : FRA et concurrents
  filter(exporter %in% c("FRA", "ITA", "CHN")) |>
  # Organiser la table
  relocate(sector, exporter, x2010) |>
  # Renommer les pays avec leur nom complet
  mutate(
    exporter =
      case_when(
        exporter == "FRA" ~ "France",
        exporter == "ITA" ~ "Italie",
        exporter == "CHN" ~ "Chine"
      )
  ) |>
  # Trier pour aficher les secteurs ensemble
  arrange(sector, desc(x2022)) |>
  # Garder uniquement la valeur au milieu pour le nom du secteur
  # Meilleure présentation
  mutate(
    .by = sector,
    num = row_number(),
    sector =
      case_when(
        num != 2 ~ "",
        .default = sector
      )
  ) |>
  select(-num) %>%
  # passer en table latex
  {
    nb_lignes <- nrow(.) # Sert pour mettre la dernière hline
    xtable(.) %>%
      print.xtable(
        type             = "latex",
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents    = TRUE,
        hline.after      = seq(3, nb_lignes - 1, 3)
      )
  }

# Supprimer les derniers \\
writeLines(
  substr(table, 1, nchar(table)-7), 
  here(path_tables_folder, "table-nb-mean-product-export.tex")
)

# Balance commerciale du haut de gamme --------------------------------------f
## Données ------------------------------------------------------------------
### Données par région ------------------------------------------------------
# Calculer le total des exportations de chaque région
df_total_export <-
  df_baci_processed |>
  summarize(
    .by = c(t, sector, exporter_name_region),
    total_export = sum(v, na.rm = TRUE)
  ) |>
  collect()


# Calculer le total des importations de chaque région
df_total_import <-
  df_baci_processed |>
  summarize(
    .by = c(t, sector, importer_name_region),
    total_import = sum(v, na.rm = TRUE)
  ) |>
  collect()


# Calculer la balance commerciale du HG
df_balance_commerciale <-
  df_total_export |>
  left_join(
    df_total_import,
    join_by(t, sector, exporter_name_region == importer_name_region)
  )  |>
  mutate(
    balance_comm = total_export / total_import
  )  |>
  arrange(desc(t), sector, exporter_name_region)

# Enregistrer les données 
write_csv(df_balance_commerciale, here(path_df_folder, "12-balance-commerciale-HG.csv"))


### Données par pays --------------------------------------------------------
# Calculer le total des exportations de chaque région
df_total_export_pays <-
  df_baci_processed |>
  summarize(
    .by = c(t, sector, exporter),
    total_export = sum(v, na.rm = TRUE)
  ) |>
  collect()


# Calculer le total des importations de chaque région
df_total_import_pays <-
  df_baci_processed |>
  summarize(
    .by = c(t, sector, importer),
    total_import = sum(v, na.rm = TRUE)
  ) |>
  collect()


# Calculer la balance commerciale du HG
df_balance_commerciale_pays <-
  df_total_export_pays |>
  left_join(
    df_total_import_pays,
    join_by(t, sector, exporter == importer)
  )  |>
  mutate(
    balance_comm = total_export / total_import
  )  |>
  arrange(desc(t), sector, exporter)

# Enregistrer les données 
write_csv(df_balance_commerciale, here(path_df_folder, "12-balance-commerciale-HG-pays.csv"))


## Fichier de résultats -----------------------------------------------------
sheet_name <- "Balance commerciale HG"
if (!sheet_name %in% getSheetNames(path_excel_results)){
  addWorksheet(wb_results, sheet_name)
}


# Ecrire la balance commerciale des différentes régions pour le HG
writeData(wb_results, sheet_name, "Balance commerciale pour le HG",
          rowNames = FALSE, startRow = 1, startCol = 1)

writeData(wb_results, sheet_name, df_balance_commerciale,
          rowNames = FALSE, startRow = 2, startCol = 1)

# Ecrire la balance commerciale des différents pays pour le HG
writeData(wb_results, sheet_name, "Balance commerciale des pays pour le HG",
          rowNames = FALSE, startRow = 1, startCol = ncol(df_balance_commerciale) + 3)

writeData(wb_results, sheet_name, df_balance_commerciale_pays,
          rowNames = FALSE, startRow = 2, startCol = ncol(df_balance_commerciale) + 3)

saveWorkbook(wb_results, path_excel_results, overwrite = TRUE)


## Graphiques ---------------------------------------------------------------
# Balance commerciale pour la France
graph <-
  df_balance_commerciale |>
  filter(exporter_name_region == "France") |>
  graph_lines_comparison(
    x = "t",
    y = "balance_comm",
    var_color = "sector",
    palette_color = "Paired",
    x_title = "Années",
    y_title = "Balance commerciale du haut de gamme",
    print = FALSE
  )+
  geom_hline(
    yintercept = 1, color = "black"
  )

graph

ggsave(here(list_path_graphs_folder$balance_commerciale, "balance-commerciale-HG-france.png"),
       graph, width = 15, height = 8)



# Balance commerciale : graphs lignes
# Fonction pour créer le graph pour chaque secteur de façon séparée
g_balance_commerciale_func <- function(df){
  graph_balance_commerciale <-
    df  |>
    mutate(
      exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter$bijouterie)
    ) |>
    graph_lines_comparison(
      x = "t",
      y = "balance_comm",
      var_color = "exporter_name_region",
      manual_color = couleurs_pays_exporter$bijouterie,
      var_linetype = "exporter_name_region",
      manual_linetype = linetype_exporter$bijouterie,
      x_title = "Années",
      y_title = "Balance commerciale",
      var_facet = "sector",
      print = FALSE
    ) +
    geom_hline(
      yintercept = 1, color = "black"
    )

  return(graph_balance_commerciale)
}

# Créer une liste où chaque élément sont les données pour un secteur
list_df_balance_commerciale <-
  df_balance_commerciale |>
  group_nest(sector, keep = TRUE) |>
  pull(data)

# Créer liste où chaque éléments sont le graph pour chaque secteur
list_graph_balance_commerciale <-
  map(
    list_df_balance_commerciale,
    g_balance_commerciale_func
  )

# Disposer les graphs dans un seul graph en 2:2
graph_balance_commerciale_unique <-
  (list_graph_balance_commerciale[[1]] + list_graph_balance_commerciale[[2]]) /
  (list_graph_balance_commerciale[[3]] + list_graph_balance_commerciale[[4]])

# Sauvegarder le graphique
ggsave(here(list_path_graphs_folder$balance_commerciale, "balance-commerciale-HG-line.png"),
       graph_balance_commerciale_unique, width = 16, height = 11)




# Graph en barre pour tous les secteurs : balance commerciale
# Fonction pour créer le graph pour un secteur
g_bar_balance_commerciale_func <- function(df){
  graph_balance_commerciale <-
    df |>
    mutate(
      exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter$bijouterie)
    ) |>
    graph_bar_comp_year(
      x = "exporter_name_region",
      y = "balance_comm",
      stack = TRUE,
      double_bar = FALSE,
      var_t = "t",
      year_1 = 2022,
      year_2 = 2010,
      color_1 = "black",
      color_2 = "black",
      var_fill = "exporter_name_region",
      manual_fill = couleurs_pays_exporter$bijouterie,
      shape = 22,
      size_shape = 5,
      var_fill_shape = "exporter_name_region",
      alpha = 1.5,
      na.rm = TRUE,
      x_title = "Exportateurs",
      y_title = "Balance commerciale",
      type_theme = "bw",
      var_facet = "sector",
      print = FALSE,
      return_output = TRUE
    ) +
    geom_hline(yintercept = 1, color = "black") +
    theme(legend.position = "none")

  return(graph_balance_commerciale)
}

# Créer une liste où chaque élément sont les données pour un secteur
list_df_balance_commerciale <-
  df_balance_commerciale |>
  group_nest(sector, keep = TRUE) |>
  pull(data)

# Créer liste où chaque éléments sont le graph pour chaque secteur
list_graph_bar_balance_commerciale <-
  map(
    list_df_balance_commerciale,
    g_bar_balance_commerciale_func
  )

# Disposer les graphs dans un seul graph en 2:2
graph_bar_balance_commerciale_unique <-
  list_graph_bar_balance_commerciale[[1]] + list_graph_bar_balance_commerciale[[2]] +
  list_graph_bar_balance_commerciale[[3]] + list_graph_bar_balance_commerciale[[4]] +
  plot_layout(ncol = 4)


# Sauvegarder le graphique
ggsave(here(list_path_graphs_folder$balance_commerciale, "balance-commerciale-HG-bar.png"),
       graph_bar_balance_commerciale_unique, width = 16, height = 11)


# Parts de marché des exportateurs ------------------------------------------
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
    path_output   = here(path_df_folder, "03-market-share-country-exporter.csv"),
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
    path_output   = here(path_df_folder, "03-market-share-regions-exporter.csv"),
    return_output = TRUE,
    return_pq     = FALSE
  ) |> 
  arrange(desc(t), sector, desc(market_share))

## Fichier de résultats ---------------------------------------------------
# Ajouter une nouvelle feuille au fichier de résultat si elle n'existe pas
sheet_name <- "Market share exporter"
if (!sheet_name %in% getSheetNames(path_excel_results)){
  addWorksheet(wb_results, sheet_name)
}


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

## Graphiques -------------------------------------------------------------
# Graph des parts de marché des exportateurs : tous les secteurs
# Fonction pour créer le graph pour un secteur
g_area_market_share_exporter_func <- function(df){
  graph_area_market_share_exporter <-
    df |>
    mutate(
    # Appliquer l'ordre d'apprition aux régions
    exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter$bijouterie)
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
    path_output = NULL,
    print = FALSE,
    return_output = TRUE
  )

  return(graph_area_market_share_exporter)
}

# Liste contenant un df par secteur
list_df_market_share_country_region_exporter <-
  df_market_share_country_region_exporter  |>
  group_nest(sector, keep = TRUE) |>
  pull(data)

# Créer un graphique par secteur
list_graph_area_market_share_exporter <-
  map(
    list_df_market_share_country_region_exporter,
    g_area_market_share_exporter_func
  )

graph_area_market_share_exporter_unique <-
  (list_graph_area_market_share_exporter[[1]] + list_graph_area_market_share_exporter[[2]]) /
  (list_graph_area_market_share_exporter[[3]] + list_graph_area_market_share_exporter[[4]])

ggsave(here(list_path_graphs_folder$market_share, "market-share-hg-exporter-countries.png"),
       graph_area_market_share_exporter_unique, width = 16, height = 11)


# Evolution commerce des secteurs -------------------------------------------
## Calcul des valeurs commercials par secteur -------------------------------
# Valeurs commerciales totales par secteur
df_v_sector <- 
  df_market_share_country_region_exporter |> 
  summarize(
    .by = c(t, sector),
    total_v = sum(v, na.rm = TRUE)
  ) |> 
  arrange(desc(t), desc(total_v))

# Enregistrer le dataframe
write_csv(df_v_sector, here(path_df_folder, "04-commerce-total-secteur.csv"))


## Fichier de résultats ---------------------------------------------------
# Créer une nouvelle feuille dans le fichier de résultat si elle n'existe pas
sheet_name <- "Evolution commerce"
if (!sheet_name %in% getSheetNames(path_excel_results)){
  addWorksheet(wb_results, sheet_name)
}

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



# Parts de marché des importateurs ------------------------------------------
## Données ------------------------------------------------------------------
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
    path_output   = here(path_df_folder, "05-market-share-country-exporter.csv"),
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
    path_output   = here(path_df_folder, "05-market-share-regions-exporter.csv"),
    return_output = TRUE,
    return_pq     = FALSE
  ) |> 
  arrange(desc(t), sector, desc(market_share))

## Fichier de résultats ---------------------------------------------------
# Créer une nouvelle feuille dans le fichier de résultat si elle n'existe pas
sheet_name <- "Market share importer"
if (!sheet_name %in% getSheetNames(path_excel_results)){
  addWorksheet(wb_results, sheet_name)
}


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


## Graphiques -------------------------------------------------------------
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


# Direction des exportations ------------------------------------------------
## Données ------------------------------------------------------------------
# Destination des exportations : ce que représente l'importer pour l'exporter
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
  # Garder uniquement le France, l'Italie et la Chine dans les exporter
  filter(
    exporter_name_region %in% c("France", "Italie", "Chine et HK")
  )

# Sauvegarder le fichier
write_csv(df_destination_exports, here(path_df_folder, "06-destination-exports.csv"))


## Fichier de résultats -----------------------------------------------------
# Créer une nouvelle feuille dans le fichier de résultat si elle n'existe pas
sheet_name <- "Direction exportations"
if (!sheet_name %in% getSheetNames(path_excel_results)){
  addWorksheet(wb_results, sheet_name)
}

# Ecriture des direction des exportations dans le fichier de résultats
writeData(wb_results, sheet_name, "Direction des exportations",
          rowNames = FALSE, startRow = 1, startCol = 1)

writeData(wb_results, sheet_name, df_destination_exports,
          rowNames = FALSE, startRow = 2, startCol = 1)

saveWorkbook(wb_results, path_excel_results, overwrite = TRUE)


## Graphiques ------------------------------------------------------------
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

# Exécuter la fonction de représentation pour chaque graphique
walk(
  sector_vector, 
  \(sector_vector) market_share_by_exporter(df_destination_exports, sector_vector)
)

# Faire un graphique contenant les 4 secteurs er 3 pays
graph <-
  df_destination_exports |>
  collect() |>
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
    ## var_facet = c("exporter_name_region", "sector"),
    path_output = NULL,
    width = 15,
    height = 8,
    print = FALSE,
    return_output = TRUE
  ) +
  # Facet_grid pour avoir uniquement un strip par colonne et un par ligne
  facet_grid(sector ~ exporter_name_region)

graph

ggsave(
  here(list_path_graphs_folder$direction_exportations,
       "directions-exportations.png"
       ),
  graph,
  width = 16,
  height = 11
)

# Valeurs d'importation des != régions --------------------------------------
## Données ------------------------------------------------------------------
# Valeur totale des importations par secteur pour chaque importateur
df_import_value <-
  df_baci_processed |>
  summarize(
    .by = c(t, importer_name_region, sector),
    v = sum(v, na.rm = TRUE)
  ) |>
  arrange(desc(t), sector, desc(v)) |>
  collect()

write_csv(df_import_value, here(path_df_folder, "14-import-value.csv"))

# Taux de croissance des importations totales par secteur entre 2010 et 2022
df_croissance_import_value<-
  df_import_value |>
  filter(t %in% c(2010, 2022)) |>
  pivot_wider(
    names_from = t,
    values_from = v,
    names_prefix = "x"
  ) |>
  mutate(
    taux_croissance = (x2022 - x2010) / x2010 * 100
  )  |>
  select(-c(x2022, x2010))

write_csv(df_croissance_import_value, here(path_df_folder, "14-croissance-import-value.csv"))


## Fichier de résultats -----------------------------------------------------
# Créer une nouvelle feuille dans le fichier de résultat si elle n'existe pas
sheet_name <- "Valeur importations"
if (!sheet_name %in% getSheetNames(path_excel_results)){
  addWorksheet(wb_results, sheet_name)
}


# Ecriture des valeurs d'importation de chaque région
writeData(wb_results, sheet_name, "Valeur des importations",
          rowNames = FALSE, startRow = 1, startCol = 1)

writeData(wb_results, sheet_name, df_import_value,
          rowNames = FALSE, startRow = 2, startCol = 1)


# Ecriture des croissances des valeurs d'importation de chaque région
writeData(wb_results, sheet_name, "Croissance des valeur des importations entre 2010 et 2022",
          rowNames = FALSE, startRow = 1, startCol = ncol(df_import_value) + 3)

writeData(wb_results, sheet_name, df_croissance_import_value,
          rowNames = FALSE, startRow = 2, startCol = ncol(df_import_value) + 3)

saveWorkbook(wb_results, path_excel_results, overwrite = TRUE)

## Graphique ----------------------------------------------------------------
# Graph ligne de l'évolution des valeurs d'importations totales
df_import_value |>
  mutate(
    importer_name_region = factor(importer_name_region, levels = ordre_pays_importer$general)
  ) |>
filter(
  !importer_name_region %in% c("RDM", "Amérique", "Moyen-Orient", "Suisse")
)  |>
  graph_lines_comparison(
    x = "t",
    y = "v",
    var_linetype = "importer_name_region",
    manual_linetype = linetype_importer$general,
    var_color = "importer_name_region",
    manual_color = couleurs_pays_importer$general,
    x_title = "Années",
    y_title = "Valeurs importées (milliers de $ courants)",
    type_theme = "bw",
    var_facet = "sector",
    path_output = here(list_path_graphs_folder$valeur_importations, "valeurs-importations.png")
  )

# Graph barre de l'évolution de valeurs d'importations totales
graph <-
  df_import_value |>
  mutate(
    importer_name_region = factor(importer_name_region, levels = ordre_pays_importer$general)
  ) |>
  graph_bar_comp_year(
    x = "importer_name_region",
    y = "v",
    stack = TRUE,
    double_bar = FALSE,
    var_t = "t",
    year_1 = 2022,
    year_2 = 2010,
    color_1 = "black",
    alpha = 0.7,
    var_fill = "importer_name_region",
    manual_fill = couleurs_pays_importer$general,
    var_fill_shape = "importer_name_region",
    x_title = "Importateurs",
    y_title = "Importations en milliers de dollars courants",
    var_facet = "sector",
    path_output = NULL,
    print = TRUE,
    return_output = TRUE
  ) +
  theme(
    legend.position = "none"
  )

graph

ggsave(
  here(list_path_graphs_folder$valeur_importations, "valeurs-importations-bar.png"),
  graph,
  width = 15,
  height = 8
)


# Graph bar des taux de croissance des valeurs d'importations
graph <-
  df_croissance_import_value |>
  mutate(
    importer_name_region = factor(importer_name_region, levels = ordre_pays_importer$general)
  ) |>
  ggplot(aes(x = importer_name_region, y = taux_croissance, fill = importer_name_region)) +
  geom_bar(stat = "identity", width = 0.6) +
  coord_flip()+
  scale_fill_manual(values = couleurs_pays_importer$general)+
  scale_y_continuous(labels = label_percent(scale = 1)) + 
  labs(
    y = "Taux de croissance des importations entre 2010 et 2022",
    x = ""
  ) +
  facet_grid(.~sector, scales = "free_x") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    # Option du texte de l'axe des X
    axis.text.x =
      ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = 18,
        color = "black"
      ),
    axis.title.x =
      ggplot2::element_text(
        size = 22,
        vjust = -0.5
      ),
    # Option du texte de l'axe des Y
    axis.text.y =
      ggplot2::element_text(
        size = 18,
        color = "black"
      ),
    axis.title.y =
      ggplot2::element_text(
        size = 22
      ),
    # Options de la légende
    legend.position  = "none",
    # Options des facettes
    strip.background =
      ggplot2::element_rect(
        colour = "black",
        fill = "#D9D9D9"
      ),
    strip.text =
      ggplot2::element_text(
        size = 18,
        color = "black"
      )
  )


graph

ggsave(
  here(list_path_graphs_folder$valeur_importations, "croissance-valeurs-importations.png"),
  graph,
  width = 15,
  height = 8
)


# Demande adressée ----------------------------------------------------------
## Calcul des demandes adressées --------------------------------------------
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
    path_output = here(path_df_folder, "07-adressed-demand-base-100-compare-france.csv")
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
    path_output = NULL,
    ) |> 
  filter(exporter_name_region == "France")

write_csv(df_da_france, here(path_df_folder, "07-adressed-demand-base-100-france.csv"))


## Fichier de résultats -----------------------------------------------------
sheet_name <- "Adressed demand"
if (!sheet_name %in% getSheetNames(path_excel_results)){
  addWorksheet(wb_results, sheet_name)
}

# Ecriture de la demande adressée base 100 comparée à la France
writeData(wb_results, sheet_name, "Demande adressée en base 100 comparée à la France",
          rowNames = FALSE, startRow = 1, startCol = 1)

writeData(wb_results, sheet_name, df_da,
          rowNames = FALSE, startRow = 2, startCol = 1)

# Ecrire des parts de marché des régions importatrices
writeData(wb_results, sheet_name, "Demande adressée en base 100 de la France",
          rowNames = FALSE, startRow = 1, 
          startCol = ncol(df_da) + 3)

writeData(wb_results, sheet_name, df_da_france,
          rowNames = FALSE, startRow = 2, 
          startCol = ncol(df_da) + 3)

saveWorkbook(wb_results, path_excel_results, overwrite = TRUE)


## Représentations graphiques ----------------------------------------------
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
    color_legend = "",
    type_theme = "classic",
    path_output = here(list_path_graphs_folder$demande_adressee,
                       "demande-adressee-france.png"),
    width = 15,
    height = 8,
    print = TRUE,
    return_output = TRUE
  )

# Evolution de l'évolution de la comparaison de la demande adressée
# des différentes régions par rapport à la France
# Fonction pour créer un graph pour le secteur
g_line_adressed_demand_func <- function(df){
  graph_line_adressed_demand <-
    df |>
    mutate(
      exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter$bijouterie)
    ) |>
    filter(
      exporter_name_region != "France",
      ) |> 
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
      color_legend = "",
      type_theme = "bw",
      path_output = NULL,
      print = FALSE,
      return_output = TRUE
    ) +
    facet_wrap(~sector) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    theme(legend.key.size = unit(1, "cm"))
}

# Liste avec un dataframe par secteur
list_df_adressed_demand <-
  df_da |>
  group_nest(sector, keep = TRUE) |>
  pull(data)

# Liste avec un graph par secteur
list_graph_line_adressed_demand <-
  map(
    list_df_adressed_demand,
    g_line_adressed_demand_func
  )

graph_line_adressed_demand_unique <-
  list_graph_line_adressed_demand[[1]] + list_graph_line_adressed_demand[[2]] +
  list_graph_line_adressed_demand[[3]] + list_graph_line_adressed_demand[[4]] +
  plot_layout(ncol = 2)

ggsave(
  here(
    list_path_graphs_folder$demande_adressee,
    "demande-adressee-comparaison-with-france.png"
  ),
  graph_line_adressed_demand_unique,
  width = 16,
  height = 11
)


# Evolution des valeurs unitaires -------------------------------------------
## Calcul des différentes mesures de valeurs unitaires ----------------------
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
    path_output = here(path_df_folder, "08-uv-nominal.csv")
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
    path_output = here(path_df_folder, "08-uv-base-100-compare-france.csv")
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

write_csv(df_uv_100_france, here(path_df_folder, "08-uv-base-100-france.csv"))


## Fichier de résultats -----------------------------------------------------
# Créer une nouvelle feuille dans le fichier de résultat si elle n'existe pas
sheet_name <- "Valeurs unitaires"
if (!sheet_name %in% getSheetNames(path_excel_results)){
  addWorksheet(wb_results, sheet_name)
}


# Ecriture des valeurs unitaires nominales
writeData(wb_results, sheet_name, "Valeurs unitaires nominales",
          rowNames = FALSE, startRow = 1, startCol = 1)

writeData(wb_results, sheet_name, df_uv_nominal,
          rowNames = FALSE, startRow = 2, startCol = 1)

# Ecriture des valeurs unitaires base 100 comparaison avec France
writeData(wb_results, sheet_name, "Valeurs unitaires base 100 : comapraison avec France",
          rowNames = FALSE, startRow = 1, 
          startCol = ncol(df_uv_nominal) + 3)

writeData(wb_results, sheet_name, df_uv_100,
          rowNames = FALSE, startRow = 2, 
          startCol = ncol(df_uv_nominal) + 3)

# Ecriture des valeurs unitaires base 100 de la France
writeData(wb_results, sheet_name, "Valeurs unitaires base 100 de la France",
          rowNames = FALSE, startRow = 1, 
          startCol = ncol(df_uv_nominal) + 3 + ncol(df_uv_100) + 3)

writeData(wb_results, sheet_name, df_uv_100_france,
          rowNames = FALSE, startRow = 2, 
          startCol = ncol(df_uv_nominal) + 3 + ncol(df_uv_100) + 3)

saveWorkbook(wb_results, path_excel_results, overwrite = TRUE)


## Représentations graphiques -----------------------------------------------
### Représentations lignes --------------------------------------------------
# Graphique de l'évolution des valeurs unitaires nominales par secteur
# Fonction pour créer un graphique pour le secteur
g_line_uv <- function(df){
  graph_line_uv <-
    df |>
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
      color_legend = "",
      type_theme = "bw",
      path_output = NULL,
      print = FALSE,
      return_output = TRUE,
      var_facet = "sector"
    ) +
    theme(legend.key.size = unit(1, "cm"))

  return(graph_line_uv)
}

# Liste avec un df par secteur
list_df_uv_nominal <-
  df_uv_nominal |>
  group_nest(sector, keep = TRUE) |>
  pull(data)

# Liste avec un graph par secteur
list_graph_uv <-
  map(
    list_df_uv_nominal,
    g_line_uv
  )

# Graph unique
graph_line_uv_unique <-
  list_graph_uv[[1]] + list_graph_uv[[2]] +
  list_graph_uv[[3]] + list_graph_uv[[4]] +
  plot_layout(ncol = 2)

# Enregistrer le graph
ggsave(
  here(
    list_path_graphs_folder$valeurs_unitaires, 
    "evolution-uv-nominal.png"
  ),
  graph_line_uv_unique,
  width = 16,
  height = 11
)


# Graphique de l'évolution des valeurs unitaires en base 100 par secteur
# Comparaison avec la France
g_line_uv_base_100_comp <- function(df){
  graph_line_uv_base_100_comp <-
    df |>
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

  return(graph_line_uv_base_100_comp)
}

# Liste avec un df par secteur
list_df_uv_base_100_comp <-
  df_uv_100 |>
  group_nest(sector, keep = TRUE) |>
  pull(data)

# Liste avec un graph par secteur
list_graph_uv_base_100_comp <-
  map(
    list_df_uv_base_100_comp,
    g_line_uv_base_100_comp
  )

# Graph unique
graph_line_uv_base_100_comp_unique <-
  list_graph_uv_base_100_comp[[1]] + list_graph_uv_base_100_comp[[2]] +
  list_graph_uv_base_100_comp[[3]] + list_graph_uv_base_100_comp[[4]] +
  plot_layout(ncol = 2)

# Enregistrer le graph
ggsave(
  here(
    list_path_graphs_folder$valeurs_unitaires, 
    "evolution-uv-100-comparison-with-france.png"
  ),
  graph_line_uv_base_100_comp_unique,
  width = 16,
  height = 11
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
    color_legend = "",
    type_theme = "classic",
    path_output = here(list_path_graphs_folder$valeurs_unitaires,
                       "evolution-uv-100-france.png"),
    width = 15,
    height = 8,
    print = FALSE,
    return_output = TRUE
  ) 


### Représentations barres --------------------------------------------------
# Comparaison 2010-2022 des valeurs unitaires entre pays et secteurs
# Fonction pour faire le graph pour un secteur
g_bar_uv_func <- function(df){
  graph_bar_uv <-
    df |>
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
      double_bar = FALSE,
      fill_shape = "black",
      var_fill_shape = "exporter_name_region",
      size_shape = 5,
      year_1 = 2022,
      year_2 = 2010,
      color_1 = "black",
      color_2 = "black",
      alpha = 0.6,
      manual_fill = couleurs_pays_exporter$bijouterie,
      x_title = "Exportateurs",
      y_title = "Valeurs unitaires",
      title = "",
      subtitle = "",
      fill_legend = "",
      type_theme = "bw",
      path_output =NULL,
      print = FALSE,
      return_output = TRUE,
      var_facet = "sector"
    ) +
    theme(legend.position = "none")
} 

# Liste avec un dataframe par secteur
list_df_uv_nominal <-
  df_uv_nominal  |>
  # Enlever la Turquie de la bijouterie
  # Valeurs unitaires trop élevées comparé aux autres
  filter(!(sector == "Bijouterie" & exporter_name_region == "Turquie")) |>
  group_nest(sector, keep = TRUE) |>
  pull(data)

# Liste avec un graph par secteur
list_graph_bar_uv <-
  map(
    list_df_uv_nominal,
    g_bar_uv_func
  )

# Graphique unique
graph_bar_uv_unique <-
  list_graph_bar_uv[[1]] + list_graph_bar_uv[[2]] +
  list_graph_bar_uv[[3]] + list_graph_bar_uv[[4]] +
  plot_layout(ncol = 4)

# Enregistrer le graph
ggsave(
  here(list_path_graphs_folder$valeurs_unitaires,
       "evolution-uv-nominal-bar-carre.png"),
  graph_bar_uv_unique,
  width = 16,
  height = 11
)


## Table LaTeX des taux de croissance ---------------------------------------
# Faire une table contenant les taux de croissance des valeurs unitaires
table <-
  path_baci_processed |> 
  open_dataset() |>
  # Calculer les valeurs uinitaires en base 100 pour chaque pays secteur
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
  )  |>
  # Garder uniquement 2022 car année d'étude
  filter(t == 2022) |>
  # Le taux de croissance entre 2010 et 2022 c'est la base 100 - 100
  mutate(taux_croissance = uv_100 - 100) |>
  select(exporter_name_region, sector, taux_croissance)  |>
  arrange(sector, desc(taux_croissance)) |>
  # Transformer en table tex
  xtable()  |>
  print.xtable(
    type = "latex",
    hline.after = NULL,
    include.rownames = FALSE,
    include.colnames = FALSE,
    only.contents = TRUE
  )

# Supprimer les derniers \\ et enregistrer le fichier
writeLines(
  substr(table, 1, nchar(table)-7), 
  here(path_tables_folder, "table-taux-croissance-uv.tex")
)

# Compétitivité hors-prix ---------------------------------------------------
## Préparer les données ---------------------------------------------------
# Extraire les x pays les plus gros commercialement en 2022
# Suppression des plux petits pays si nécessaire
nb_countries <- 300

biggest_countries <-
  df_baci_total |>
  filter(t == 2022) |>
  summarize(
    .by = c(exporter),
    total_v = sum(v, na.rm = TRUE)
  ) |>
  collect() |>
  slice_max(order_by = total_v, n = nb_countries) |>
  pull(exporter)

# Ouvrir la base Gravity-Khandelwal et filtrer par les pays voulus
df_quality <-
  path_gravity_khandelwal |>
  open_dataset() |>
  filter(exporter %in% biggest_countries)

## Régression de Khandelwal ------------------------------------------------
# Estimer la régression de khandelwal
res_quality <-
  khandelwal_quality_eq(
    data_reg = df_quality  |> collect(), 
    y_var = "demand",
    x_var = "gdp_o + contig + dist + comlang_off + col_dep_ever",
    fe_var = "k^importer^t",
    path_latex_output = NULL,
    title_latex = NULL,
    label_latex = NULL,
    print_reg_output = TRUE,
    return_output = TRUE
  )


# Sauver et charger les données format parquet
res_quality$data_reg |>
  arrow_table() |>
  group_by(t) |>
  write_dataset(path_quality_khandelwal)

df_quality_khandelwal <-
  path_quality_khandelwal |>
  open_dataset()


## Hors-prix agrégé --------------------------------------------------------
# Aggréger la mesure de hors prix avec médiane pondérée par q, poids libres
df_quality_agg <-
  df_quality_khandelwal |>
  quality_aggregate(
    var_aggregate = c("t", "exporter_name_region", "sector"),
    method_aggregate = "weighted.median",
    weighted_var = "q",
    fixed_weight = FALSE,
    var_desagregate = c("t", "exporter", "importer", "k"),
    print_output = TRUE,
    return_output = TRUE,
    path_output = here(path_df_folder, "09-df-quality-agg.csv")
  )

# Aggréger la mesure de hors prix en base 100 compararer avec la France
df_quality_agg_base_100 <-
  df_quality_khandelwal |>
  quality_aggregate(
    var_aggregate = c("t", "exporter_name_region", "sector"),
    method_aggregate = "weighted.median",
    weighted_var = "q",
    fixed_weight = FALSE,
    var_desagregate = c("t", "exporter", "importer", "k"),
    print_output = TRUE,
    return_output = TRUE,
    path_output = here(path_df_folder, "09-df-quality-agg-base-100-compare-france.csv"),
    year_ref_base_100 = 2010,
    base_100 = TRUE,
    compare = TRUE,
    exporter_ref = "France",
    var_exporter_ref = "exporter_name_region"
  )

# Aggréger la mesure de hors prix en base 100 comparer avec la France
df_quality_agg_france <-
  df_quality_khandelwal |>
  quality_aggregate(
    var_aggregate = c("t", "exporter_name_region", "sector"),
    method_aggregate = "weighted.median",
    weighted_var = "q",
    fixed_weight = FALSE,
    var_desagregate = c("t", "exporter", "importer", "k"),
    print_output = TRUE,
    return_output = TRUE,
    path_output = NULL,
    year_ref_base_100 = 2010,
    base_100 = TRUE,
    compare = FALSE
  ) |>
  filter(exporter_name_region == "France")
write_csv(df_quality_agg_france, here(path_df_folder, "09-df-quality-agg-base-100-france.csv"))


## Fichier de résultats -----------------------------------------------------
sheet_name <- "Hors-prix"
if (!sheet_name %in% getSheetNames(path_excel_results)){
  addWorksheet(wb_results, sheet_name)
}

# Ecriture du hors-prix agrégé
writeData(wb_results, sheet_name, "Mesure agrégée du hors-prix",
          rowNames = FALSE, startRow = 1, startCol = 1)

writeData(wb_results, sheet_name, df_quality_agg,
          rowNames = FALSE, startRow = 2, startCol = 1)


# Ecriture du hors-prix agrégé base 100 comparaison avec la France
writeData(wb_results, sheet_name, "Mesure agrégée du hors-prix en base 100 comparée avec la France",
          rowNames = FALSE, startRow = 1, startCol = ncol(df_quality_agg) + 3)

writeData(wb_results, sheet_name, df_quality_agg_base_100,
          rowNames = FALSE, startRow = 2, startCol = ncol(df_quality_agg) + 3)


# Ecriture du hors-prix agrégé base 100 pour la France
writeData(wb_results, sheet_name, "Mesure agrégée du hors-prix en base 100 pour la France",
          rowNames = FALSE, startRow = 1, startCol = ncol(df_quality_agg) + 3 + ncol(df_quality_agg_base_100) + 3)

writeData(wb_results, sheet_name, df_quality_agg_france,
          rowNames = FALSE, startRow = 2, startCol = ncol(df_quality_agg) + 3 + ncol(df_quality_agg_base_100) + 3)


saveWorkbook(wb_results, path_excel_results, overwrite = TRUE)


## Représentation graphique -------------------------------------------------
# Evolution du hors-prix en base 100 pour la France
df_quality_agg_france |>
  graph_lines_comparison(
    x = "t",
    y = "quality_100",
    var_color = "sector",
    palette_color = "Paired",
    var_linetype = NULL,
    manual_linetype = NULL,
    x_title = "Année",
    y_title = "Compétitivité hors-prix en base 100",
    title = "",
    subtitle = "",
    color_legend = "",
    type_theme = "bw",
    width = 15,
    height = 8,
    print = FALSE,
    return_output = TRUE,
    var_facet = NULL,
    path_output = here(list_path_graphs_folder$quality, "evolution_quality_100_france.png")
  )


# Evolution du hors-prix comparé à la France pour tous les secteurs 
graph <-
  df_quality_agg_base_100 |>
  mutate(
    exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter$bijouterie)
  ) |> 
  graph_lines_comparison(
    x = "t",
    y = "quality_ratio",
    var_color = "exporter_name_region",
    manual_color = couleurs_pays_exporter$bijouterie,
    var_linetype = "exporter_name_region",
    manual_linetype = linetype_exporter$bijouterie,
    x_title = "Année",
    y_title = "Hors-prix en base 100 comparé avec la France",
    title = "",
    subtitle = "",
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

ggsave(here(list_path_graphs_folder$quality, "evolution_quality_100_comparison_with_france.png"),
       graph, width = 15, height = 8)


# Graphique en barre (2021) avec carré pour 2010
# Fonction pour faire un graph pour le secteur
# but : un graph par secteur puis assemblage en un seul graph : car != légendes
g_bar_hp_func <- function(df){
  graph_bar_hp <-
    df |>
    mutate(
      exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter$bijouterie)
    ) |>
    graph_bar_comp_year(
      x = "exporter_name_region",
      y = "quality",
      stack = TRUE,
      double_bar = FALSE,
      var_t = "t",
      year_1 = 2022,
      year_2 = 2010,
      color_1 = "black",
      color_2 = "black",
      var_fill = "exporter_name_region",
      manual_fill = couleurs_pays_exporter$bijouterie,
      shape = 22,
      size_shape = 5,
      var_fill_shape = "exporter_name_region",
      alpha = 1.5,
      na.rm = TRUE,
      x_title = "Exportateurs",
      y_title = "Compétitivité hors-prix",
      type_theme = "bw",
      var_facet = "sector",
      path_output = NULL,
      print = FALSE,
      return_output = TRUE
    ) +
    theme(legend.position = "none")

  return(graph_bar_hp)
}

# Liste avec un df par secteur 
list_df_hp <-
  df_quality_agg |>
  group_nest(sector, keep = TRUE) |>
  pull(data)

# Liste avec un graph par secteur
list_graph_bar_hp <-
  map(
    list_df_hp,
    g_bar_hp_func
  )

# graph unique
graph_bar_hp_unique <-
  list_graph_bar_hp[[1]] + list_graph_bar_hp[[2]] +
  list_graph_bar_hp[[3]] + list_graph_bar_hp[[4]] +
  plot_layout(ncol = 4)

# Enregistrer le graph
ggsave(
  here(list_path_graphs_folder$quality,
       "evolution-hors-prix-nominal-bar-carre.png"),
  graph_bar_hp_unique,
  width = 16,
  height = 11
)


## Table LaTeX des taux de croissance ---------------------------------------
# Table contenant les taux de croissance du hors-prix entre 2010 et 2022
table <-
  df_quality_khandelwal |>
  # Calculer le hors-prix agrégé en base 100
  quality_aggregate(
    var_aggregate = c("t", "exporter_name_region", "sector"),
    method_aggregate = "weighted.median",
    weighted_var = "q",
    fixed_weight = FALSE,
    var_desagregate = c("t", "exporter", "importer", "k"),
    print_output = TRUE,
    return_output = TRUE,
    path_output = NULL,
    year_ref_base_100 = 2010,
    base_100 = TRUE,
    compare = FALSE
  )  |>
  # Garder uniquement 2022 car année d'étude
  filter(t == 2022) |>
  # Le taux de croissance entre 2010 et 2022 c'est la base 100 - 100
  mutate(taux_croissance = quality_100 - 100) |>
  select(exporter_name_region, sector, taux_croissance)  |>
  arrange(sector, desc(taux_croissance)) |>
  # Transformer en table tex
  xtable()  |>
  print.xtable(
    type = "latex",
    hline.after = NULL,
    include.rownames = FALSE,
    include.colnames = FALSE,
    only.contents = TRUE
  )

# Supprimer les derniers \\ et enregistrer le fichier
writeLines(
  substr(table, 1, nchar(table)-7), 
  here(path_tables_folder, "table-taux-croissance-hp.tex")
)


# Graphiques triples infos --------------------------------------------------
## Données ------------------------------------------------------------------
# fusionner les df pour avoir les données dans un seul df
df_ms_uv_hp <-
  # Prendre les parts de marché
  df_market_share_country_region_exporter |>
  select(t, sector, exporter_name_region, market_share) |>
  # Ajouter les valeurs unitaires
  left_join(
    df_uv_nominal,
    join_by(t, exporter_name_region, sector)
  ) |>
  # Ajouter la qualité agrégée
  left_join(
    df_quality_agg,
    join_by(t, exporter_name_region, sector)
  )

write_csv(df_ms_uv_hp, here(path_df_folder, "13-df-ms-uv-hp.csv"))


# Données en variation entre 2010 et 2022
df_ms_uv_hp_variations <-
  df_ms_uv_hp |>
  filter(t %in% c(2010, 2022)) |>
  pivot_wider(
    names_from = t,
    values_from = c(market_share, uv, quality)
  ) |>
  mutate(
    var_uv = (uv_2022 - uv_2010) / uv_2010 * 100,
    var_quality = (quality_2022 - quality_2010) / quality_2010 * 100
  ) |>
  select(sector, exporter_name_region, market_share_2022, var_uv, var_quality)

write_csv(df_ms_uv_hp_variations, here(path_df_folder, "13-df-ms-uv-hp-variations.csv"))


## Graphiques ---------------------------------------------------------------
### Graph x-y des trois données en 2022 en niveau ---------------------------
# Fonction pour faire un graphique par secteur
g_ms_uv_hp_function <- function(df){
  graph_ms_uv_hp <-
    df  |>
    mutate(
      exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter$bijouterie)
    ) |>
    ggplot(aes(x = uv, y = quality, color = exporter_name_region, size = market_share)) +
    geom_point() +
    scale_color_manual(values = couleurs_pays_exporter$bijouterie) +
    scale_size_continuous(range = c(1,10)) +
    facet_wrap(~sector, scales = "free_x") +
    labs(
      x = "Valeurs unitaires en 2022",
      y = "Mesure du hors-prix en 2022",
      size = "Parts de marché (%)",
      color = "Exportateurs"
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      # Option des titres
      plot.title =
        ggplot2::element_text(
          size = 26,
          hjust = 0.5
        ),
      plot.subtitle =
        ggplot2::element_text(
          size = 22,
          hjust = 0.5
        ),
      plot.caption =
        ggplot2::element_text(
          size = 16,
          hjust = 0,
          color = "black"
        ),
      # Option du texte de l'axe des X
      axis.text.x =
        ggplot2::element_text(
          angle = 45,
          hjust = 1,
          size = 18,
          color = "black"
        ),
      axis.title.x =
        ggplot2::element_text(
          size = 22,
          vjust = -0.5
        ),
      # Option du texte de l'axe des Y
      axis.text.y =
        ggplot2::element_text(
          size = 18,
          color = "black"
        ),
      axis.title.y =
        ggplot2::element_text(
          size = 22
        ),
      # Options de la légende
      legend.position  = "right",
      legend.text =
        ggplot2::element_text(
          size = 18,
          color = "black"
        ),
      legend.key.spacing.y = ggplot2::unit(0.3, "cm"),
      legend.title =
        ggplot2::element_text(
          size = 22,
          color = "black",
          hjust = 0
        ),
      # Options des facettes
      strip.background =
        ggplot2::element_rect(
          colour = "black",
          fill = "#D9D9D9"
        ),
      strip.text =
        ggplot2::element_text(
          size = 18,
          color = "black"
        )
    ) +
    guides(color = guide_legend(override.aes = list(size = 5))) +
    # Ligne invisible pour faire apparaitre le 0 (flemme de me prendre la tête)
    geom_hline(yintercept = 0, color = "white", alpha = 0) +
    geom_vline(xintercept = 0, color = "white", alpha = 0)

  return(graph_ms_uv_hp)
}

# Liste avec un df par secteur
list_df_ms_uv_hp <-
  df_ms_uv_hp |>
  filter(
    t == 2022,
    (sector != "Bijouterie" & !exporter_name_region %in% c("RDM", "Amérique", "Moyen-Orient", "Suisse")) |
      (sector == "Bijouterie" & !exporter_name_region %in% c("RDM", "Amérique", "Moyen-Orient", "Turquie"))
  ) |>
  group_nest(sector, keep = TRUE) |>
  pull(data)

# Liste avec un graphique par secteur
list_graph_ms_uv_hp <-
  map(
    list_df_ms_uv_hp,
    g_ms_uv_hp_function
  )

# Fusionner en un seul graph
graph_ms_uv_hp_unique <-
  list_graph_ms_uv_hp[[1]] + list_graph_ms_uv_hp[[2]] +
  list_graph_ms_uv_hp[[3]] + list_graph_ms_uv_hp[[4]] +
  plot_layout(ncol = 2)

graph_ms_uv_hp_unique

ggsave(
  here(list_path_graphs_folder$ms_uv_hp, "ms-uv-hp-2010-2022.png"),
  graph_ms_uv_hp_unique,
  width = 16,
  height = 11
)


### graph x-y des trois données en variation --------------------------------
# Fonction pour faire un graphique par secteur
g_ms_uv_hp_variations_function <- function(df){
  graph_ms_uv_hp_variations <-
    df  |>
    mutate(
      exporter_name_region = factor(exporter_name_region, levels = ordre_pays_exporter$bijouterie)
    ) |>
    ggplot(aes(x = var_uv, y = var_quality, color = exporter_name_region, size = market_share_2022)) +
    geom_point() +
    scale_color_manual(values = couleurs_pays_exporter$bijouterie) +
    scale_size_continuous(range = c(1,10)) +
    facet_wrap(~sector) +
    labs(
      title = "",
      x = "Variation des valeurs unitaires (%)",
      y = "Variation du hors-prix (%)",
      size = "Parts de marché (%)",
      color = "Exportateurs"
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      # Option des titres
      plot.title =
        ggplot2::element_text(
          size = 26,
          hjust = 0.5
        ),
      plot.subtitle =
        ggplot2::element_text(
          size = 22,
          hjust = 0.5
        ),
      plot.caption =
        ggplot2::element_text(
          size = 16,
          hjust = 0,
          color = "black"
        ),
      # Option du texte de l'axe des X
      axis.text.x =
        ggplot2::element_text(
          angle = 45,
          hjust = 1,
          size = 18,
          color = "black"
        ),
      axis.title.x =
        ggplot2::element_text(
          size = 22,
          vjust = -0.5
        ),
      # Option du texte de l'axe des Y
      axis.text.y =
        ggplot2::element_text(
          size = 18,
          color = "black"
        ),
      axis.title.y =
        ggplot2::element_text(
          size = 22
        ),
      # Options de la légende
      legend.position  = "right",
      legend.text =
        ggplot2::element_text(
          size = 18,
          color = "black"
        ),
      legend.key.spacing.y = ggplot2::unit(0.3, "cm"),
      legend.title =
        ggplot2::element_text(
          size = 22,
          color = "black",
          hjust = 0
        ),
      # Options des facettes
      strip.background =
        ggplot2::element_rect(
          colour = "black",
          fill = "#D9D9D9"
        ),
      strip.text =
        ggplot2::element_text(
          size = 18,
          color = "black"
        )
    ) +
    guides(
      color = guide_legend(override.aes = list(size = 5)),
      size = guide_legend(order = 2)
    )+
    # Ligne invisible pour faire apparaitre le 0 (flemme de me prendre la tête)
    geom_hline(yintercept = 0, color = "white", alpha = 0) +
    geom_vline(xintercept = 0, color = "white", alpha = 0)

  return(graph_ms_uv_hp_variations)
}

# Liste avec un df par secteur
list_df_ms_uv_hp_variations <-
  df_ms_uv_hp_variations |>
  filter(
  (sector != "Bijouterie" & !exporter_name_region %in% c("RDM", "Amérique", "Moyen-Orient", "Suisse")) |
    (sector == "Bijouterie" & !exporter_name_region %in% c("RDM", "Amérique", "Moyen-Orient", "Turquie"))
  ) |>
  group_nest(sector, keep = TRUE) |>
  pull(data)

# Liste avec un graphique par secteur
list_graph_ms_uv_hp_variations <-
  map(
    list_df_ms_uv_hp_variations,
    g_ms_uv_hp_variations_function
  )

# Fusionner en un seul graph
graph_ms_uv_hp_variations_unique <-
  list_graph_ms_uv_hp_variations[[1]] + list_graph_ms_uv_hp_variations[[2]] +
  list_graph_ms_uv_hp_variations[[3]] + list_graph_ms_uv_hp_variations[[4]] +
  plot_layout(ncol = 2)

graph_ms_uv_hp_variations_unique

ggsave(
  here(list_path_graphs_folder$ms_uv_hp, "ms-uv-hp-variation-2010-2022.png"),
  graph_ms_uv_hp_variations_unique,
  width = 16,
  height = 11
)


# Droits de douane ----------------------------------------------------------
## Codes HS révision 5 ------------------------------------------------------
# Obtenir les codes des produits en révision 5. Besoin pour obtenir les
# Droits de douane de Houssein
df_products_HG_revision_5 <-
  df_products_HG |>
  mutate(
    k_revision_5 = concord_hs(k, origin = "HS0", destination = "HS5", dest.digit = 6)
  ) |>
  select(k_revision_5) |>
  rename(k = k_revision_5)

write_csv(df_products_HG_revision_5, here(path_df_folder, "codes-produits-revision-5.csv"))


## Données ------------------------------------------------------------------
# Code FRA : 251
# Code ITA : 380
# Code CHN : 156
# Code HKG : 344

df_countries_mm <-
  here(path_MacMap_folder, "Countries.csv") |>
  read_csv()

df_MacMap_processed <-
  df_MacMap_brute |>
  left_join(
    df_countries_mm |>select(-Country),
    join_by(importer == isoMM)
  ) |>
  rename(
    j_mm = importer,
    importer = L3
  ) |>
  left_join(
    df_countries_mm  |> select(-Country),
    join_by(exporter == isoMM)
  ) |>
  rename(
    i_mm = exporter,
    exporter = L3
  ) |>
  collect()


test <-
  df_baci_processed |>
  left_join(
    df_MacMap_processed,
    join_by(exporter, importer, k == hs6)
  ) |>
  collect()

test |>
  relocate(i, j, i_mm, j_mm, exporter, importer, k, v, q, sector) |>
  filter(is.na(ave_pref_applied))


df_MacMap_brute |>
  filter(exporter == "004", hs6 == "620449", importer == "056") |>
  collect()

df_MacMap_brute |>
  filter(exporter == "056", hs6 == "620449") |>
  collect()

df_MacMap_brute |>
  filter(exporter == "918") |>
  collect()

df_MacMap_brute |>
  summarize(
    .by = c(importer, hs6),
    n = n()
  ) |>
  collect()



df_baci_processed |>
  filter(i == 56) |>
  collect()
 
## ita et fra même dd
## pondéré -> on en dit qqc mais que par rapport aux flux : on exporte plus mais parce quon est + compétitif donc + DD

## moyenne simple pour ITA et FRA et CHN

## moyenne pondérée par flux bilatéraux


