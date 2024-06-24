# Load les différents dataframes ------------------------------------------
## Dataframe des parts de marché des pays exportateurs --------------------
if (file.exists(here::here(path_df_folder, "03-market-share-country-exporter.csv"))){
  df_market_share_country_exporter <-
    here::here(path_df_folder, "03-market-share-country-exporter.csv") |>
    readr::read_csv(show_col_types = FALSE)
}

## Dataframe des parts de marché des régions exportatrices ----------------
if (file.exists(here::here(path_df_folder, "03-market-share-regions-exporter.csv"))){
  df_market_share_country_region_exporter <-
    here::here(path_df_folder, "03-market-share-regions-exporter.csv") |>
    readr::read_csv(show_col_types = FALSE)
}


## Dataframe du commerce total par secteur --------------------------------
if (file.exists(here::here(path_df_folder, "04-commerce-total-secteur.csv"))){
  df_v_sector <-
    here::here(path_df_folder, "04-commerce-total-secteur.csv") |>
    readr::read_csv(show_col_types = FALSE)
}


## Dataframe des parts de marché des pays importateurs --------------------
if (file.exists(here::here(path_df_folder, "05-market-share-country-importer.csv"))){
  df_market_share_country_importer <-
    here::here(path_df_folder, "05-market-share-country-importer.csv") |>
    readr::read_csv(show_col_types = FALSE)
}


## Dataframe des parts de marché des regions importatrices ----------------
if (file.exists(here::here(path_df_folder, "05-market-share-regions-importer.csv"))){
  df_market_share_country_region_importer <-
    here::here(path_df_folder, "05-market-share-regions-importer.csv") |>
    readr::read_csv(show_col_types = FALSE)
}


## Dataframe des destinations des exportations ----------------------------
if (file.exists(here::here(path_df_folder, "06-destination-exports.csv"))){
  df_destination_exports <-
    here::here(path_df_folder, "06-destination-exports.csv") |>
    readr::read_csv(show_col_types = FALSE)
}


## Dataframe de la DA en base 100 comparée avec la France -----------------
if (file.exists(here::here(path_df_folder, "07-adressed-demand-base-100-compare-france.csv"))){
  df_da <-
    here::here(path_df_folder, "07-adressed-demand-base-100-compare-france.csv") |>
    readr::read_csv(show_col_types = FALSE)
}


## Dataframe de la DA de la France en base 100 ----------------------------
if (file.exists(here::here(path_df_folder, "07-adressed-demand-base-100-france.csv"))){
  df_da_france <-
    here::here(path_df_folder, "07-adressed-demand-base-100-france.csv") |>
    readr::read_csv(show_col_types = FALSE)
}


## Dataframe des valeurs unitaires nominales ------------------------------
if (file.exists(here::here(path_df_folder, "08-uv-nominal.csv"))){
  df_uv_nominal <-
    here::here(path_df_folder, "08-uv-nominal.csv") |>
    readr::read_csv(show_col_types = FALSE)
}


## Dataframe des valeurs unitaires en base 100 : comp france -------------
if (file.exists(here::here(path_df_folder, "08-uv-base-100-compare-france.csv"))){
  df_uv_100 <-
    here::here(path_df_folder, "08-uv-base-100-compare-france.csv") |>
    readr::read_csv(show_col_types = FALSE)
}


## Dataframe des valeurs unitaires en base 100 : France ------------------
if (file.exists(here::here(path_df_folder, "08-uv-base-100-france.csv"))){
  df_uv_100_france <-
    here::here(path_df_folder, "08-uv-base-100-france.csv") |>
    readr::read_csv(show_col_types = FALSE)
}


## Dataframe du hors-prix agrégé ----------------------------------------
if (file.exists(here::here(path_df_folder, "09-df-quality-agg.csv"))){
  df_quality_agg <-
    here::here(path_df_folder, "09-df-quality-agg.csv") |>
    readr::read_csv(show_col_types = FALSE)
}

## Dataframe du hors-prix agrégé en base 100 : comp france ---------------
if (file.exists(here(path_df_folder, "09-df-quality-agg-base-100-compare-france.csv"))){
  df_quality_agg_base_100 <-
    here(path_df_folder, "09-df-quality-agg-base-100-compare-france.csv") |>
    readr::read_csv(show_col_types = FALSE)
}

## Dataframe du hors-prix agrégé en base 100 : comp france ---------------
if (file.exists(here(path_df_folder, "09-df-quality-agg-base-100-france.csv"))){
  df_quality_agg_france <-
    here(path_df_folder, "09-df-quality-agg-base-100-france.csv") |>
    readr::read_csv(show_col_types = FALSE)
}

## Dataframe du commerce en fonction des gammes pour le monde
if (file.exists(here(path_df_folder, "10-commerce-sector-gamme-monde.csv"))){
  df_commerce_sector_gamme <-
    here(path_df_folder, "10-commerce-sector-gamme-monde.csv") |>
    readr::read_csv(show_col_types = FALSE)
}

# Dataframe du commerce en fonction des gammes pour la France
if (file.exists(here(path_df_folder, "10-commerce-sector-gamme-france.csv"))){
  df_commerce_sector_gamme_france <-
    here(path_df_folder, "10-commerce-sector-gamme-france.csv") |>
    readr::read_csv(show_col_types = FALSE)
}

# Dataframe du nombre de marchés par pays
if (file.exists(here(path_df_folder, "11-nb-market.csv"))){
  df_nb_market <-
    here(path_df_folder, "11-nb-market.csv") |>
    readr::read_csv(show_col_types = FALSE)
}

# Dataframe des balances commerciales du Haut de gamme
if (file.exists(here(path_df_folder, "12-balance-commerciale-HG.csv"))){
  df_balance_commerciale <-
    here(path_df_folder, "12-balance-commerciale-HG.csv") |>
    readr::read_csv(show_col_types = FALSE)
}

# Dataframe des graphiques triples infos en niveau
if (file.exists(here(path_df_folder, "13-df-ms-uv-hp.csv"))){
  df_ms_uv_hp <-
    here(path_df_folder, "13-df-ms-uv-hp.csv") |>
    readr::read_csv(show_col_types = FALSE)
}

# Dataframe des graphiques triples infos en variations
if (file.exists(here(path_df_folder, "13-df-ms-uv-hp-variations.csv"))){
  df_ms_uv_hp_variations <-
    here(path_df_folder, "13-df-ms-uv-hp-variations.csv") |>
    readr::read_csv(show_col_types = FALSE)
}
