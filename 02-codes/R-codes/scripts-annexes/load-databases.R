# Load les différentes database en format parquet -------------------------
## Charger la base baci_mi_brute ------------------------------------------
if (dir.exists(path_baci_mi_brute)){
  df_baci_mi_brute <-
    path_baci_mi_brute |>
    arrow::open_dataset()
}


## Charger la base baci_processed -----------------------------------------
if (dir.exists(path_baci_processed)){
  df_baci_processed <-
    path_baci_processed |>
    arrow::open_dataset()
}


## Charger la base baci_total ---------------------------------------------
if (dir.exists(path_baci_total)){
  df_baci_total <-
    path_baci_total |>
    arrow::open_dataset()
}


## Charger la base gravity_khandelwal -------------------------------------
if (dir.exists(path_gravity_khandelwal)){
  df_gravity_khandelwal <-
    path_gravity_khandelwal |>
    arrow::open_dataset()
}


## Charger la base quality_khandelwal -------------------------------------
if (dir.exists(path_quality_khandelwal)){
  df_quality_khandelwal <-
    path_quality_khandelwal |>
    arrow::open_dataset()
}


## Charger la base MAcMap brute
if (dir.exists(path_MacMap_pq)){
  schema_mm <-
    arrow::schema(
      arrow::Field$create("importer", type = arrow::string()),
      arrow::Field$create("exporter", type = arrow::string()),
      arrow::Field$create("ave_pref_applied", type = arrow::float64()),
      arrow::Field$create("hs6", type = arrow::string())
    )
  
  df_MacMap_brute <-
    path_MacMap_pq |>
    arrow::open_dataset(schema = schema_mm)
}
