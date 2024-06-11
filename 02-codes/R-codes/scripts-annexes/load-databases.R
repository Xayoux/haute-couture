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
