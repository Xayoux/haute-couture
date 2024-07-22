# Script pour convertir les données csv de MacMap envoyées par Houssein en
# Données parquet pour permettre leur commit et push dans github.

# Créer scéhma pour le type des données
schema_mm <-
    arrow::schema(
      arrow::Field$create("importer", type = arrow::string()),
      arrow::Field$create("hs6", type = arrow::string()),
      arrow::Field$create("exporter", type = arrow::string()),
      arrow::Field$create("ave_pref_applied", type = arrow::float64())
    )


# Importer les données csv
here::here(
    "01-raw-data",
    "MacMap",
    "mmhs62019.csv"
) |>
  arrow::read_csv_arrow(schema = schema_mm, skip = 1, as_data_frame = FALSE) |>
  # Les convertir en parquet groupés par HS6
  dplyr::group_by(hs6) |>
  arrow::write_dataset(
    here::here(
      "01-raw-data",
      "MacMap",
      "MacMap-pq"
    )
  )

