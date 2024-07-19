# Script pour convertir les données csv de MacMap envoyées par Houssein en
# Données parquet pour permettre leur commit et push dans github.

# Importer les données csv
here::here(
    "01-raw-data",
    "MacMap",
    "mmhs62019.csv"
) |>
  arrow::read_csv_arrow() |>
  # Les convertir en parquet groupés par HS6
  dplyr::group_by(hs6) |>
  arrow::write_dataset(
    here::here(
      "01-raw-data",
      "MacMap",
      "MacMap-pq"
    )
  )

