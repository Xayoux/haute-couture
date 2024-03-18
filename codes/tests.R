library(here)
library(tidyverse)
library(arrow)
library(tictoc)
library(readxl)

path <- "d:/capliez/Documents/BACI/BACI_HS92_V202401/BACI_HS92_Y2022_V202401.csv"

df_country_codes <- "d:/capliez/Documents/BACI/BACI_HS92_V202401/country_codes_V202401.csv" |> 
  read_csv() |> 
  select(country_code, country_iso3)

df_product <- here("processed-data","codes-produits.xlsx") |> 
  read_excel()

codes <- unlist(unique(df_product$HS92))

df_baci <- 
  path |> 
  read_csv_arrow(as_data_frame = FALSE) |> 
  filter(k %in% codes) |> 
  mutate(i = as.double(i), 
         j = as.double(j)) |> 
  left_join(df_country_codes, by = c("i" = "country_code")) |> 
  rename(exporter = country_iso3) |> 
  left_join(df_country_codes, by = c("j" = "country_code")) |>
  rename(importer = country_iso3) |>
  collect()

df3 <- 
  here("..", "BACI", "BACI_HS92_V202401") |> 
  list.files(full.names = TRUE, pattern = "^BACI") |>
  open_dataset(format = "csv") |>
  filter(i == 4) |> 
  collect()






