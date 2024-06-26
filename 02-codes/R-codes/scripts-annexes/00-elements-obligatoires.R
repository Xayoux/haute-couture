# Packages nécessaires ----------------------------------------------------

# Tester si devtools est installé. Si ce n'est pas le cas, l'installer
# Permet d'installer le package concordance de github pour pouvoir effectuer 
# Des correspondances avec la dernière version de la nomenclature.
# Si concordance est déjà installé à partir de CRAN, le désinstaller, fermer R, puis réinstaller avec la commande ci-dessous
## if(!require(devtools)) install.packages("devtools")
## if(!require(analyse.competitivite)) install_github("Xayoux/analyse.competitivite", dependencies=TRUE)
## if(!require(concordance)) install_github("insongkim/concordance", dependencies=TRUE)
## if(!require(here)) install.packages("here")
## if(!require(readxl)) install.packages("readxl")
## if(!require(arrow)) install.packages("arrow")
## if(!require(writexl)) install.packages("writexl")
## if(!require(openxlsx)) install.packages("openxlsx")
## if(!require(tidyverse)) install.packages("tidyverse")
## if(!require(scales)) install.packages("scales")
## if(!require(xtable)) install.packages("xtable")
## if(!require(janitor)) install.packages("janitor")
## if(!require(fixest)) install.packages("fixest")
## if(!require(wbstats)) install.packages("wbstats")

source(
  here(
    "02-codes", 
    "R-codes",
    "scripts-annexes",
    "load-packages.R"
  )
)


# Options -----------------------------------------------------------------
# Enlever l'écriture scientifique
options(scipen = 999)


# Arborescence ------------------------------------------------------------
# Créer l'arborescence des dossiers utilisés pour les différents outputs
# Importer les variables pour les chemins d'accès
source(
  here(
    "02-codes", 
    "R-codes",
    "scripts-annexes",
    "creation-folder-arborescence.R"
  )
)


# Liste produits / concurrents --------------------------------------------
# Importer la liste des produits initiaux si elle existe déjà
if(file.exists(here(path_df_folder, "01-codes-produits.xlsx"))){
  df_product <-
    here(path_df_folder, "01-codes-produits.xlsx") |> 
    read_excel()
}

# Importer la liste des produits haut de gamme si elle existe déjà
if(file.exists(here(path_df_folder, "02-list_k_concu.xlsx"))){
  df_products_HG <- 
    here(path_df_folder, "02-list_k_concu.xlsx") |>
    read_xlsx(sheet = "product_HG_france")
}

# Importer la liste des concurrents si elle existe déjà
if(file.exists(here(path_df_folder, "02-list_k_concu.xlsx"))){
  df_concurrents_HG <- 
    here(path_df_folder, "02-list_k_concu.xlsx") |>
    read_xlsx(sheet = "sector_concurrents") |> 
    select(exporter, sector) |> 
    distinct()
}


# Définition de l'ordre/couleurs des pays pour les graphs -----------------
source(
  here(
    "02-codes",
    "R-codes",
    "scripts-annexes",
    "ordre-couleurs-pays.R"
  )
)


# Créer ou load le fichier de stockage des résultats ----------------------
if(file.exists(path_excel_results)){
  wb_results <- openxlsx::loadWorkbook(path_excel_results)
} else {
  wb_results <- openxlsx::createWorkbook()
  openxlsx::saveWorkbook(wb_results, path_excel_results)
}


# Load les différentes databases en format parquet ------------------------
source(
  here(
    "02-codes",
    "R-codes",
    "scripts-annexes",
    "load-databases.R"
  )
)


# Load les différents dataframes ------------------------------------------
source(
  here(
    "02-codes",
    "R-codes",
    "scripts-annexes",
    "load-dataframes.R"
  )
)
