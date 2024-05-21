#  ------------------------------------------------------------------------
#
# Title : Script contenant tout ce qu'il faut lancer avant l'analyse
#    By : Romain CAPLIEZ...
#  Date : 2024-05-21
#
#  ------------------------------------------------------------------------

# **************************************************************** --------
# Packages nécessaires ----------------------------------------------------

# Tester si devtools est installé. Si ce n'est pas le cas, l'installer
# Permet d'installer le package concordance de github pour pouvoir effectuer 
# Des correspondances avec la dernière version de la nomenclature.
# Si concordance est déjà installé à partir de CRAN, le désinstaller, fermer R, puis réinstaller avec la commande ci-dessous
if(!require(devtools)) install.packages("devtools")
if(!require(analyse.competitivite)) install_github("Xayoux/analyse.competitivite", dependencies=TRUE)
if(!require(concordance)) install_github("insongkim/concordance", dependencies=TRUE)
if(!require(here)) install.packages("here")
if(!require(readxl)) install.packages("readxl")
if(!require(arrow)) install.packages("arrow")
if(!require(writexl)) install.packages("writexl")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(scales)) install.packages("scales")
if(!require(xtable)) install.packages("xtable")
if(!require(janitor)) install.packages("janitor")


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
    "00-creation-arborescence-folder.R"
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


# Définitions paramètres esthétiques des graphs ---------------------------

# Ordre des pays
ordre_pays_exporter <- 
  list(
    general    = c("Reste du monde", "Amérique","Moyen-Orient",
                   "Reste de l'Asie", "Chine et Hong Kong",   
                   "Suisse", "Reste de Union européenne", "Italie", "France"),
    
    bijouterie = c("Reste du monde", "Amérique", "USA", "Moyen-Orient",
                   "Turquie", "Reste de l'Asie", "Chine et Hong Kong",   
                   "Suisse", "Reste de Union européenne", "Italie", "France")
  )

ordre_pays_importer <-
  list(
    general =
      c("Reste du monde", "Amérique", "USA", "Moyen-Orient", "ARE",
        "Reste de l'Asie" , "Japon et Corée", "Chine et Hong Kong",
        "Suisse", "Reste de Union européenne", "Italie", "France"),
    
    bijouterie =
      c("Reste du monde", "Amérique", "USA", "Moyen-Orient", "ARE",
        "Reste de l'Asie" , "Japon et Corée", "Chine et Hong Kong",
        "Suisse", "Reste de Union européenne", "Italie", "France")
  )


# Couleurs des pays
couleurs_pays_exporter <- 
  list(
    general = 
      c(
        "France"                    = "#006CA5",
        "Italie"                    = "#04B2DE",
        "Reste de Union européenne" = "#48CAE4",
        "Suisse"                    = "#90E0EF",
        "Chine et Hong Kong"        = "#ae4d4d",
        "Reste de l'Asie"           = "#F7B4BB",
        "Moyen-Orient"              = "#3AB0AA",
        "Amérique"                  = "#d499ed",
        "Reste du monde"            = "#D9D9D9"
      ),
    
    bijouterie = 
      c(
        "France"                    = "#006CA5",
        "Italie"                    = "#04B2DE",
        "Reste de Union européenne" = "#48CAE4",
        "Suisse"                    = "#90E0EF",
        "Chine et Hong Kong"        = "#ae4d4d",
        "Reste de l'Asie"           = "#F7B4BB",
        "Turquie"                   = "#008270",
        "Moyen-Orient"              = "#3AB0AA",
        "USA"                       = "#7600bc",
        "Amérique"                  = "#d499ed",
        "Reste du monde"            = "#D9D9D9"
      )
  )

couleurs_pays_importer <-
  list(
    general = 
      c(
        "France"                    = "#006CA5",
        "Italie"                    = "#04B2DE",
        "Reste de Union européenne" = "#48CAE4",
        "Suisse"                    = "#90E0EF",
        "Chine et Hong Kong"        = "#ae4d4d",
        "Japon et Corée"            = "#F46D75",
        "Reste de l'Asie"           = "#F7B4BB",
        "ARE"                       = "#008259",
        "Moyen-Orient"              = "#3AB0AA",
        "USA"                       = "#7600bc",
        "Amérique"                  = "#d499ed",
        "Reste du monde"            = "#D9D9D9"
      ),
    
    bijouterie = 
      c(
        "France"                    = "#006CA5",
        "Italie"                    = "#04B2DE",
        "Reste de Union européenne" = "#48CAE4",
        "Suisse"                    = "#90E0EF",
        "Chine et Hong Kong"        = "#ae4d4d",
        "Japon et Corée"            = "#F46D75",
        "Reste de l'Asie"           = "#F7B4BB",
        "ARE"                       = "#008259",
        "Moyen-Orient"              = "#3AB0AA",
        "USA"                       = "#7600bc",
        "Amérique"                  = "#d499ed",
        "Reste du monde"            = "#D9D9D9"
      )
  )


# Type de ligne des pays
linetype_exporter <- 
  list(
    general = c(
      "France"                    = "solid",
      "Italie"                    = "dashed",
      "Reste de Union européenne" = "dotted",
      "Suisse"                    = "longdash",
      "Chine et Hong Kong"        = "solid",
      "Reste de l'Asie"           = "dashed",
      "Moyen-Orient"              = "solid",
      "Amérique"                  = "solid",
      "Reste du monde"            = "solid"
    ),
    bijouterie = c(
      "France"                    = "solid",
      "Italie"                    = "dashed",
      "Reste de Union européenne" = "dotted",
      "Suisse"                    = "longdash",
      "Chine et Hong Kong"        = "solid",
      "Reste de l'Asie"           = "dashed",
      "Turquie"                   = "dashed",
      "Moyen-Orient"              = "solid",
      "USA"                       = "solid",
      "Amérique"                  = "solid",
      "Reste du monde"            = "solid"
    )
  )

