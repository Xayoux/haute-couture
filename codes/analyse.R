#  ------------------------------------------------------------------------
#
# Title : Etude de la compétitivité des produits français de la haute couture
#    By : Romain CAPLIEZ...
#  Date : 2024-03-18
#
#  ------------------------------------------------------------------------


# Importer les librairies utilisées ---------------------------------------

# Tester si devtools est installé. Si ce n'est pas le cas, l'installer
# Permet d'installer le package concordance de github pour pouvoir effectuer 
# Des correspondances avec la dernière version de la nomenclature.
if(!require(devtools)) install.packages("devtools")
if(!require(concordance)) install_github("insongkim/concordance", dependencies=TRUE)
library(tidyverse)
library(here)
library(analyse.competitivite)
library(readxl)
library(tictoc)
library(arrow)
library(xtable)
library(patchwork)
library(writexl)
library(openxlsx)


# Créer la liste des produits à utiliser ----------------------------------

# Vecteur contenant les numéros des chapitres / des sous-sections de la nomenclature pour les produits voulus
chapter_codes <- c(4202, 4203, 61, 62, 64, 6504, 6505, 6506, 7113, 7114, 7116, 7117)

# Créer un dataframe contenant les correspondances entre les codes produits de la nomenclature HS22 et HS92
df_product <- 
  extract_product(
    codes_vector = chapter_codes,
    path_output = here("processed-data", "codes-produits.xlsx"),
    revision_origin = "HS22",
    revision_destination = "HS92",
    export = TRUE,
    return_df = TRUE,
    correspondance = TRUE
  )

remove(chapter_codes)

# Créer la base de données BACI ------------------------------------------
# dl_baci(
#   dl_folder = here("..", "BACI2"),rm_csv = FALSE
# )


# Définition des produits de luxe -----------------------------------------

# Regarder la concu et le nb de produits pour différents seuils -----------
# Créer le dataframe contenant le calcul des gammes selon la méthode de Fontagné (1997)
# Seuil à 1.5 -> 50% supérieur à la médianne mondiale pondérée et 2.5
# Année de référence : 2010

# Définition de plusieurs seuils différents
seuils <- c(0.15, 0.25, 0.5, 0.75, 1, 1.5, 2)

# Calcul des gammes pour chaque seuil pour l'année 2010
gamme_ijkt_fontagne_1997(
  path_baci_parquet = here("..", "BACI2", "BACI-parquet"),
  alpha_H = seuils,
  years = c(2010),
  codes = unique(df_product$HS92),
  return_output = TRUE,
  path_output = here("processed-data", "BACI-gamme"),
  remove = TRUE
) 

# Concurrents et produits pour chaque seuil de haut de gamme
concu_explo_function <- function(alpha){
  # Dataframe des gammes pour le seuil alpha
  df_gammes <- 
    # Ouvre le dataset des gammes (pas en mémoire)
    here("processed-data", "BACI-gamme") |> 
    open_dataset() |> 
    # somme les valeurs de commerce pour chaque produit de chaque pays selon les différentes gammes (H, L, M)
    # Permet de connaître la valeur du commerce de chaque gamme de chaque produit par pays
    summarize(
      .by = c(t, exporter, k, !!sym(paste0("gamme_fontagne_1997_", alpha + 1))),
      !!paste0("total_v_gamme_tik_", alpha + 1) := sum(v, na.rm = TRUE)
    ) |> 
    # Trie les données
    arrange(t, exporter, k, !!sym(paste0("gamme_fontagne_1997_", alpha + 1))) |> 
    collect() |> 
    # Ajoute la part de chaque gamme dans le commerce total du produit pour chaque pays
    # Permet de savoir la part que le haut de gamme représente dans le commerce total du produit pour chaque pays
    mutate(
      .by = c(t, exporter, k),
      !!paste0("share_total_v_gamme_tik_", alpha + 1) := 
        !!sym(paste0("total_v_gamme_tik_", alpha + 1)) / sum(!!sym(paste0("total_v_gamme_tik_", alpha + 1)))
    )
  
  
  # Dataframe comprenant uniquement les produits haut de gamme français
  # Si plus de 50% de la valeur commerciale du produit est considérée comme H avec le seuil choisi
  df_products_luxes_fr <- 
    df_gammes |> 
    filter(
      t == 2010,
      exporter == "FRA",
      !!sym(paste0("gamme_fontagne_1997_", 1 + alpha)) == "H",
      !!sym(paste0("share_total_v_gamme_tik_", 1 + alpha)) >= 0.5
    ) |> 
    select(t, k, !!sym(paste0("share_total_v_gamme_tik_", 1 + alpha))) 
  
  
  # Dataframe répertoriant chaque concurrent sur chaque produit retenu pour la France
  df_concu_luxe <- 
    df_gammes |> 
    # Garde uniquement les produits de luxe fr et les concurrents qui répondent aux critères
    filter(
      t == 2010, 
      k %in% unique(df_products_luxes_fr$k),
      !!sym(paste0("gamme_fontagne_1997_", 1 + alpha)) == "H",
      !!sym(paste0("share_total_v_gamme_tik_", 1 + alpha)) >= 0.5
    ) |> 
    # Calcul la part de marché de chaque concurrent sur chaque produit
    mutate(
      .by = c(k),
      !!paste0("market_share_", 1 + alpha) := 
        !!sym(paste0("total_v_gamme_tik_", 1 + alpha)) / sum(!!sym(paste0("total_v_gamme_tik_", 1 + alpha)), na.rm = TRUE)
    ) |> 
    # Garder uniquement les concurrents qui ont plus de 5% de part de marché
    filter(
      !!sym(paste0("market_share_", 1 + alpha)) >= 0.05
    ) |> 
    # Garder uniquement les variables d'intérêt
    select(k, exporter, 
           !!sym(paste0("share_total_v_gamme_tik_", 1 + alpha)), 
           !!sym(paste0("market_share_", 1 + alpha))) |> 
    # Trier les données
    arrange(k, desc(!!sym(paste0("market_share_", 1 + alpha)))) |> 
    # Ajouter la description des produits HS6 retenus
    left_join(
      df_product |> 
        select(HS92, description_HS92),
      by = c("k" = "HS92")
    ) 
  
  # Extraire la liste des concurrents uniques
  vector_concu <- 
    df_concu_luxe |> 
    pull(exporter) |> 
    unique()
  
  # Calculer le nombre de concurrents par produit
  nb_concu_product <-
    df_concu_luxe |> 
    summarize(
      .by = k, 
      nb_concu = n()
    )
  
  # Retourner pour chque seuils ces éléments dans une liste
  return(list(df_concu_luxe, nb_concu_product, vector_concu))
}


# Exécuter la fonction pour chaque seuil
liste <- 
  seuils |> 
  map(concu_explo_function)


# Créer un classeur excel pour stocker les résultats
{wb <- createWorkbook()  
  # Pour chaque élément de la liste (chaque seuil)
  for (i in seq_along(liste)) {
    
    # Nommer la feuille en fonction du seuil
    sheet_name <- paste("Seuil", 1 + seuils[i], sep = "_")
    
    # Ajouter une nouvelle feuille
    addWorksheet(wb, sheetName = sheet_name)  
    
    # Pour chaque dataframe dans l'élément actuel de la liste
    for (j in seq_along(liste[[i]])) {
      start_col <- c(1, 7, 10)[j]  # Définit le point de départ de la colonne dans la feuille excel
      
      writeData(wb, sheet = sheet_name, x = liste[[i]][[j]], startCol = start_col)  # Écrit les données dans la feuille
    }
  }
  # si concurrent.xlsx existe supprimer le fichier
  if(file.exists(here("processed-data", "concurrents.xlsx"))){
    file.remove(here("processed-data", "concurrents.xlsx"))
  }
  
  # Sauvegarder le fichier
  saveWorkbook(wb, file = here("processed-data", "concurrents.xlsx"))
  
  # Supprimer les variables inutiles dans l'environnement
  remove(liste, i, j, seuils, sheet_name, start_col, wb, concu_explo_function)
}
















# Mettre au propre --------------------------------------------------------


# Pays concurrents : chapitres agrégés ------------------------------------

# gamme_ijkt_fontagne_1997(
#   path_baci_parquet = here("..", "BACI2", "BACI-parquet"),
#   alpha_H = 1.5,
#   years = c(2010),
#   codes = unique(df_product$HS92),
#   return_output = FALSE,
#   path_output = here("processed-data", "BACI-gamme")
# ) 
# 
# df_gammes <- 
#   here("processed-data", "BACI-gamme") |> 
#   open_dataset() |> 
#   summarize(
#     .by = c(t, exporter, k, gamme_fontagne_1997),
#     total_v_tik = sum(v, na.rm = TRUE)
#   ) |> 
#   arrange(t, exporter, k, gamme_fontagne_1997) |> 
#   collect() |> 
#   mutate(
#     .by = c(t, exporter, k),
#     share_total_v_gamme = total_v_tik / sum(total_v_tik)
#   )
# 
# products_luxes_fr <- 
#   df_gammes |> 
#   filter(
#     t == 2010,
#     exporter == "FRA",
#     gamme_fontagne_1997 == "H",
#     share_total_v_gamme >= 0.5
#   ) |> 
#   select(t, k, share_total_v_gamme) 
# 
# 
# concu <- 
#   df_gammes |> 
#   filter(
#     t == 2010, 
#     k %in% unique(products_luxes_fr$k)
#   ) |> 
#   mutate(
#     chapter = substr(k, 1, 2),
#   ) |> 
#   summarize(
#     .by = c(exporter, chapter, gamme_fontagne_1997),
#     total_v_tik = sum(total_v_tik, na.rm = TRUE)
#   ) |> 
#   mutate(
#     .by = c(exporter, chapter),
#     share_total_v_gamme = total_v_tik / sum(total_v_tik, na.rm = TRUE)
#   ) |> 
#   filter(
#     gamme_fontagne_1997 == "H",
#     share_total_v_gamme >= 0.5
#   ) |> 
#   mutate(
#     .by = chapter,
#     market_share = total_v_tik / sum(total_v_tik, na.rm = TRUE)
#   ) |> 
#   filter(market_share >= 0.05) |> 
#   arrange(chapter, desc(market_share)) |> 
#   pull(exporter) |> 
#   unique()




























