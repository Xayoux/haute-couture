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

# Créer la base de données BACI ------------------------------------------
# dl_baci(
#   dl_folder = here("..", "BACI2"),
# )

# Définition des produits de luxe -----------------------------------------

# Gammes des marchés : méthode de Fontagné et al (1997)
liste_df <- list()
alpha_vector <- c(0.15, 0.25, 0.50, 0.75, 1, 1.5)
compteur <- 1
for (i in alpha_vector){
  gamme_ijkt_fontagne_1997(
    path_baci_parquet = here("..", "BACI2", "BACI-parquet"),
    alpha_H = i,
    years = c(2010,2022),
    codes = unique(df_product$HS92),
    return_output = FALSE,
    path_output = here("processed-data", "BACI-gamme")
  ) 
  
  df_gammes <- 
    here("processed-data", "BACI-gamme") |> 
    open_dataset() |> 
    summarize(
      .by = c(t, exporter, k, gamme_fontagne_1997),
      total_v_tik = sum(v, na.rm = TRUE)
    ) |> 
    arrange(t, exporter, k, gamme_fontagne_1997) |> 
    collect() |> 
    mutate(
      .by = c(t, exporter, k),
      share_total_v_gamme = total_v_tik / sum(total_v_tik)
    )
  
  products_luxes_fr <- 
    df_gammes |> 
    filter(
      t %in% c(2010,2022),
      exporter == "FRA",
      gamme_fontagne_1997 == "H",
      share_total_v_gamme >= 0.5
    ) |> 
    select(t, k, share_total_v_gamme) 
  
  nb_concu <- 
    df_gammes |>
    filter(
      t %in% c(2010,2022),
      share_total_v_gamme >= 0.5,
      k %in% unique(products_luxes_fr$k),
      gamme_fontagne_1997 == "H",
      exporter != "FRA"
    ) |> 
    mutate(
      .by = c(t,k),
      mean_v = mean(total_v_tik, na.rm = TRUE),
      sup_mean = (total_v_tik >= mean_v)
    ) |>
    summarize(
      .by = c(t,k),
      n_concu = n(),
      mean_v = mean(total_v_tik, na.rm = TRUE),
      median_v = median(total_v_tik, na.rm = TRUE),
      q_60_v = quantile(total_v_tik, 0.6, na.rm = TRUE),
      q_70_v = quantile(total_v_tik, 0.7, na.rm = TRUE),
      p_80_v = quantile(total_v_tik, 0.8, na.rm = TRUE),
      p_90_v = quantile(total_v_tik, 0.9, na.rm = TRUE),
      n_concu_sup_mean = sum(sup_mean, na.rm = TRUE)
    ) |> 
    arrange(t, k)
  
  liste_df[[compteur]] <-
    products_luxes_fr |> 
    left_join(
      nb_concu,
      by = c("k", "t")
    ) |> 
    mutate(
      k_chapter = substr(k, 1, 2)
    ) |> 
    summarize(
      .by = c(t, k_chapter),
      nb_concu_mean = mean(n_concu, na.rm = TRUE),
      nb_concu_median = median(n_concu, na.rm = TRUE),
      nb_concu_sup_mean_mean = mean(n_concu_sup_mean, na.rm = TRUE),
      mean_v_mean = mean(mean_v, na.rm = TRUE),
      median_v_mean = mean(median_v, na.rm = TRUE),
      q_60_v_mean = mean(q_60_v, na.rm = TRUE),
      q_70_v_mean = mean(q_70_v, na.rm = TRUE),
      p_80_v_mean = mean(p_80_v, na.rm = TRUE),
      p_90_v_mean = mean(p_90_v, na.rm = TRUE),
      nb_products = n()
    ) |> 
    mutate(
      seuil = 1 + i
    )
  
  df_add <-
    liste_df[[compteur]] |> 
    summarize(
      .by = c(t,seuil),
      k_chapter = "total",
      nb_concu_mean = mean(nb_concu_mean, na.rm = TRUE),
      nb_concu_median = mean(nb_concu_median, na.rm = TRUE),
      nb_concu_sup_mean_mean = mean(nb_concu_sup_mean_mean, na.rm = TRUE),
      mean_v_mean = mean(mean_v_mean, na.rm = TRUE),
      median_v_mean = mean(median_v_mean, na.rm = TRUE),
      q_60_v_mean = mean(q_60_v_mean, na.rm = TRUE),
      q_70_v_mean = mean(q_70_v_mean, na.rm = TRUE),
      p_80_v_mean = mean(p_80_v_mean, na.rm = TRUE),
      p_90_v_mean = mean(p_90_v_mean, na.rm = TRUE),
      nb_products = sum(nb_products)
    )
  
  liste_df[[compteur]] <-
    liste_df[[compteur]] |> 
    bind_rows(df_add)
  
  compteur <- compteur + 1
}

df <- 
  liste_df |> 
  list_rbind()


g_nb_concu <- 
  df |> 
  relocate(seuil) |> 
  ggplot(
    aes(x = as.character(seuil), y = nb_concu_sup_mean_mean, color = k_chapter)
  ) + 
  geom_point()+
  geom_line(aes(group = k_chapter), linewidth = 1)+
  # scale_color_brewer(palette = "Paired")+
  scale_color_manual(values = c("#008270", "#3AB0AA", "#B4D3CE", "#A5A5A5",
                                "#8C4648", "#C29B9C", "black"), labels = c("total" = "moyenne"))+
  facet_grid(.~t) +
  labs(
    title = "Nombre de concurrents en moyenne par catégorie de produits selon des seuils différents",
    subtitle = "Un produit est haut de gamme si plus de 50% de sa valeur est classée haut de gamme",
    x = "Seuil",
    y = "Nombre de concurrents en moyenne",
    color = "Produits"
  ) +
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
  ) 

g_nb_product <- 
  df |> 
  relocate(seuil, t, k_chapter, nb_products) |> 
  ggplot(
    aes(x = as.character(seuil), y = nb_products, color = k_chapter)
  ) + 
  geom_point()+
  geom_line(aes(group = k_chapter), linewidth = 1)+
  # scale_color_brewer(palette = "Paired")+
  scale_color_manual(values = c("#008270", "#3AB0AA", "#B4D3CE", "#A5A5A5",
                                "#8C4648", "#C29B9C", "black"))+
  facet_grid(.~t) +
  labs(
    title = "Nombre de produits par catégorie selon différents seuils",
    subtitle = "Un produit est haut de gamme si plus de 50% de sa valeur est classée haut de gamme",
    x = "Seuil",
    y = "Nombre de produits",
    color = "Produits"
  ) +
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
  )

g_merge <- g_nb_concu / g_nb_product
g_merge

































