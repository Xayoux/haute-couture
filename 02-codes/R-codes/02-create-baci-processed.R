create_baci_processed <- function(baci, ponderate, years = NULL, codes = NULL, 
                                  method_outliers = 'classic', 
                                  seuil_H_outliers, seuil_L_outliers, 
                                  alpha_H_gamme, seuil_2_HG, path_output, 
                                  return_output, return_pq, remove = TRUE){
  
  if (remove) {
    if (file.exists(path_output)) {
      unlink(path_output, recursive = TRUE)
    }
  }
 
  # BACI sans outlier + calcul de gamme
   baci_mi_processed <- 
     # Suppression des outliers
     analyse.competitivite::clean_uv_outliers(
       baci = baci,
       years = years,
       codes = codes,
       method = method_outliers,
       seuil_H = seuil_H_outliers,
       seuil_L = seuil_L_outliers,
       path_output = NULL,
       return_output = TRUE,
       return_pq = TRUE
     ) |> 
     # Calcul des gammes
     analyse.competitivite::gamme_ijkt_fontagne_1997(
       ponderate = ponderate,
       alpha_H = alpha_H_gamme,
       pivot = "longer",
       path_output = NULL,
       return_output = TRUE,
       return_pq = TRUE
     )
   

   # Définir les produits sur lesquels la France se positionne dans le haut de gamme
   product_HG_france <-
     baci_mi_processed |>
     # Garder uniquement les flux frnaçais de 2010 (2010 = baseline)
     dplyr::filter(
       exporter == "FRA",
       t == 2010
      ) |>
     # Calculer la somme des flux de chaque produit pour chaque gamme
     dplyr::summarize(
       .by = c(k, gamme_fontagne_1997),
       total_v_tikg = sum(v, na.rm = TRUE)
     ) |>
     dplyr::collect() |>
     # Calculer la part que représente chaque gamme par produit
     dplyr::mutate(
       .by = k,
       share_total_v_gamme_tikg = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
     ) |>
     # Garder uniquement les produits dont la gamme H est supérieure au seuil
     dplyr::filter(
       gamme_fontagne_1997 == "H",
       share_total_v_gamme_tikg >= seuil_2_HG
     ) |>
     # Renvoyer un vecteur avec les codes produits uniquement
     dplyr::arrange(k) |> 
     dplyr::pull(k) |>
     unique()
   
   # # Enregistrer les produits sélectionnés
   # if (return_output) {
   #   writexl::write_xlsx(dplyr::as_tibble(product_HG_france), here::here(path_df_analyse_folder, "02-product_HG_france.xlsx"))
   # }
   
   
   # Sélectionner les concurrents
   concurrents_vector <- 
     baci_mi_processed |> 
     dplyr::filter(
       t == 2010
      ) |> 
     dplyr::summarize(
       .by = c(i, k, gamme_fontagne_1997),
       total_v_tikg  = sum(v, na.rm = TRUE)
      ) |> 
     dplyr::collect() |> 
     dplyr::mutate(
       .by = c(i, k),
       share_total_v_gamme_tikg = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
     ) |> 
     dplyr::filter(
       gamme_fontagne_1997 == "H"
     ) |> 
     # calculer part de marché de chaque exportateur dans le haut de gamme
     dplyr::mutate(
       .by = c(k),
       market_share_HG = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
     ) |> 
     dplyr::filter(
       (share_total_v_gamme_tikg >= seuil_2_HG & market_share_HG >= 0.05) |
         (market_share_HG >= 0.1),
       i != "FRA"
     ) |> 
     select(i, k, share_total_v_gamme_tikg, market_share_HG) |> 
     arrange(k, market_share_HG)
   
   
   # Rengistrer les produits et concurrents sélectionnés
   list_k_concu <- 
     list(
       "product_HG_france" = dplyr::as_tibble(product_HG_france),
       "concurrents_vector" = concurrents_vector
     ) |> 
     writexl::write_xlsx(here::here(path_df_analyse_folder, "02-list_k_concu.xlsx"))
   
   
   
   
   
   
   # Finalisation de la base BACI
   df_baci <-
     baci_mi_processed |>
     # Garder uniquement les produits sélectionnés à partir de la France
     # Garder uniquement les flux haut de gamme de ces produits
     dplyr::filter(
       k %in% product_HG_france,
       gamme_fontagne_1997 == "H"
     ) |>
     dplyr::select(!c(alpha_H, alpha_L, med_ref_t_k)) |>
     analyse.competitivite::add_chelem_classification(
       path_output = path_output,
       return_output = return_output,
       return_pq = return_pq
     )

   return(df_baci)
}