create_baci_processed <- function(baci, years = NULL, codes = NULL, 
                                  method_outliers = 'classic', 
                                  seuil_H_outliers, seuil_L_outliers, 
                                  alpha_H_gamme, seuil_2_HG, path_output, 
                                  return_output, return_pq, remove = TRUE){
  
  if (remove) {
    if (file.exists(path_output)) {
      unlink(path_output, recursive = TRUE)
    }
  }
 
   baci_mi_processed <- 
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
     analyse.competitivite::gamme_ijkt_fontagne_1997(
       alpha_H = alpha_H_gamme,
       pivot = "longer",
       path_output = NULL,
       return_output = TRUE,
       return_pq = TRUE
     )

   product_HG_france <-
     baci_mi_processed |>
     dplyr::filter(
       exporter == "FRA",
       t == 2010
      ) |>
     dplyr::summarize(
       .by = c(k, gamme_fontagne_1997),
       total_v_tikg = sum(v, na.rm = TRUE)
     ) |>
     dplyr::collect() |>
     dplyr::mutate(
       .by = k,
       share_total_v_gamme_tikg = total_v_tikg / sum(total_v_tikg, na.rm = TRUE)
     ) |>
     dplyr::filter(
       gamme_fontagne_1997 == "H",
       share_total_v_gamme_tikg >= seuil_2_HG
     ) |>
     dplyr::pull(k) |>
     unique()



   df_baci <-
     baci_mi_processed |>
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