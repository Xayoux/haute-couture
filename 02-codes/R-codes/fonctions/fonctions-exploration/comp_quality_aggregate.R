#' @title
#' Comparer les méthodes d'aggrégation de la compétitivité hors-prix
#'
#' @description
#' Permet de comparer dans un unique dataframe les résultats de
#' l'aggrégation de la mesure de la compétitivité hors-prix selon
#' différentes méthodes et variables de pondérations.
#'
#' Les méthodes sont : mean, median, weighted.mean, weighted.median
#'
#' 
#' @param data Les données à utiliser
#' @param method_aggregate_vector Vecteur contenant les méthodes à utiliser
#' en chaînes de caractères
#' @param weighted_var_vector Les variables à utiliser pour pondérer si
#' nécessaire. Si la méthode ne nécessite pas de pondération, mettre "NULL".
#' La longueur de ce vecteur doit être la même que pour la variable
#' method_aggregate_vector
#' @param fixed_weight Booléen indiquant si les poids doivent être fixés à
#' une année
#' @param year_ref Année sur laquelle les poids doivent être fixés
#' @param year_display Année(s) à afficher dans le tableau de résultat
#' @param path_output Chemin d'accès au fichier pour enregistrer les résultats
#' @return 
comp_quality_aggregate <- function(data, method_aggregate_vector,
                                   weighted_var_vector, fixed_weight, year_ref,
                                   year_display, path_output = NULL){

  # Créer la variable qui va permettre de nommer les différentes
  # Méthodes utilisées
  name_weighted <- c("1")

  for (i in 1:length(weighted_var_vector)){
    name_weighted[i] <- dplyr::if_else(weighted_var_vector[i] == "NULL",
                                      "", stringr::str_glue("_{weighted_var_vector[i]}"))
  }

  variable_names <- as.vector(
    stringr::str_glue("{method_aggregate_vector}{name_weighted}")
  )
  
  # Définir une fonction qui aggrège la qualkité selon une méthode précise
  agg_quality_loop_func <- function(data, aggregate_method, var_weighted,
                                    fixed_weight, year_ref, year_display,
                                    variable_name){
    
    df <-
      data |>
      dplyr::collect() |>
      analyse.competitivite::quality_aggregate(
        var_aggregate_k = "sector",
        var_aggregate_i = "gamme_fontagne_1997",
        method_aggregate = aggregate_method,
        weighted_var = ifelse(var_weighted == "NULL", "q", var_weighted),
        fixed_weight = fixed_weight,
        year_ref = year_ref,
        print_output = FALSE,
        return_output = TRUE
      ) |>
      dplyr::arrange(t, sector, dplyr::desc(quality)) |>
      dplyr::filter(t %in% year_display) |>
      dplyr::mutate(name = variable_name)
  }


  # Exécuter la fonction pour chaque méthode voulue
  df_res <-
    purrr::pmap(
      list(
        method_aggregate_vector,
        weighted_var_vector,
        variable_names
      ),
    \(method_aggregate, weighted_var, variable_names) agg_quality_loop_func(
      data = data,
      aggregate_method = method_aggregate,
      var_weighted = weighted_var,
      fixed_weight = fixed_weight,
      year_ref = year_ref,
      year_display = year_display,
      variable_name = variable_names
    )
    )  |>
    purrr::list_rbind() |>
    tidyr::pivot_wider(
      names_from = name,
      values_from = quality
    )

  # Enregistrer le résultat
  if (!is.null(path_output)){
    writexl::write_xlsx(df_res, path_output)
  }

  return(df_res)
     
} 

