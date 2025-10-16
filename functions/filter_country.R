#' @param params YAML header parameter, if given
filter_country <- function(params) {
  df_diagnostic_results <- df_diagnostic_results |> 
    filter(ORIGIN %in% params$origin_country)
}