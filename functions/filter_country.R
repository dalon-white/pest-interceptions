#' @param params YAML header parameter, if given
filter_country <- function(data, params) {
  if (is.null(params$origin_country)) {
    return(data)
  } else {
    data <- data |> 
    filter(ORIGIN %in% params$origin_country)
    return(data)
  }
}