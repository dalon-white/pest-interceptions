#' Run if commodity filters are provided
#' @param params YAML header parameter, if given
#' @param data Data frame to filter

filter_commodity <- function(params, data){
# Only apply filtering if at least one parameter is not NULL
if (!is.null(params$commodity_common_name) || !is.null(params$commodity_taxonomic_name)) {
  # Create patterns only from non-NULL parameters
  if (!is.null(params$commodity_common_name)) {
    common_pattern <- paste(params$commodity_common_name, collapse = "|")
    has_common_pattern <- TRUE
  } else {
    has_common_pattern <- FALSE
  }

  if (!is.null(params$commodity_taxonomic_name)) {
    taxonomic_pattern <- paste(params$commodity_taxonomic_name, collapse = "|")
    has_taxonomic_pattern <- TRUE
  } else {
    has_taxonomic_pattern <- FALSE
  }
  
  # Build the filter dynamically based on available patterns
  if (has_common_pattern && has_taxonomic_pattern) {
    data <- data |> filter(
      str_detect(COMMODITY_DISPLAY_NAME, common_pattern) |
      str_detect(COMMODITY_TAXONOMIC_DISPLAY_NAME, taxonomic_pattern)
    )
  } else if (has_common_pattern) {
    data <- data |> filter(
      str_detect(COMMODITY_DISPLAY_NAME, common_pattern)
    )
  } else if (has_taxonomic_pattern) {
    data <- data |> filter(
      str_detect(COMMODITY_TAXONOMIC_DISPLAY_NAME, taxonomic_pattern)
    )
  }
}
return(data)
}