#' Build a request-specific report directory name
#'
#' @param params The R Markdown params list used for filtering the request
#'
#' @return A directory name describing the search criteria
build_interception_dir_name <- function(params) {
  paste0(
    if (!is.null(params$pest_nickname)) paste(params$pest_nickname, collapse = "_") else "all pests",
    "_on_",
    if (!is.null(params$commodity_common_name)) {
      paste(params$commodity_common_name)
    } else if (!is.null(params$commodity_taxonomic_name)) {
      paste(params$commodity_taxonomic_name, collapse = "_")
    } else {
      "anything"
    },
    "_from_",
    if (!is.null(params$origin_country)) paste(params$origin_country, collapse = "_") else "anywhere",
    "_from_period_",
    if (!is.null(params$begin_date)) params$begin_date else "anytime",
    "_to_",
    if (!is.null(params$end_date)) params$end_date else "anytime"
  )
}

#' Build standard interception output paths
#'
#' @param params The R Markdown params list used for filtering the request
#' @param request_date The request date as YYYYMMDD
#' @param output_root The root folder for generated report artifacts
#'
#' @return A list containing the output directory and standard artifact paths
build_interception_output_paths <- function(params,
                                            request_date,
                                            output_root = here::here("reports")) {
  dir_name <- build_interception_dir_name(params)
  output_dir <- file.path(output_root, dir_name)
  artifact_stem <- paste0(request_date, "_interception")

  list(
    dir_name = dir_name,
    output_dir = output_dir,
    artifact_stem = artifact_stem,
    data_csv = file.path(output_dir, paste0(artifact_stem, "_data.csv")),
    data_html = file.path(output_dir, paste0(artifact_stem, "_data.html")),
    summary_csv = file.path(output_dir, paste0(artifact_stem, "_summary.csv")),
    graph_png = file.path(output_dir, paste0(artifact_stem, "_graph.png")),
    report_html = file.path(output_dir, paste0(artifact_stem, "_report.html"))
  )
}