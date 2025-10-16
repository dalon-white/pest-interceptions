# get start and end dates from params
get_date_params <- function(params) {
  begin_date <- if(is.na(params$begin_date)) as.Date("1900-01-01") else as.Date(params$begin_date, "%d-%m-%Y")
  end_date <- if(is.na(params$end_date)) Sys.Date() else as.Date(params$end_date, "%d-%m-%Y")
  return(list(begin_date = begin_date, end_date = end_date))
}

# Filter dates
# Apply date filters only when needed
filter_dates <- function(data, begin_date = NULL, end_date = NULL) {
  if (!is.null(begin_date)) {
    data <- data |> 
      filter(INSPECTION_DATE >= begin_date)
  }

  if (!is.null(end_date)) {
    data <- data |> 
      filter(INSPECTION_DATE <= end_date)
  }

  return(data)
}