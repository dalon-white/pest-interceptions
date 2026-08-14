parse_request_date <- function(value, default = NULL) {
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return(default)
  }

  if (inherits(value, "Date")) {
    return(value)
  }

  value <- as.character(value)
  if (tolower(value) == "today") {
    return(Sys.Date())
  }

  parsed_date <- as.Date(value, tryFormats = c("%m-%d-%Y", "%Y-%m-%d", "%d-%m-%Y"))
  if (is.na(parsed_date)) {
    stop(paste0("Could not parse date parameter: ", value), call. = FALSE)
  }

  parsed_date
}

# get start and end dates from params
get_date_params <- function(params) {
  begin_date <- parse_request_date(params$begin_date, default = as.Date("1900-01-01"))
  end_date <- parse_request_date(params$end_date, default = Sys.Date())
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

#Wrap your date parameters in !! (bang-bang operator) to force immediate evaluation:
# This prevents dbplyr from translating them to SQL, which then thinks they are a column name. Instead, it forces evaluation in R first, yielding the correct date value to be used in the SQL query.