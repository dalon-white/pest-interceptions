get_determinations <- function(connection) {
  # First, get all columns to avoid SQL errors if column names change
  result <- tbl(connection, sql("SELECT
                       *
                       FROM [PPQ_AQI_ARMDMV2].[ARMDATADM].[SYS2_BRG_DIAGNOSTIC_DETERMINATION]")) |>
    collect()

  # Get the actual column names from the retrieved data
  actual_cols <- colnames(result)

  # Define mandatory columns we need
  base_cols <- c(
    "ID",
    "DIAGNOSTIC_REQUEST_ID",
    "PEST_TAXON_ID",
    "PEST_TAXON_SIMPLE_NAME",
    "PEST_TAXONOMIC_NAME",
    "ID_AUTHORITY",
    "DETERMINATION_TYPE_ID",
    "DETERMINED_BY_GROUP_ID",
    "DETERMINATION_DATETIME",
    "QUARANTINE_STATUS_HAWAII",
    "QUARANTINE_STATUS_PUERTO_RICO"
  )

  # Check which quarantine status columns exist and add them
  quarantine_mainland_col <- NULL
  if ("QUARANTINE_STATUS_MAINLAND" %in% actual_cols) {
    quarantine_mainland_col <- "QUARANTINE_STATUS_MAINLAND"
  } else if ("QUARANTINE_STATUS_CONUS" %in% actual_cols) {
    quarantine_mainland_col <- "QUARANTINE_STATUS_CONUS"
  }

  # Create the final column selection list
  select_cols <- c(base_cols, quarantine_mainland_col)
  select_cols <- select_cols[!is.null(select_cols)]

  # Select the needed columns
  result <- result |>
    dplyr::select(all_of(select_cols)) |>
    rename("DIAGNOSTIC_DETERMINATION_ID" = "ID")

  # If needed, standardize column names for downstream code
  if (!is.null(quarantine_mainland_col) && quarantine_mainland_col == "QUARANTINE_STATUS_MAINLAND") {
    result <- result |> rename("QUARANTINE_STATUS_CONUS" = "QUARANTINE_STATUS_MAINLAND")
  }

  return(result)
}




#pull BRG_DIAGNOSTIC_DETERMINATION_NOT_POSSIBLE_REASON table 

## Just done to attach DIAGNOSTIC_NOT_POSSIBLE_FLAG in the event that a later molecular analysis was done but failed at a better determination (per Andy Carmichael's instructions, 8/12/24 on Teams)
get_diag_determ_not_possible <- function(connection, data){
  tbl(connection, sql("SELECT
                      *
                      FROM [PPQ_AQI_ARMDMV2].[ARMDATADM].[SYS2_BRG_DIAGNOSTIC_DETERMINATION_NOT_POSSIBLE_REASON]")) |>
    collect() |> 
    dplyr::select(ID,
                  DIAGNOSTIC_DETERMINATION_ID,
                  DIAGNOSTIC_DETERMINATION_NOT_POSSIBLE_REASON)
}



# ---- bring these together into a single function ----
get_diagnostic_results <- function(connection, data) {

  diag_determ_records <- get_determinations(connection)
  determ_not_possible_data <- get_diag_determ_not_possible(connection)

  diag_determ_records <- diag_determ_records |>
    filter(DIAGNOSTIC_DETERMINATION_ID %in% (
      data |> pull(DIAGNOSTIC_DETERMINATION_ID)
    )
    )

#merge these
diag_determ_records <- diag_determ_records |>
  left_join(determ_not_possible_data,
            by=c('DIAGNOSTIC_DETERMINATION_ID'))

#Filter to records that match the data request
diag_determ_records <- diag_determ_records |>
  filter(DIAGNOSTIC_DETERMINATION_ID %in% (data |> pull(DIAGNOSTIC_DETERMINATION_ID)))

output <- data |> left_join(diag_determ_records,
                          by=intersect(colnames(data),
                                       colnames(diag_determ_records)
                                      )
                        )
return(output)
}



# ---- parse determinations ----
parse_final_determination = function(data) {
  data |> 
    group_by(DIAGNOSTIC_REQUEST_ID) |> 
    #Andy Carmichael says if that the most recent "DETERMINATION_CREATED_DATETIME" should be the most accurate, with very rare exceptions.  (The only time I can think of is if there was a molecular ID request, and it failed.)   In that case there DETERM_NOT_POSS_FLAG should be flagged.  
    #Filter for rows that are not a failed diagnostic determination
    dplyr::filter(is.na(DIAGNOSTIC_DETERMINATION_NOT_POSSIBLE_REASON)) |> 
    #Filter the determination type - final has the highest TYPE_ID number
    dplyr::filter(DETERMINATION_TYPE_ID==max(DETERMINATION_TYPE_ID)) |> 
    #Filter for who ID'd it - the group_ID is a hierarchy of expertise, so take the max ID
    dplyr::filter(DETERMINED_BY_GROUP_ID == max(DETERMINED_BY_GROUP_ID)) |> 
    #The most recent is in practice the most correct - sometimes a final ID will be corrected at a later date, or someone will review for training or spot checking
    dplyr::filter(DETERMINATION_DATETIME == max(DETERMINATION_DATETIME))
}
# ---- end parse determinations ----


print_diagnostic_results <- function(parsed_determinations_data) {
  print(paste0("Found ", nrow(parsed_determinations_data), " ultimate identifications matching criteria"))
  print(head(parsed_determinations_data))
}