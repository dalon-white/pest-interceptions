# Connect to AQAS database
connect_aqas_db <- function() {
  DBI::dbConnect(
    odbc::odbc(),
    .connection_string =
      "Driver=SQL Server;
                       Server=AAP00VA3PPQSQL0\\MSSQLSERVER,1433;
                        Database=PPQ_AQI_AQAS_DW;
trusted_connection=yes"
  )
}


aqas_intercept_sql <- function() {
  "
SELECT DISTINCT
      [INTERCEPT_ID]
      ,[LOCATION_ID]
      ,[LOCATION_NM]
      ,[INTERCEPT_DT]
      ,[TIME_ID]
      ,[FINAL_DETERM_ID]
      ,[INTERCEPT_NUM]
      ,[PORT_REFERENCE_NUMBER]
      ,[PATHWAY]
      ,[MODE_OF_TRANSPORT]
      ,[FORWARD_TO]
      ,[PRIORITY]
      ,[OVERTIME_FL]
      ,[UDF1]
      ,[ORIGIN_UNSURE_FL]
      ,[ORIGIN]
      ,[ORIGIN_ID]
      ,[DESTINATION_STATE]
      ,[DESTINATION_CITY]
      ,[DESTINATION_ZIP]
      ,[IMPORTED_AS]
      ,[MATERIAL_FOR]
      ,[WHERE_INTERCEPT]
      ,[AIRLINE]
      ,[MARITIME_NM]
      ,[CARRIER_NUM]
      ,[SHIPMENT_ID_NUM]
      ,[SHIPMENT_TYPE]
      ,[NATIONAL_AGR_RELEASE_PROG]
      ,[HOST_PROXIMITY]
      ,[INSP_HOST]
      ,[INSP_GENUS]
      ,[INSP_AGRICULT_FL]
      ,[INVALID_INSP_HOST]
      ,[BIO_HOST]
      ,[BIO_GENUS]
      ,[BIO_AGRICULT_FL]
      ,[INVALID_BIO_HOST]
      ,[HOST_PART]
      ,[HOST_QUANTITY]
      ,[UNIT]
      ,[DISEASE_STAGE]
      ,[ALIVE_IMMATURE]
      ,[ALIVE_PUPAE]
      ,[ALIVE_ADULT]
      ,[ALIVE_EGG]
      ,[ALIVE_CYST]
      ,[DEAD_IMMATURE]
      ,[DEAD_PUPAE]
      ,[DEAD_ADULT]
      ,[DEAD_EGG]
      ,[DEAD_CYST]
      ,[NIS_REVIEW_FL]
      ,[PEST]
      ,[INVALID_PEST]
      ,[INCONCL_REASON]
      ,[PEST_TYPE]
      ,[REPORTABLE_FL]
      ,[PEST_ORDER]
      ,[SUB_ORDER]
      ,[SUPER_FAMILY]
      ,[FAMILY]
      ,[SUB_FAMILY]
      ,[TRIBE]
      ,[GENUS]
      ,[SUB_GENUS]
      ,[SPECIES]
      ,[SUB_SPECIES]
      ,[AUTHOR]
      ,REPLACE(REPLACE(REPLACE([PEST_REMARKS], CHAR(13), ''), CHAR(10), ''), '|', ':::') AS [PEST_REMARKS_REFORMAT]
      ,[DETERMINED_BY]
      ,[DETERMINED_DT]
      ,REPLACE(REPLACE(REPLACE([DETERM_REMARKS], CHAR(13), ''), CHAR(10), ''), '|', ':::') AS [DETERM_REMARKS_REFORMAT]
      ,[QUARANTINE_STATUS]
      ,[SEL_LOT]
      ,[APPROVED_FL]
      ,REPLACE(REPLACE(REPLACE([REMARKS], CHAR(13), ''), CHAR(10), ''), '|', ':::') AS [REMARKS_REFORMAT]
      ,[F309_ID]
      ,[CREATE_USER]
      ,[CREATE_DT]
      ,[MODIFY_USER]
      ,[MODIFY_DT]
      ,[REFRESH_DT]
FROM [PPQ_AQI_AQAS_DW].[DW_AQAS].[PESTID_F309_INTERCEPT]
"
}


aqas_intercepts <- function(connection = connect_aqas_db()) {
  dplyr::tbl(connection, dbplyr::sql(aqas_intercept_sql()))
}


normalize_aqas_intercepts <- function(data) {
  data |>
    dplyr::mutate(
      DATA_SOURCE = "AQAS",
      SOURCE_RECORD_ID = as.character(.data$INTERCEPT_ID),
      INSPECTION_DATE = as.Date(.data$INTERCEPT_DT),
      COMMODITY_DISPLAY_NAME = dplyr::coalesce(.data$INSP_HOST, .data$BIO_HOST),
      COMMODITY_TAXONOMIC_DISPLAY_NAME = .data$INSP_HOST,
      INSPECTION_PATHWAY = .data$PATHWAY,
      SUBCATEGORY = .data$PEST_TYPE,
      #DETERMINATION_TYPE = as.character(.data$DETERMINATION_TYPE),
      QUARANTINE_RECOMMENDATION = .data$QUARANTINE_STATUS,
      DIAGNOSTIC_DETERMINATION_ID = as.character(.data$FINAL_DETERM_ID),
      PEST_DISPLAY_NAME = .data$PEST,
      PEST_TAXONOMIC_NAME = dplyr::case_when(
        !is.na(.data$GENUS) & !is.na(.data$SPECIES) ~ paste(.data$GENUS, .data$SPECIES),
        !is.na(.data$GENUS) ~ .data$GENUS,
        TRUE ~ .data$PEST
      ),
      PEST_TAXON_SIMPLE_NAME = dplyr::coalesce(.data$SPECIES, .data$GENUS, .data$PEST),
      PEST_TAXONOMY_KINGDOM = NA_character_,
      PEST_TAXONOMY_PHYLUM = NA_character_,
      PEST_TAXONOMY_CLASS = NA_character_,
      PEST_TAXONOMY_ORDER = .data$PEST_ORDER,
      PEST_TAXONOMY_FAMILY = .data$FAMILY,
      PEST_TAXONOMY_GENUS = .data$GENUS,
      PEST_TAXONOMY_SPECIES = .data$SPECIES,
      number_alive_immature = .data$ALIVE_IMMATURE,
      number_alive_pupae = .data$ALIVE_PUPAE,
      number_alive_adult = .data$ALIVE_ADULT,
      number_alive_egg = .data$ALIVE_EGG,
      number_alive_cyst = .data$ALIVE_CYST,
      number_dead_immature = .data$DEAD_IMMATURE,
      number_dead_pupae = .data$DEAD_PUPAE,
      number_dead_adult = .data$DEAD_ADULT,
      number_dead_egg = .data$DEAD_EGG,
      number_dead_cyst = .data$DEAD_CYST
    )
}


bind_aqas_arm_results <- function(arm_data, aqas_data, verbose = TRUE) {
  coerce_to_date <- function(value) {
    if (inherits(value, "Date")) {
      return(value)
    }

    as.Date(as.character(value), tryFormats = c("%m-%d-%Y", "%Y-%m-%d", "%d-%m-%Y"))
  }

  matching_columns <- intersect(names(arm_data), names(aqas_data))
  arm_only_columns <- setdiff(names(arm_data), names(aqas_data))
  aqas_only_columns <- setdiff(names(aqas_data), names(arm_data))

  if (isTRUE(verbose)) {
    print("Common columns between both datasets:")
    print(matching_columns)

    print("Columns only in ARM data:")
    print(arm_only_columns)

    print("Columns only in AQAS data:")
    print(aqas_only_columns)
  }

  column_types <- data.frame(
    column_name = matching_columns,
    arm_type = vapply(matching_columns, function(col) class(arm_data[[col]])[1], character(1)),
    aqas_type = vapply(matching_columns, function(col) class(aqas_data[[col]])[1], character(1)),
    stringsAsFactors = FALSE
  )

  mismatched_columns <- column_types[column_types$arm_type != column_types$aqas_type, ]

  if (isTRUE(verbose)) {
    print("Columns with mismatched types:")
    print(mismatched_columns)
  }

  for (i in seq_len(nrow(mismatched_columns))) {
    col <- mismatched_columns$column_name[i]
    arm_col <- arm_data[[col]]
    aqas_col <- aqas_data[[col]]

    if (inherits(arm_col, "Date") || inherits(aqas_col, "Date")) {
      arm_data[[col]] <- coerce_to_date(arm_col)
      aqas_data[[col]] <- coerce_to_date(aqas_col)
    } else if (inherits(arm_col, "POSIXt") || inherits(aqas_col, "POSIXt")) {
      arm_data[[col]] <- as.POSIXct(arm_col)
      aqas_data[[col]] <- as.POSIXct(aqas_col)
    } else if (is.character(arm_col) || is.character(aqas_col) || is.factor(arm_col) || is.factor(aqas_col)) {
      arm_data[[col]] <- as.character(arm_col)
      aqas_data[[col]] <- as.character(aqas_col)
    } else if (inherits(arm_col, "integer64") || inherits(aqas_col, "integer64")) {
      arm_data[[col]] <- as.numeric(arm_col)
      aqas_data[[col]] <- as.numeric(aqas_col)
    } else if (is.numeric(arm_col) || is.numeric(aqas_col)) {
      arm_data[[col]] <- as.numeric(arm_col)
      aqas_data[[col]] <- as.numeric(aqas_col)
    } else if (is.logical(arm_col) || is.logical(aqas_col)) {
      arm_data[[col]] <- as.logical(arm_col)
      aqas_data[[col]] <- as.logical(aqas_col)
    }
  }

  dplyr::bind_rows(arm_data, aqas_data)
}