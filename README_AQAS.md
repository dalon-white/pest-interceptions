# AQAS Data Access and Normalization

# Purpose

This document describes how this project pulls older pest interception data from AQAS and prepares it to work with the existing ARM-oriented request, filter, plotting, and report workflow.

AQAS contains similar interception information to ARM, but the table and column names are not the same. The current approach does not use a separate reference table or external column crosswalk. Instead, AQAS rows are normalized in R by adding a set of shared, report-friendly columns that match the column names already used by the ARM workflow.

The normalization code is in `functions/connect_and_access_aqas.R`, especially `normalize_aqas_intercepts()`.

# Method

The AQAS workflow has three steps:

1. Connect to the AQAS database with `connect_aqas_db()`.
2. Retrieve AQAS interception records with `aqas_intercepts()`.
3. Convert AQAS-specific columns into the shared analysis columns with `normalize_aqas_intercepts()`.

The original AQAS columns are retained. The normalization step adds shared columns such as `INSPECTION_DATE`, `COMMODITY_DISPLAY_NAME`, `ORIGIN`, and `PEST_TAXONOMY_GENUS` so AQAS records can pass through the same filters and report functions as ARM data.

This means the column equivalencies are currently encoded in R code, not stored in a standalone reference table. That keeps the process simple while the mapping is mostly direct or uses simple fallback rules. A separate reference table may be useful later if more legacy sources are added, if many more fields need to be harmonized, or if the mappings need to be reviewed outside the code.

# AQAS Query

The AQAS records come from:

```sql
[PPQ_AQI_AQAS_DW].[DW_AQAS].[PESTID_F309_INTERCEPT]
```

The query uses `SELECT DISTINCT` and reformats three remark fields to remove carriage returns, line breaks, and pipe characters that can cause problems when writing reports or delimited files:

```sql
REPLACE(REPLACE(REPLACE([PEST_REMARKS], CHAR(13), ''), CHAR(10), ''), '|', ':::') AS [PEST_REMARKS_REFORMAT]
REPLACE(REPLACE(REPLACE([DETERM_REMARKS], CHAR(13), ''), CHAR(10), ''), '|', ':::') AS [DETERM_REMARKS_REFORMAT]
REPLACE(REPLACE(REPLACE([REMARKS], CHAR(13), ''), CHAR(10), ''), '|', ':::') AS [REMARKS_REFORMAT]
```

# Column Mapping

The table below shows the shared columns added for reporting and filtering, and the AQAS column or expression used to create each one.

| Shared column used by ARM-style workflow | AQAS source column or expression | Notes |
|---|---|---|
| `DATA_SOURCE` | literal value `"AQAS"` | Marks the row as coming from AQAS. |
| `SOURCE_RECORD_ID` | `as.character(INTERCEPT_ID)` | Preserves the AQAS source record identifier. |
| `INSPECTION_DATE` | `as.Date(INTERCEPT_DT)` | Used by `filter_dates()` and year summaries. |
| `COMMODITY_DISPLAY_NAME` | `coalesce(INSP_HOST, BIO_HOST)` | Uses inspected host first, then biological host if inspected host is missing. |
| `COMMODITY_TAXONOMIC_DISPLAY_NAME` | `INSP_HOST` | AQAS inspected host is used for commodity taxonomic matching; `INSP_GENUS` and `BIO_GENUS` are not used for this shared field. |
| `INSPECTION_PATHWAY` | `PATHWAY` | Used by pathway summaries and plots. |
| `SUBCATEGORY` | `PEST_TYPE` | Carries AQAS pest type into the shared result set. |
| `DETERMINATION_TYPE` | `as.character(DETERMINATION_TYPE)` | Uses the AQAS determination type field directly, matching the ARM column by meaning. |
| `QUARANTINE_RECOMMENDATION` | `QUARANTINE_STATUS` | Carries AQAS quarantine status into the shared result set. |
| `DIAGNOSTIC_DETERMINATION_ID` | `as.character(FINAL_DETERM_ID)` | Normalized to character so ARM and AQAS rows can be combined. |
| `PEST_DISPLAY_NAME` | `PEST` | AQAS pest display text. |
| `PEST_TAXONOMIC_NAME` | `GENUS` + `SPECIES`, else `GENUS`, else `PEST` | Builds a taxonomic display value when structured fields are available. |
| `PEST_TAXON_SIMPLE_NAME` | `coalesce(SPECIES, GENUS, PEST)` | Uses the most specific available AQAS pest name component. |
| `PEST_TAXONOMY_KINGDOM` | `NA_character_` | AQAS table does not currently provide this field in the helper query. |
| `PEST_TAXONOMY_PHYLUM` | `NA_character_` | AQAS table does not currently provide this field in the helper query. |
| `PEST_TAXONOMY_CLASS` | `NA_character_` | AQAS table does not currently provide this field in the helper query. |
| `PEST_TAXONOMY_ORDER` | `PEST_ORDER` | Used by taxonomy filters. |
| `PEST_TAXONOMY_FAMILY` | `FAMILY` | Used by taxonomy filters. |
| `PEST_TAXONOMY_GENUS` | `GENUS` | Used by taxonomy filters. |
| `PEST_TAXONOMY_SPECIES` | `SPECIES` | Used by taxonomy filters. |
| `number_alive_immature` | `ALIVE_IMMATURE` | Keeps AQAS life-stage count information. |
| `number_alive_pupae` | `ALIVE_PUPAE` | Keeps AQAS life-stage count information. |
| `number_alive_adult` | `ALIVE_ADULT` | Keeps AQAS life-stage count information. |
| `number_alive_egg` | `ALIVE_EGG` | Keeps AQAS life-stage count information. |
| `number_alive_cyst` | `ALIVE_CYST` | Keeps AQAS life-stage count information. |
| `number_dead_immature` | `DEAD_IMMATURE` | Keeps AQAS life-stage count information. |
| `number_dead_pupae` | `DEAD_PUPAE` | Keeps AQAS life-stage count information. |
| `number_dead_adult` | `DEAD_ADULT` | Keeps AQAS life-stage count information. |
| `number_dead_egg` | `DEAD_EGG` | Keeps AQAS life-stage count information. |
| `number_dead_cyst` | `DEAD_CYST` | Keeps AQAS life-stage count information. |

# Combining ARM and AQAS Results

Request files can retrieve ARM and AQAS data separately, apply the same filters to each source, and then combine the filtered results.

```r
source(here::here("functions", "connect_and_access_aqas.R"))
source(here::here("functions", "filter_dates.R"))
source(here::here("functions", "filter_commodity.R"))
source(here::here("functions", "filter_country.R"))
source(here::here("functions", "filter_taxa.R"))

# AQAS pull
aqas_conn <- connect_aqas_db()
df_aqas <- aqas_intercepts(aqas_conn) |>
  collect() |>
  normalize_aqas_intercepts()

# Apply the same request parameters used for ARM
aqas_results <- df_aqas |>
  filter_dates(begin_date = begin_date, end_date = end_date) |>
  filter_commodity(params = params) |>
  filter_country(params = params) |>
  filter_by_taxonomy(params = params, threshold = 0.99)

# Combine while reconciling type differences in shared columns.
results <- bind_aqas_arm_results(arm_results, aqas_results)
```

AQAS fields that are not equivalent to ARM fields remain separate after binding. For example, AQAS `LOCATION_ID` is not written into ARM `INSPECTION_LOCATION_STATE_CODE`, and AQAS `INTERCEPT_ID` is not written into ARM `DIAGNOSTIC_REQUEST_ID`.

# Current Caveats

- The mapping is intentionally limited to columns needed by the current filters, summaries, plots, and reports.
- AQAS does not currently provide normalized kingdom, phylum, or class fields through this helper, so those shared columns are set to `NA_character_`.
- AQAS `LOCATION_ID` remains separate from ARM `INSPECTION_LOCATION_STATE_CODE` because the fields are not semantically identical.
- AQAS `INTERCEPT_ID` remains separate from ARM `DIAGNOSTIC_REQUEST_ID` because the fields are not semantically identical.
- The current approach keeps the original AQAS columns in the output, so users can trace normalized fields back to the source data.
- A formal reference table or crosswalk may be useful if this project needs broader field harmonization or more source databases.