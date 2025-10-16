
# Connect to ARM database
connect_db <- function() {
  DBI::dbConnect(
    odbc::odbc(),
    .connection_string =
      "Driver=SQL Server;
                       Server=AAP00VA3PPQSQL0\\MSSQLSERVER,1433;
                        Database=PPQ_AQI_ARMDMV2;
trusted_connection=yes")
}


# Access diagnostic requests table
mvw_diagnostic_results <- function() {tbl(
  connect_db(),
  sql("select * FROM [APHIS_Imports].[dbo].[mvw_Diagnostic_Results]")
)}