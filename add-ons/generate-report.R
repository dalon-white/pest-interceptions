# Load required packages
if (!requireNamespace("rmarkdown", quietly = TRUE)) install.packages("rmarkdown")
if (!requireNamespace("htmltools", quietly = TRUE)) install.packages("htmltools")

library(htmltools)

# Source the YAML header loader to access the document title
source(here::here("add-ons","load-yaml-header-in-vscode.R"))

#' Generate a comprehensive HTML report for pest interception data
#'
#' @param data The data frame containing the pest interception data
#' @param params The parameters used for filtering the data (from YAML header)
#' @param base_filename The base filename to use for saving the report
#' @param include_plot Whether to include a reference to the plot in the report (default: TRUE)
#' @param include_data_preview Whether to include a preview of the data in the report (default: TRUE)
#' @param include_parameters Whether to include a section showing the search parameters (default: TRUE)
#' @param include_methodology Whether to include a section describing the methodology (default: TRUE)
#' @param custom_title Custom title for the report (default: uses document title or "Pest Interceptions Report")
#' @param additional_content Any additional HTML content to include in the report
#'
#' @return HTML content as a string
#' @export
#'
generate_pest_report <- function(
  data = df_diagnostic_results,
  params = NULL,
  base_filename = NULL,
  include_plot = TRUE,
  include_data_preview = TRUE,
  include_parameters = TRUE,
  include_methodology = TRUE,
  custom_title = NULL,
  additional_content = NULL
) {
  # Load necessary libraries
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    install.packages("htmltools")
    library(htmltools)
  }
  
  # Get title from document if available and not explicitly provided
  if (is.null(custom_title)) {
    tryCatch({
      custom_title <- get_document_title()
    }, error = function(e) {
      custom_title <- "Pest Interceptions Report"
    })
  }
  
  # Generate base_filename if not provided
  if (is.null(base_filename)) {
    # Create a clean version of the title for the filename
    title_for_filename <- gsub("[^a-zA-Z0-9]", "_", custom_title)
    title_for_filename <- gsub("_+", "_", title_for_filename)
    
    # Add date to filename
    base_filename <- paste0(title_for_filename, "_", format(Sys.Date(), "%Y%m%d"))
  }

  source_label <- "ARM"
  if ("DATA_SOURCE" %in% names(data)) {
    data_sources <- sort(unique(stats::na.omit(data$DATA_SOURCE)))
    if (length(data_sources) > 0) {
      source_label <- paste(data_sources, collapse = " and ")
    }
  }
  
  # CSS for the report
  css_content <- "
    body { font-family: Arial, sans-serif; line-height: 1.6; max-width: 900px; margin: 0 auto; padding: 20px; }
    h1 { color: #2c3e50; border-bottom: 1px solid #eee; padding-bottom: 10px; }
    h2 { color: #3498db; margin-top: 25px; }
    h3 { color: #2980b9; }
    table { border-collapse: collapse; width: 100%; margin: 20px 0; }
    th, td { padding: 12px 15px; border: 1px solid #ddd; text-align: left; }
    th { background-color: #f8f9fa; }
    tr:nth-child(even) { background-color: #f2f2f2; }
    .summary { background-color: #f8f8f8; border-left: 4px solid #3498db; padding: 15px; margin: 20px 0; }
    .timestamp { color: #7f8c8d; font-style: italic; margin-top: 30px; }
    .section { margin-bottom: 30px; }
    .footer { margin-top: 40px; border-top: 1px solid #eee; padding-top: 20px; font-size: 0.9em; }
  "
  
  # Start building report content
  report_content <- tags$html(
    tags$head(
      tags$title(custom_title),
      tags$style(css_content)
    ),
    
    tags$body(
      # Header section
      tags$h1(custom_title),
      tags$div(class = "timestamp", 
               paste("Generated on:", format(Sys.time(), "%A, %B %d, %Y at %I:%M %p"))),
      
      # Summary section
      tags$div(class = "summary",
        tags$p(paste0("This report documents a search for pest interceptions in the ", source_label, " data source(s).")),
        tags$p(tags$strong(paste0("Found ", nrow(data), " matching interception records.")))
      ),
      
      # Parameters section (conditional)
      if (include_parameters && !is.null(params)) {
        tags$div(class = "section",
          tags$h2("Search Parameters"),
          tags$table(
            tags$tr(tags$th("Parameter"), tags$th("Value")),
            
            # Date Range
            tags$tr(
              tags$td("Date Range"),
              tags$td(
                paste0(
                  if(!is.null(params$begin_date)) params$begin_date else "anytime", 
                  " to ", 
                  if(!is.null(params$end_date)) {
                    if(params$end_date == "today") format(Sys.Date(), "%Y-%m-%d") else params$end_date
                  } else "anytime"
                )
              )
            ),
            
            # Pest Nickname
            tags$tr(
              tags$td("Pest Nickname"),
              tags$td(
                if(!is.null(params$pest_nickname)) paste(params$pest_nickname, collapse=", ") else "All pests"
              )
            ),
            
            # Commodity Common Name
            tags$tr(
              tags$td("Commodity Common Name"),
              tags$td(
                if(!is.null(params$commodity_common_name)) paste(params$commodity_common_name, collapse=", ") else "Any"
              )
            ),
            
            # Commodity Taxonomic Name
            tags$tr(
              tags$td("Commodity Taxonomic Name"),
              tags$td(
                if(!is.null(params$commodity_taxonomic_name)) paste(params$commodity_taxonomic_name, collapse=", ") else "Any"
              )
            ),
            
            # Origin Country
            tags$tr(
              tags$td("Origin Country"),
              tags$td(
                if(!is.null(params$origin_country)) paste(params$origin_country, collapse=", ") else "Any"
              )
            ),
            
            # Pest Taxonomy: Kingdom
            tags$tr(
              tags$td("Pest Taxonomy: Kingdom"),
              tags$td(
                if(!is.null(params$KINGDOM)) paste(params$KINGDOM, collapse=", ") else "Any"
              )
            ),
            
            # Pest Taxonomy: Phylum
            tags$tr(
              tags$td("Pest Taxonomy: Phylum"),
              tags$td(
                if(!is.null(params$PHYLUM)) paste(params$PHYLUM, collapse=", ") else "Any"
              )
            ),
            
            # Pest Taxonomy: Class
            tags$tr(
              tags$td("Pest Taxonomy: Class"),
              tags$td(
                if(!is.null(params$CLASS)) paste(params$CLASS, collapse=", ") else "Any"
              )
            ),
            
            # Pest Taxonomy: Order
            tags$tr(
              tags$td("Pest Taxonomy: Order"),
              tags$td(
                if(!is.null(params$ORDER)) paste(params$ORDER, collapse=", ") else "Any"
              )
            ),
            
            # Pest Taxonomy: Family
            tags$tr(
              tags$td("Pest Taxonomy: Family"),
              tags$td(
                if(!is.null(params$FAMILY)) paste(params$FAMILY, collapse=", ") else "Any"
              )
            ),
            
            # Pest Taxonomy: Genus
            tags$tr(
              tags$td("Pest Taxonomy: Genus"),
              tags$td(
                if(!is.null(params$GENUS)) paste(params$GENUS, collapse=", ") else "Any"
              )
            ),
            
            # Pest Taxonomy: Species
            tags$tr(
              tags$td("Pest Taxonomy: Species"),
              tags$td(
                if(!is.null(params$SPECIES)) paste(params$SPECIES, collapse=", ") else "Any"
              )
            )
          )
        )
      } else {
        NULL  # No parameters section if not requested
      },
      
      # Methodology section (conditional)
      if (include_methodology) {
        tags$div(class = "section",
          tags$h2("Search Methodology"),
          tags$p("The following steps were taken to generate this data:"),
          tags$ol(
            tags$li(paste0("Connected to ", source_label, " data source(s) and retrieved interception records")),
            
            # Conditional list items based on params
            if(!is.null(params) && (!is.null(params$begin_date) || !is.null(params$end_date))) 
              tags$li("Filtered results by date range") else NULL,
              
            if(!is.null(params) && !is.null(params$origin_country)) 
              tags$li("Filtered results by origin country") else NULL,
              
            if(!is.null(params) && (!is.null(params$commodity_common_name) || !is.null(params$commodity_taxonomic_name))) 
              tags$li("Filtered results by commodity names using string pattern matching") else NULL,
              
            if(!is.null(params) && (!is.null(params$KINGDOM) || !is.null(params$PHYLUM) || !is.null(params$CLASS) || 
               !is.null(params$ORDER) || !is.null(params$FAMILY) || !is.null(params$GENUS) || !is.null(params$SPECIES)))
              tags$li("Filtered results by pest taxonomy using fuzzy matching") else NULL,
              
            tags$li(
              "Obtained the final determinations by:",
              tags$ul(
                tags$li("Filtering out determinations flagged as not possible"),
                tags$li("Selecting the determinations with the highest type ID"),
                tags$li("Prioritizing determinations by highest expertise group ID"),
                tags$li("Using the most recent determination when multiple options existed")
              )
            )
          )
        )
      } else {
        NULL  # No methodology section if not requested
      },
      
      # Results summary
      tags$div(class = "section",
        tags$h2("Results Summary"),
        tags$p(paste0("The search returned ", 
               tags$strong(nrow(data)), 
               " ultimate identifications matching the criteria.")),
               
        # Add links to any generated plot files
        if (include_plot && file.exists(paste0(base_filename, "_plot.png"))) {
          tags$div(
            tags$h3("Visualizations"),
            tags$p("The following visualizations were generated from the data:"),
            tags$ul(
              tags$li(tags$a(href = paste0(base_filename, "_plot.png"), "Visualization Plot"))
            ),
            tags$img(src = paste0(base_filename, "_plot.png"), 
                   alt = "Pest Interception Visualization", 
                   style = "max-width: 600px; margin: 20px 0;")
          )
        } else {
          NULL
        }
      ),
      
      # Sample data preview (conditional)
      if (include_data_preview && nrow(data) > 0) {
        tags$div(class = "section",
          tags$h3("Sample Data Preview"),
          tags$p("Below is a preview of the first few records:"),
          if(nrow(data) > 0) {
            preview_data <- head(data, 5)
            tags$table(
              tags$tr(
                lapply(names(preview_data)[seq_len(min(6, ncol(preview_data)))], function(col) tags$th(col))
              ),
              lapply(seq_len(nrow(preview_data)), function(row) {
                tags$tr(
                  lapply(seq_len(min(6, ncol(preview_data))), function(col) tags$td(as.character(preview_data[row, col])))
                )
              })
            )
          } else {
            tags$p("No data available to preview.")
          },
          tags$p(tags$em("Note: The preview shows only the first 5 rows and 6 columns for brevity."))
        )
      } else {
        NULL  # No data preview if not requested
      },
      
      # Data access section
      tags$div(class = "section",
        tags$h2("Accessing the Data"),
        tags$p(
          "The complete interactive dataset can be accessed ", 
          tags$a(href = paste0(base_filename, "_data.html"), "here"), "."
        ),
        tags$p("The interactive data viewer provides options to copy, download as CSV, Excel, or PDF using the buttons at the top of the table.")
      ),
      
      # Any additional custom content
      if (!is.null(additional_content)) {
        tags$div(class = "section", HTML(additional_content))
      } else {
        NULL
      },
      
      # Footer
      tags$div(class = "footer",
        tags$p("Report generated by the Pest Interception Analysis Tool"),
        tags$p(paste("Generated using R", R.version.string)),
        tags$p(paste("USDA APHIS PPQ -", format(Sys.time(), "%Y")))
      )
    )
  )
  
  # Convert the tags object to HTML
  html_content <- as.character(report_content)
  
  # Return the HTML content
  return(html_content)
}

#' Save the generated report to a file
#'
#' @param report_content The HTML content to save
#' @param base_filename The base filename for saving the report
#' @param report_suffix The suffix to add to the filename (default: "_report")
#'
#' @return The full path to the saved report file
#' @export
#'
save_report <- function(report_content, base_filename, report_suffix = "_report") {
  # Ensure the base_filename doesn't already have the report_suffix
  if (!grepl(report_suffix, base_filename)) {
    output_file <- paste0(base_filename, report_suffix, ".html")
  } else {
    output_file <- paste0(base_filename, ".html")
  }

  output_dir <- dirname(output_file)
  if (!output_dir %in% c(".", "") && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Write the report to file
  writeLines(report_content, output_file)
  
  # Return the filename
  return(output_file)
}