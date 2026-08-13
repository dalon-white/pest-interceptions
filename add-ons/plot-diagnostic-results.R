# Load required libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")

library(tidyverse)
library(ggplot2)
library(viridis)
library(dplyr)
library(lubridate)

# Disable R CMD check notes about "no visible binding for global variable"
utils::globalVariables(c(
  "INSPECTION_DATE", "INSPECTION_YEAR", "ORIGIN", 
  "total_pests", "total_country_pests", "total_taxa_pests", "total_commodity_pests", 
  "overall_total", "COMMODITY_DISPLAY_NAME", "COMMODITY_TAXONOMIC_DISPLAY_NAME",
  "PEST_TAXONOMIC_NAME", "PEST_DISPLAY_NAME", "PEST_TAXON_SIMPLE_NAME",
  "similarity", "dummy", "use_fill"
))

#' Summarize diagnostic results by origin and year
#' @param data The data frame containing diagnostic results
#' @param arrange_by A string specifying how to arrange the results ("origin", "year", or "total")
#' @param truncate_origins A boolean indicating whether to truncate less common origins into "Other"
#' @param top_n_origins An integer specifying how many origins to keep before truncating
#' @param group_by_origin A boolean indicating whether to group by origin
#' @param group_by_year A boolean indicating whether to group by year
#' @param group_by_commodities A boolean indicating whether to group by commodities
#' @param truncate_commodities A boolean indicating whether to truncate less common commodities into "Other"
#' @param top_n_commodities An integer specifying how many commodities to keep before truncating
#' @param group_by_taxon A boolean indicating whether to group by taxon
#' @param truncate_taxon A boolean indicating whether to truncate less common taxa into "Other"
#' @param top_n_taxon An integer specifying how many taxa to keep before truncating
#' @return A data frame containing summarized diagnostic results
summarize_diagnostic_results <- function(data,
                                       arrange_by = "origin", #or "year"; anything else will be by total pests
                                       truncate_origins = TRUE,
                                       top_n_origins = 10,
                                       group_by_origin = TRUE,
                                       group_by_year = TRUE,
                                       group_by_commodities = FALSE,
                                       truncate_commodities = TRUE,
                                       top_n_commodities = 10,
                                       group_by_taxon = TRUE,
                                       truncate_taxon = TRUE,
                                       top_n_taxon = 10,
                                       group_by_pathway = FALSE,
                                       taxonomic_level_grouping = NULL) {
  
  # Ensure data exists and has required columns
  if (missing(data) || nrow(data) == 0) {
    warning("No data provided or empty dataset")
    return(NULL)
  }
  
  # Create a copy to avoid modifying the original data
  working_data <- data
  
  if(!is.null(taxonomic_level_grouping)){
    #Use the taxonomic level specified to find the column
    # For instance taxonomic_level_grouping = "GENUS" would look for "PEST_TAXONOMY_GENUS"
    taxon_col <- paste0("PEST_TAXONOMY_", toupper(taxonomic_level_grouping))
    if(taxon_col %in% colnames(working_data)){
      working_data <- working_data %>%
        mutate(PEST_TAXONOMIC_NAME = .data[[taxon_col]])
    } else {
      warning(paste("Specified taxonomic level column", taxon_col, "not found in data. Using default PEST_TAXONOMIC_NAME."))
    }
  }


  # Add a year column if grouping by year
  if (group_by_year) {
    working_data <- working_data %>% 
      mutate(INSPECTION_YEAR = lubridate::year(INSPECTION_DATE)) %>% 
      group_by(INSPECTION_YEAR) %>% 
      mutate(INSPECTION_YEAR = as.factor(INSPECTION_YEAR))
  }
  
  # Add grouping variables as requested
  group_vars <- c()
  
  if (group_by_year) {
    group_vars <- c(group_vars, "INSPECTION_YEAR")
  }
  
  if (group_by_origin) {
    group_vars <- c(group_vars, "ORIGIN")
  }
  
  if (group_by_commodities) {
    group_vars <- c(group_vars, "COMMODITY_DISPLAY_NAME", "COMMODITY_TAXONOMIC_DISPLAY_NAME")
  }
  
  if (group_by_taxon) {
    group_vars <- c(group_vars, "PEST_TAXONOMIC_NAME", "PEST_TAXON_SIMPLE_NAME")
  }
  if (group_by_pathway) {
    group_vars <- c(group_vars, "INSPECTION_PATHWAY")
  }
  
  # If no grouping variables, use a dummy grouping
  if (length(group_vars) == 0) {
    working_data <- working_data %>% mutate(dummy = 1)
    group_vars <- "dummy"
  }
  # If truncating origins, group less common origins into "Other"
  if (truncate_origins && "ORIGIN" %in% colnames(working_data)) {
    # Identify top origins by total pest count
    top_origins <-  working_data %>%
      group_by(ORIGIN) %>%
      summarise(total_country_pests = n()) %>%
      arrange(desc(total_country_pests)) %>%
      slice_head(n = top_n_origins) %>%
      pull(ORIGIN)
    
    # Create a new dataframe with top origins and "Other"
    working_data <- working_data %>%
      mutate(ORIGIN = ifelse(ORIGIN %in% top_origins, ORIGIN, "Other"))
  }
  
  # If truncating taxonomic names, group less common taxa into "Other"
  if (truncate_taxon && "PEST_TAXONOMIC_NAME" %in% colnames(working_data)) {
    # Identify top taxa by total pest count
    top_taxa <- working_data %>%
      group_by(PEST_TAXONOMIC_NAME) %>%
      summarise(total_taxa_pests = n()) %>%
      arrange(desc(total_taxa_pests)) %>%
      slice_head(n = top_n_taxon) %>%
      pull(PEST_TAXONOMIC_NAME)
    
    # Create a new dataframe with top taxa and "Other"
    working_data <- working_data %>%
      mutate(PEST_TAXONOMIC_NAME = ifelse(PEST_TAXONOMIC_NAME %in% top_taxa, PEST_TAXONOMIC_NAME, "Other"))
  }
  
  # If truncating commodities, group less common commodities into "Other"
  if (truncate_commodities && "COMMODITY_DISPLAY_NAME" %in% colnames(working_data)) {
    # Identify top commodities by total pest count
    top_commodities <- working_data %>%
      group_by(COMMODITY_DISPLAY_NAME) %>%
      summarise(total_commodity_pests = n()) %>%
      arrange(desc(total_commodity_pests)) %>%
      slice_head(n = top_n_commodities) %>%
      pull(COMMODITY_DISPLAY_NAME)
    
    # Create a new dataframe with top commodities and "Other"
    working_data <- working_data %>%
      mutate(COMMODITY_DISPLAY_NAME = ifelse(COMMODITY_DISPLAY_NAME %in% top_commodities, COMMODITY_DISPLAY_NAME, "Other"))
  }

  # Group and summarize
  working_data <- working_data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(total_pests = n(), .groups = "drop")
  
  # Arrange based on the specified column
  if (arrange_by == "year" && "INSPECTION_YEAR" %in% colnames(working_data)) {
    working_data <- working_data %>% arrange(INSPECTION_YEAR, desc(total_pests))
  } else if (arrange_by == "origin" && "ORIGIN" %in% colnames(working_data)) {
    working_data <- working_data %>% arrange(desc(total_pests))
  } else {
    working_data <- working_data %>% arrange(desc(total_pests))
  }
  

  summary_df <- working_data
   if (group_by_origin) {
    # Re-order factor levels based on total pest counts
    origin_order <- working_data %>%
      group_by(ORIGIN) %>%
      summarise(overall_total = sum(total_pests)) %>%
      arrange(desc(overall_total)) %>%
      pull(ORIGIN)
    
    # Set factor levels to maintain order
    summary_df$ORIGIN <- factor(summary_df$ORIGIN, levels = origin_order)
  }
  
  # If grouping by taxonomic name, order factor levels
  if (group_by_taxon && "PEST_TAXONOMIC_NAME" %in% colnames(summary_df)) {
    # Re-order factor levels based on total pest counts
    taxon_order <- summary_df %>%
      group_by(PEST_TAXONOMIC_NAME) %>%
      summarise(overall_total = sum(total_pests)) %>%
      arrange(desc(overall_total)) %>%
      pull(PEST_TAXONOMIC_NAME)
    
    # Set factor levels to maintain order
    summary_df$PEST_TAXONOMIC_NAME <- factor(summary_df$PEST_TAXONOMIC_NAME, levels = taxon_order)
  }
  
  # If grouping by commodity, order factor levels
  if (group_by_commodities && "COMMODITY_DISPLAY_NAME" %in% colnames(summary_df)) {
    # Re-order factor levels based on total pest counts
    commodity_order <- summary_df %>%
      group_by(COMMODITY_DISPLAY_NAME) %>%
      summarise(overall_total = sum(total_pests)) %>%
      arrange(desc(overall_total)) %>%
      pull(COMMODITY_DISPLAY_NAME)
    
    # Set factor levels to maintain order
    summary_df$COMMODITY_DISPLAY_NAME <- factor(summary_df$COMMODITY_DISPLAY_NAME, levels = commodity_order)
  }

  # Return the summary data frame
  return(summary_df)
}

#' Save summary data to a CSV file
#' @param data The data frame to save
#' @param base_filename The base filename to use for the output file
save_summary <- function(data, base_filename, output_dir = here::here(".gitignored", "output", "data")) {
  if (missing(base_filename)) {
    base_filename <- paste0("diagnostic_results_summary_", format(Sys.Date(), "%Y%m%d"))
  }

  output_file <- paste0(base_filename, "_summary.csv")
  if (dirname(output_file) %in% c(".", "")) {
    output_file <- file.path(output_dir, output_file)
  }

  output_parent <- dirname(output_file)
  if (!dir.exists(output_parent)) {
    dir.create(output_parent, recursive = TRUE)
  }
  
  # Save the summary data to a CSV file
  write.csv(data, output_file, row.names = FALSE)
  
  # Return the filename (invisible)
  invisible(output_file)
}

#' Create and save a plot of diagnostic results
#' @param data The data frame to plot
#' @param base_filename The base filename to use for the output file
#' @param plot_x The column name to use for the x-axis
#' @param xlab The label for the x-axis
#' @param plot_y The column name to use for the y-axis
#' @param ylab The label for the y-axis
#' @param plot_fill The column name to use for the fill color
#' @param fill_lab The label for the fill color legend
#' @param plot_title The title for the plot
#' @param plot_subtitle The subtitle for the plot
#' @param width The width of the output plot in inches
#' @param height The height of the output plot in inches
#' @param dpi The resolution of the output plot in dots per inch
save_plot <- function(data,
                      base_filename = NULL,
                      output_dir = here::here(".gitignored", "output", "data"),
                      plot_x = "INSPECTION_YEAR",
                      xlab = "Year",
                      plot_y = "total_pests",
                      ylab = "Total Pests",
                      plot_fill = "ORIGIN",
                      fill_lab = "Origin",
                      plot_title = "Pest Interceptions by Year and Origin",
                      plot_subtitle = "Summary of pest interceptions",
                      width = 10,
                      height = 7,
                      dpi = 300) {
  
  if (missing(data) || is.null(data) || nrow(data) == 0) {
    warning("No data provided or empty dataset for plotting")
    return(invisible(NULL))
  }
  
  if (is.null(base_filename)) {
    base_filename <- paste0("diagnostic_results_plot_", format(Sys.Date(), "%Y%m%d"))
  }

  output_file <- paste0(base_filename, "_plot.png")
  if (dirname(output_file) %in% c(".", "")) {
    output_file <- file.path(output_dir, output_file)
  }

  output_parent <- dirname(output_file)
  if (!dir.exists(output_parent)) {
    dir.create(output_parent, recursive = TRUE)
  }
  
  # Check if required columns exist
  required_cols <- c(plot_x, plot_y, plot_fill)
  missing_cols <- required_cols[!required_cols %in% colnames(data)]
  
  if (length(missing_cols) > 0) {
    warning("Missing required columns for plotting: ", 
            paste(missing_cols, collapse = ", "),
            ". Using available columns.")
    
    # Use available columns instead
    if (!plot_x %in% colnames(data) && "INSPECTION_YEAR" %in% colnames(data)) {
      plot_x <- "INSPECTION_YEAR"
    } else if (!plot_x %in% colnames(data)) {
      stop("Cannot create plot: required x-axis column not found in data")
    }
    
    if (!plot_y %in% colnames(data) && "total_pests" %in% colnames(data)) {
      plot_y <- "total_pests"
    } else if (!plot_y %in% colnames(data)) {
      stop("Cannot create plot: required y-axis column not found in data")
    }
    
    if (!plot_fill %in% colnames(data) && "ORIGIN" %in% colnames(data)) {
      plot_fill <- "ORIGIN"
    } else if (!plot_fill %in% colnames(data)) {
      warning("Fill column not found in data. Creating plot without fill.")
      use_fill <- FALSE
    } else {
      use_fill <- TRUE
    }
  } else {
    use_fill <- TRUE
  }
  
  # Create the plot
  if (use_fill) {
    p <- ggplot(data, aes_string(x = plot_x, y = plot_y, fill = plot_fill)) +
      geom_col(position = "dodge") +
      scale_fill_viridis_d(option = "plasma")
  } else {
    p <- ggplot(data, aes_string(x = plot_x, y = plot_y)) +
      geom_col(position = "dodge", fill = "steelblue")
  }
  
  # Add labels and theme
  p <- p +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = xlab,
      y = ylab,
      fill = fill_lab
    ) +
    theme_classic() +
    theme(
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Save the plot
  ggsave(output_file, 
         plot = p, 
         width = width, 
         height = height, 
         dpi = dpi)
  
  # Return the plot object invisibly
  invisible(p)
}