filter_by_taxonomy <- function(filtered_data, params, threshold = 0.9) {
  taxonomy_levels <- c("KINGDOM", "PHYLUM", "CLASS", "ORDER", "FAMILY", "GENUS", "SPECIES")
  taxonomy_params <- params[intersect(taxonomy_levels, names(params))]
  non_null_params <- names(taxonomy_params)[!sapply(taxonomy_params, is.null)]
  
  # If there are no non-NULL parameters, return the original data
  if (length(non_null_params) == 0) {
    return(filtered_data)
  }
  
  # For each non-NULL parameter, filter the data
  for (param in non_null_params) {
    # Create the column name with prefix
    column_name <- paste0("PEST_TAXONOMY_", param)
    
    # Check if the column exists in filtered_data
    if (column_name %in% colnames(filtered_data)) {
      # Get the values to filter by
      filter_values <- taxonomy_params[[param]]
      
      # Create empty result set to collect matches
      level_matches <- data.frame()
      
      # Process each value in the parameter vector
      for (param_value in filter_values) {
        # Find matches for this taxonomic value using fuzzy matching
        matches <- filtered_data %>%
          mutate(
            similarity = sapply(
              .data[[column_name]], 
              function(x) {
                if (is.na(x) || is.na(param_value)) return(0)
                x <- as.character(x)
                param_value <- as.character(param_value)
                1 - stringdist(tolower(x), tolower(param_value), method = "jw") / 
                  max(nchar(x), nchar(param_value))
              }
            )
          ) %>%
          filter(similarity >= threshold)
        
        # Add to our matches collection
        level_matches <- bind_rows(level_matches, matches)
      }
      
      # Replace filtered_data with the matches from this level
      if (nrow(level_matches) > 0) {
        filtered_data <- level_matches %>% 
          distinct() %>%
          select(-similarity)
      } else {
        # No matches found for this level
        print(paste0("No matches found for level: ", param))
        filtered_data <- filtered_data[0, ] # Return empty dataset with same structure
        return(filtered_data)
      }
    } else {
      print(paste0("Column ", column_name, " not found in data"))
    }
  }
  
  return(filtered_data)
}

print_matches <- function(data) {
  print(paste0("Found ", nrow(data), " records matching taxonomic criteria"))
  head(data)
}