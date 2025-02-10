



################################################################################################################################################################################################
# functions
################################################################################################################################################################################################

# Grouping the data
process_lipid_data <- function(sequence, data) {
  # Initialize an empty list to store the sample names by group and other information
  results <- list()
  
  sample_identifiers <- rownames(sequence)[sequence[, "labels"] == "Sample"]
  groups <- sequence[sample_identifiers, "group"]
  names <- sample_identifiers
  
  # Extract the 'Sample' labels and corresponding 'group' from 'sequence'
  sample_rows <- sequence[sequence$labels == "Sample", ]
  results$sample_rows <- sample_rows  # Store the sample rows in the results list
  
  unique_groups <- unique(sample_rows$group)
  results$unique_groups <- unique_groups  # Store the unique groups in the results list
  
  # Initialize an empty list within results for grouped_samples
  results$grouped_samples <- list()
  
  # Iterate over each group to get the corresponding sample names
  for (group in unique_groups) {
    if (!is.na(group)) {
      # Get the sample names for the current group
      samples_in_group <- sample_rows[sample_rows$group == group, 1]
      
      # Add the sample names to the list, named by their group
      results$grouped_samples[[paste("group", group)]] <- samples_in_group
      results[[paste("Group", group)]] <- samples_in_group  # Store each group separately in the results list
    }
  }
  
  # Check if any of the components are NULL or empty and handle accordingly
  if (length(results$grouped_samples) == 0) {
    results$grouped_samples <- list(group1 = NA)  # Placeholder if no groups found
  }
  
  
  # Return the list of results that now includes grouped samples, unique groups, sample rows, and each group
  return(results)
}



# Function to create grouped data frames based on sequence and data
create_grouped_data_frames <- function(sequence, data) {
  # Initialize an empty list to store data frames for each group
  grouped_data_frames <- list()
  
  # Extract the 'Sample' labels and corresponding 'group' from 'sequence'
  sample_rows <- sequence[sequence$labels == "Sample", ]
  unique_groups <- unique(sample_rows$group)
  
  # Iterate over each unique group to create data frames
  for (group in unique_groups) {
    if (!is.na(group)) {
      # Get the sample identifiers for the current group
      sample_identifiers <- rownames(sample_rows)[sample_rows$group == group]
      
      # Find the matching column indices, excluding NA values
      matching_indices <- match(sample_identifiers, colnames(data))
      matching_indices <- matching_indices[!is.na(matching_indices)]
      
      # Check if we have any matching columns at all
      if (length(matching_indices) > 0) {
        # Select only the columns for the current group
        group_data <- data[, matching_indices, drop = FALSE]
        
        
        
        # Store the filtered data frame in the list, named by the group
        grouped_data_frames[[paste("group", group)]] <- group_data
      } else {
        warning(paste("Group", group, "contains column names that are not in the data. Skipping this group."))
      }
    }
  }
  
  return(grouped_data_frames)
}




# Function to group lipids by their group prefix (e.g., "CAR", "LP", etc.)
group_lipids_by_group <- function(data) {
  # Assuming the first column of 'data' contains the lipid names like "CAR(18:1)"
  lipid_names <- data[[1]]  # Replace 1 with the actual column name or index if different
  
  # Use a regular expression to extract the group prefix from lipid names
  # This matches any consecutive alphabetic characters at the beginning of the string
  lipid_groupes <- sub("\\(([0-9]+:[0-9]+)\\).*", "", lipid_names)
  
  # Create a data frame that maps lipid names to their group
  group_mapping <- data.frame(Lipid_Name = lipid_names, group = lipid_groupes, stringsAsFactors = FALSE)
  
  # Optionally, if you want to return a list that names each group by its group
  # names(grouped_data) <- unique(lipid_groupes)
  
  return(group_mapping)
}


merge_duplicates <- function(data) {
  # Define possible names for the compound name column
  possible_name_cols <- c("Name", "Compound.name", "Compound_Name", "CompoundName")
  
  # Find the compound name column
  compound_name_col <- intersect(possible_name_cols, names(data))
  
  if (length(compound_name_col) == 0) {
    stop("No compound name column found. Please ensure your data has one of the following column names: ", paste(possible_name_cols, collapse = ", "))
  } else if (length(compound_name_col) > 1) {
    warning("Multiple compound name columns found: ", paste(compound_name_col, collapse = ", "), ". Using the first one: ", compound_name_col[1])
    compound_name_col <- compound_name_col[1]
  }
  
  # Group by the compound name column and sum all numeric columns
  data_merged <- data %>%
    group_by(across(all_of(compound_name_col))) %>%
    summarise(
      across(where(is.numeric), sum, na.rm = TRUE),
      .groups = 'drop'
    )
  
  return(data_merged)
}

merged_info_function <- function(data) {
  # Define possible names for the compound name column
  possible_name_cols <- c("Name", "Compound.name", "Compound_Name", "CompoundName", "Compound.Name")
  
  # Find the compound name column
  compound_name_col <- intersect(possible_name_cols, names(data))
  
  if (length(compound_name_col) == 0) {
    # No compound name column found, return NULL instead of stopping
    warning("No compound name column found. Please ensure your data has one of the following column names: ",
            paste(possible_name_cols, collapse = ", "), ". Returning NULL.")
    return(NULL)
  } else if (length(compound_name_col) > 1) {
    warning("Multiple compound name columns found: ", 
            paste(compound_name_col, collapse = ", "), 
            ". Using the first one: ", compound_name_col[1])
    compound_name_col <- compound_name_col[1]
  }
  
  # Define possible original annotation column names
  possible_annotation_cols <- c(
    "Original.annotation",
    "Original annotation",
    "Original_Annotation",
    "OriginalAnnotation",
    "original_annotation",
    "Original.Annotations",
    "Original.Annotations",
    "original.annotation",
    "original Annotation"
  )
  
  # Find the annotation column
  annotation_col <- intersect(possible_annotation_cols, names(data))
  
  if (length(annotation_col) == 0) {
    # No annotation column found, return NULL instead of stopping
    warning("No original annotation column found. Please ensure your data has one of the following column names: ",
            paste(possible_annotation_cols, collapse = ", "), ". Returning NULL.")
    return(NULL)
  } else if (length(annotation_col) > 1) {reposition_ether_lipids <- function(name) {
    # This regex looks for "(O-xx:yy)" or "(P-xx:yy)" patterns and rearranges them into "-O(xx:yy)" or "-P(xx:yy)".
    cleaned_name <- gsub("\\((O|P)-([0-9]+:[0-9]+)\\)", "-\\1(\\2)", name)
    return(cleaned_name)
  }
  warning("Multiple original annotation columns found: ", 
          paste(annotation_col, collapse = ", "), 
          ". Using the first one: ", annotation_col[1])
  annotation_col <- annotation_col[1]
  }
  
  # If we reach this point, both columns are found, proceed as before
  merged_info <- data %>%
    group_by(.data[[compound_name_col]]) %>%
    summarise(
      merged_molecules = paste(.data[[annotation_col]], collapse = ", "),
      count = n(),
      .groups = "drop"
    ) %>%
    filter(count > 1) %>%
    rename(Compound_Name = !!compound_name_col)
  
  return(merged_info)
}





#duplicated names have add _1, _2 and _3 depending on how many duplicates. 
unique_compound_names <- function(data) {
  # Ensure that 'data' is a data frame and has at least one column
  if (!is.data.frame(data) || ncol(data) < 1) {
    stop("The input must be a data frame with at least one column.")
  }
  
  # Apply the processing to the first column of 'data'
  data[[1]] <- ave(data[[1]], data[[1]], FUN = function(x) {
    if (length(x) > 1) {
      # Extract the base name without parentheses
      base_name <- sub("\\(.*\\)", "", x)
      # Extract the part within parentheses
      suffix <- sub(".*\\(", "(", x)
      # Combine base name with sequence number and the part within parentheses
      paste0(base_name, "_", seq_along(x), suffix)
    } else {
      x
    }
  })
  
  # Return the modified data
  return(data)
}




#merge duplicated names of the data
merge_duplicates <- function(data) {
  # Ensure the first column is treated as the Compound Name
  compound_name_col <- names(data)[1]
  
  # Group by the first column and then summarise all other columns by summing
  data_merged <- data %>%
    group_by(.data[[compound_name_col]]) %>%
    summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop')
  
  return(data_merged)
}




#duplicated names have add _1, _2 and _3 depending on how many duplicates. 
unique_compound_names <- function(data) {
  # Ensure that 'data' is a data frame and has at least one column
  if (!is.data.frame(data) || ncol(data) < 1) {
    stop("The input must be a data frame with at least one column.")
  }
  
  # Apply the processing to the first column of 'data'
  data[[1]] <- ave(data[[1]], data[[1]], FUN = function(x) {
    if (length(x) > 1) {
      # Extract the base name without parentheses
      base_name <- sub("\\(.*\\)", "", x)
      # Extract the part within parentheses
      suffix <- sub(".*\\(", "(", x)
      # Combine base name with sequence number and the part within parentheses
      paste0(base_name, "_", seq_along(x), suffix)
    } else {
      x
    }
  })
  
  # Return the modified data
  return(data)
}


###############
# Data cleaning
###############

# Function to extract patterns from compound names
# Removes noise, keeping only the name and length (e.g., "CAR 14:1'CAR'[M+H]+" becomes "CAR 14:1")
extract_pattern <- function(name) {
  # Updated pattern to handle various cases, preserving lipid class and removing noise
  pattern <- "([A-Za-z]+\\([O,P]?-?[0-9]+:[0-9]+(;[0-9A-Za-z<>+]+)*\\))|([A-Za-z]+\\s[O,P]?-?[0-9]+:[0-9]+)|([A-Za-z]+[O,P]?-?[0-9]+:[0-9]+)"
  
  # Extract matches based on the pattern
  matches <- regmatches(name, gregexpr(pattern, name))
  
  # Return the first match or the whole name if no match is found
  if (length(matches[[1]]) > 0) {
    return(matches[[1]][1])
  } else {
    return(name)
  }
}


# Function to clean the lipid names by removing additional content after semicolons inside parentheses
clean_lipid_name <- function(name) {
  # Remove everything after the semicolon inside parentheses, e.g., "Cer(39:2;O2)" becomes "Cer(39:2)"
  cleaned_name <- gsub("(\\([0-9]+:[0-9]+);[^)]+", "\\1", name)
  
  return(cleaned_name)
}


# Function to remove "O-" or "P-" prefixes from lipid names, but keep the rest intact
remove_ether_lipids <- function(name) {
  # Remove "O-" and "P-" inside parentheses
  cleaned_name <- gsub("\\(O-([0-9]+:[0-9]+)\\)", "(\\1)", name)  # Remove "O-" within parentheses
  cleaned_name <- gsub("\\(P-([0-9]+:[0-9]+)\\)", "(\\1)", cleaned_name)  # Remove "P-" within parentheses
  
  # Remove "O-" and "P-" outside parentheses with or without space after the lipid class
  cleaned_name <- gsub(" O-([0-9]+:[0-9]+)", " (\\1)", cleaned_name)  # Remove "O-" outside parentheses
  cleaned_name <- gsub(" P-([0-9]+:[0-9]+)", " (\\1)", cleaned_name)  # Remove "P-" outside parentheses
  
  return(cleaned_name)
}

reposition_ether_lipids <- function(name) {
  # This regex looks for "(O-xx:yy)" or "(P-xx:yy)" patterns and rearranges them into "-O(xx:yy)" or "-P(xx:yy)".
  cleaned_name <- gsub("\\((O|P)-([0-9]+:[0-9]+)\\)", "-\\1(\\2)", name)
  return(cleaned_name)
}


format_strings <- function(input_strings) {
  # Remove multiple spaces but preserve single spaces between lipid class and chain length
  formatted_strings <- gsub("\\s{2,}", " ", input_strings)
  
  # Add parentheses around the chain length numbers if they are not already inside parentheses
  # This version handles cases with or without spaces between the lipid class and the chain length
  formatted_strings <- gsub("([A-Za-z]+)\\s*([0-9]+:[0-9]+)", "\\1(\\2)", formatted_strings)
  
  return(formatted_strings)
}



# Function to filter rows based on the specified pattern, removing any data that are not in X(C:D) format
filter_data_by_pattern <- function(data) {
  # Define the regular expression pattern for the correct format
  pattern <- "^.+\\(\\d+:\\d+\\)$"
  
  # Filter rows that match the pattern (lipid name in the first column)
  filtered_data <- data[grepl(pattern, data[[1]]), ]
  
  return(filtered_data)
}



remove_patterned_rows <- function(data) {
  # Check if data is a data frame and has at least one column
  if (!is.data.frame(data) || ncol(data) < 1) {
    stop("Input must be a data frame with at least one column.")
  }
  
  # Identify rows that do not match the pattern ^.+\(\d+:\d+\)$
  filtered_data <- data[!grepl("^.+\\(\\d+:\\d+\\)$", data[[1]]), ]
  return(filtered_data)
}


