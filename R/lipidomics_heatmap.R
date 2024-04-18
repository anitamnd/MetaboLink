
# Grouping the data ????? ANITA I think this function is not needed
process_data <- function(sequence, data) {
  # Initialize an empty list to store the sample names by group and other information
  results <- list()
  
  sample_identifiers <- rownames(sequence)[sequence[, "labels"] == "Sample"]
  groups <- sequence[sequence[, 'labels'] == "Sample", "class"]
  names <- sample_identifiers
  
  # Extract the 'Sample' labels and corresponding 'class' from 'sequence'
  sample_rows <- sequence[sequence$labels == "Sample", ]
  results$sample_rows <- sample_rows  # Store the sample rows in the results list
  
  unique_groups <- unique(sample_rows$class)
  results$unique_groups <- unique_groups  # Store the unique groups in the results list
  
  # Initialize an empty list within results for grouped_samples
  results$grouped_samples <- list()
  
  # Iterate over each group to get the corresponding sample names
  for (group in unique_groups) {
    if (!is.na(group)) {
      # Get the sample names for the current group
      samples_in_group <- sample_rows[sample_rows$class == group, 1]
      
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
  
  # Extract the 'Sample' labels and corresponding 'class' from 'sequence'
  sample_rows <- sequence[sequence$labels == "Sample", ]
  unique_groups <- unique(sample_rows$group, na.rm = TRUE)
  
  # Iterate over each unique group to create data frames
  for (group in unique_groups) {
    if (!is.na(group)) { #TODO not necessary
      # Get the sample identifiers for the current group
      sample_identifiers <- rownames(sample_rows)[sample_rows$class == group]
      
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




combine_data <- function(sequence, data) {

# Initialize an empty data frame to store the combined data
combined_data <- data.frame()
sample_rows <- sequence[sequence$labels == "Sample", ] #358
unique_groups <- unique(sample_rows$class)

for (group in unique_groups) {
  if (!is.na(group) && group %in% sequence$class) {
    samples_in_group <- sequence[sequence$class == group & sequence$labels == "Sample", 1]
    # Find the matching column indices, excluding NA values
    matching_indices <- match(samples_in_group, colnames(data))
    matching_indices <- matching_indices[!is.na(matching_indices)]  # Exclude NA values
    
    # Check if we have any matching columns at all
    if (length(matching_indices) == 0) {
      next  # Skip this group if no matching columns are found
    }
    
    # Extract the data for the current group's samples
    group_data <- data[, matching_indices, drop = FALSE]
    
    # Combine the extracted data for the current group with the main combined data frame
    # This assumes that all data frames have the same number of rows and row order
    if (ncol(combined_data) == 0) {
      combined_data <- group_data
    } else {
      combined_data <- cbind(combined_data, group_data)
    }
  }
}

# Now 'combined_data' contains all the sample data from each group without the group prefix
return(combined_data)
}



calculate_means_for_grouped_data <- function(grouped_data_frames) {
  # Initialize a new list to store the modified data frames
  new_grouped_data_frames <- list()
  
  # Iterate over each group's data frame in the list
  for (group_name in names(grouped_data_frames)) {
    # Clone the current group's data frame to avoid modifying the original
    group_data <- grouped_data_frames[[group_name]]
    
    # Assuming the first column is not numeric and should be excluded from the mean calculation
    # Calculate the mean for each row across all other columns
    means <- rowMeans(group_data[, drop = FALSE], na.rm = TRUE)
    
    # Append the calculated means as a new column to the cloned data frame
    group_data$Mean <- means
    
    # Add the modified data frame to the new list
    new_grouped_data_frames[[group_name]] <- group_data
  }
  
  # Return the new list of grouped data frames with means calculated
  return(new_grouped_data_frames)
}


####      ####      ####      ####      ####      ####      ####      ####      ####      ####
# Anita 
####      ####      ####      ####      ####      ####      ####      ####      ####      ####

# Function to group lipids by their class prefix (e.g., "CAR", "LP", etc.)
group_lipids_by_class <- function(data) {
  # Assuming the first column of 'data' contains the lipid names like "CAR(18:1)"
  lipid_names <- data[[1]]  # Replace 1 with the actual column name or index if different
  
  # Use a regular expression to extract the class prefix from lipid names
  # This matches any consecutive alphabetic characters at the beginning of the string
  lipid_classes <- sub("\\(([0-9]+:[0-9]+)\\).*", "", lipid_names)
  
  # Create a data frame that maps lipid names to their class
  class_mapping <- data.frame(Lipid_Name = lipid_names, Class = lipid_classes, stringsAsFactors = FALSE)
  
  # Now, split 'data' into a list of data frames, each containing only lipids of the same class
  grouped_data_lipds <- split(data, class_mapping$Class)
  
  # Optionally, if you want to return a list that names each group by its class
  # names(grouped_data) <- unique(lipid_classes)
  
  return(grouped_data_lipds)
}


# Data cleaning

# Function to extract patterns from compound names
# Removes all noise from compound name, so name and length is the only left: eg. going from "CAR 14:1'CAR'[M+H]+" to "CAR 14:1"
extract_pattern <- function(name) {
  
  # Pattern to find first part consisting of letters and numbers with a colon or a letter before the numbers
  pattern <- "([A-Za-z]+\\s[0-9]+:[0-9]+)|([A-Za-z]+\\s[[:alpha:]]?-?[0-9]+:[0-9]+)"
  matches <- regmatches(name, gregexpr(pattern, name))
  
  # Returns the first match, or the hole name if no match
  if (length(matches[[1]]) > 0) {
    return(matches[[1]][1])
  } else {
    return(name)
  }
}

# Function to format strings
# Puts the length and double bonds numbers into a (). Eg "CAR 14:1" to "CAR(14:1)"
format_strings <- function(input_strings) {
  # Use gsub with regular expression to remove all whitespace characters
  formatted_strings <- gsub("\\s+", "", input_strings)
  # Add parentheses around the numbers
  formatted_strings <- gsub("([A-Za-z]*)(\\d+):(\\d+)", "\\1(\\2:\\3)", formatted_strings)
  return(formatted_strings)
}

### pattern_column is that corret? 
# Function to filter rows based on the specified pattern, meaning removes any data that are not on X(C:D) format.
filter_data_by_pattern <- function(data) {
  # Define the regular expression pattern
  pattern <- "^.+\\(\\d+:\\d+\\)$"
  
  # Check if the first column of data matches the pattern
  filtered_data <- data[grepl(pattern, data[[1]]), ]
  
  return(filtered_data)
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