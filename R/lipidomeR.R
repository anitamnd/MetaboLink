

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


### REFACTORING
extract_pattern <- function(name) {
  # Pattern to find first part consisting of letters and numbers with a colon or a letter before the numbers
  pattern <- "([A-Za-z]+\\s[0-9]+:[0-9]+)|([A-Za-z]+\\s[[:alpha:]]?-?[0-9]+:[0-9]+)"
  matches <- regmatches(name, gregexpr(pattern, name))
  
  # Returns the first match, or the original name if no match is found
  return(ifelse(length(matches[[1]]) > 0, matches[[1]][1], name))
}

format_lipid_name <- function(lipid_name) {
  # Remove all whitespaces
  formatted_name <- gsub("\\s+", "", lipid_name)
  # This regex matches the desired pattern and removes any characters following the lipid specification
  formatted_name <- gsub("([A-Za-z]*)(\\d+):(\\d+)", "\\1(\\2:\\3)", formatted_name)
  return(formatted_name)
}

rename_duplicates <- function(names) {
  # Use ave() to apply a function to add suffixes e.g., X_1(C:D), X_2(C:D), etc.
  formatted_names <- ave(names, names, FUN = function(x) {
    if (length(x) > 1) {
      # Split base and parentheses, then insert sequence numbers
      base <- gsub("\\(.*\\)", "", x)
      suffix <- gsub(".*\\(", "(", x)
      x <- paste0(base, "_", seq_along(x), suffix)
    }
    return(x)
  })
  return(formatted_names)
}

validate_names <- function(names) {  
  valid_names <- grepl("^.+\\(\\d+:\\d+\\)$", names)
  return(valid_names)
}

get_grouped_mean <- function(data, sequence) {
  groupMeans <- data.frame(
    row.names = rownames(data)
  )
  for(group in unique(na.omit(sequence$group))) {
    group_data <- data[sequence$group %in% group]
    mean <- rowMeans(group_data, na.rm=TRUE)
    groupMeans[group] <- mean
  }
  return(groupMeans)
}
