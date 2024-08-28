### Missing Value Filtration Functions 

#' Calculate Keep Function
#'
#' This function calculates the 'keep' variable which is used to determine which
#' rows of the data should be kept based on the sample type and cutoff.
#'
#' @param data A data frame containing the data to be filtered.
#' @param sequence A data frame containing the sequence of the data.
#' @param sample_type A character string representing the sample type to be used in the calculation.
#' @param cutoff A numeric value representing the cutoff to be used in the calculation.
#'
#' @return A logical vector indicating which rows of the data should be kept.
calculate_keep <- function(data, seq, sample_type, cutoff) {

  data_sample <- data[seq[, 1] %in% sample_type]
  data_sample[data_sample == 0] <- NA

  return(rowSums(!is.na(data_sample)) / ncol(data_sample) >= cutoff)
}


#' In Group Filtration Function
#'
#' This function performs 'in group' filtration on the data. It calculates a
#' 'keep' matrix based on the presence of non-NA values in each class of data.
#' Rows of data where any column in the 'keep' matrix is TRUE are kept.
#'
#' @param data A data frame containing the data to be filtered.
#' @param sequence A data frame containing the sequence of the data.
#' @param cutoff A numeric value representing the cutoff to be used in the filtration.
#'
#' @return A logical vector indicating which rows of the data should be kept.
in_group_filtration <- function(data, sequence, cutoff) {
  data_sample <- data[sequence[, 1] %in% "Sample"]
  data_sample[data_sample == 0] <- NA
  
  groups <- factor(sequence[, 'group'], exclude = NA)
  print(levels(groups))
  sequence_sample <- sequence[sequence[, 1] %in% "Sample", ]
  num_groups <- length(levels(groups))
  keep_matrix <- matrix(FALSE, nrow = nrow(data_sample), ncol = num_groups)

  for(group_index in 1:num_groups) {
    print(levels(groups)[group_index])
    group_data <- data_sample[, sequence_sample[, 4] %in% levels(groups)[group_index]]
    print(head(group_data))
    keep_matrix[, group_index] <- rowSums(!is.na(group_data)) / ncol(group_data) >= cutoff
  }
  keep <- apply(keep_matrix, 1, any)

  return(keep)
}


#' Cutoff Removal Function
#'
#' This function removes rows from the data based on a cutoff and a specified method. 
#' The methods can be 'entire data', 'in QC', or 'in group'.
#'
#' @param data A data frame containing the data to be filtered.
#' @param seq A data frame containing the sequence of the data.
#' @param cutoff A numeric value representing the cutoff to be used in the filtration.
#' @param method A character string representing the method to be used in the filtration.
#'
#' @return A data frame containing the filtered data.
cutoffrm <- function(data, sequence, cutoff, method) {
  cutoff <- cutoff / 100
  valid_methods <- c("entire data", "in QC", "in group")

  if (!method %in% valid_methods) {
    stop(paste("Invalid method. Expected one of:", 
              paste(valid_methods, collapse = ", ")))
  } 
  if ("entire data" %in% method) {
    keep <- calculate_keep(data, sequence, "Sample", cutoff)
  }
  if ("in QC" %in% method) {
    keep <- calculate_keep(data, sequence, "QC", cutoff)
  }
  if ("in group" %in% method) {
    keep <- in_group_filtration(data, sequence, cutoff)
  }

  data <- data[keep, ]
  return(data)
}