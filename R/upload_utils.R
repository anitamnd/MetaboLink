###

#' Identify Labels Function
#'
#' This function identifies the labels for each column in the data based on the column names and types. 
#' It also checks for duplicates and sends an alert if multiple columns are labeled as "Name", "RT", "Mass", "Adduct_pos", or "Adduct_neg".
#'
#' @param data A data frame representing the data.
#'
#' @return A factor vector representing the labels for each column in the data.
identifyLabels <- function(data) {
    labels <- sapply(names(data), function(x) {
        if (grepl("BLANK", toupper(x), fixed = TRUE) && is.numeric(data[, x])) {
            "Blank"
        } else if (grepl("QC", toupper(x), fixed = TRUE) && is.numeric(data[, x])) {
            "QC"
        } else if (grepl("NAME", toupper(x), fixed = TRUE)) {
            "Name"
        } else if (grepl("MASS|M/Z|M.Z", toupper(x)) && is.numeric(data[, x])) {
            "Mass"
        } else if (grepl("RT|TIME|RETENTION", toupper(x)) && is.numeric(data[, x])) {
            "RT"
        } else if (grepl("ADDUCT_POS", toupper(x), fixed = TRUE)) {
            "Adduct_pos"
        } else if (grepl("ADDUCT_NEG", toupper(x), fixed = TRUE)) {
            "Adduct_neg"
        } else if (grepl("\\b(ADDUCT|ION|IONS)\\b", toupper(x)) && any(grepl("\\]\\+", data[, x]))) {
            "Adduct_pos"
        } else if (grepl("\\b(ADDUCT|ION|IONS)\\b", toupper(x)) && any(grepl("\\]\\-", data[, x]))) {
            "Adduct_neg"
        } else if (grepl("[[:digit:]]", toupper(x)) && is.numeric(data[, x])) {
            "Sample"
        } else {
            "-"
        }
    })

    # Ensure that there is only one column labeled as "Name", "RT", and "Mass"
    duplicatesCheck <- function(label) {
        if (sum(labels == label) > 1) {
            labels[labels == label & duplicated(labels)] <- "-"
        }
    }
    
    duplicatesCheck("Name")
    duplicatesCheck("RT")
    duplicatesCheck("Mass")
    
    if ((sum(labels == "Adduct_pos") + sum(labels == "Adduct_neg")) > 1) {
        sendSweetAlert(title = "Info", text = "Multiple adduct columns found. Only one is allowed. Please check the data.", type = "info")
    }
    
    labels <- factor(labels, levels = c("Name", "Blank", "QC", "Sample", "RT", "Mass", "Adduct_pos", "Adduct_neg", "-"))
    return(labels)
}


#' Check Sequence Function
#'
#' This function checks the sequence data for missing columns. If any columns are missing, it adds them with NA values. It also renames the "class" column to "group" and reorders the columns.
#'
#' @param sequence A data frame representing the sequence data.
#'
#' @return A data frame representing the checked and modified sequence data.
checkSequence <- function(sequence) {
  columnsToCheck <- c("sample", "batch", "order", "group", "time", "paired", "amount")
  colnames(sequence)[colnames(sequence) == "class"] <- "group"
  missingColumns <- setdiff(columnsToCheck, colnames(sequence))

  if (length(missingColumns) > 0) {
    sequence[missingColumns] <- NA
  }

  sequence <- sequence[, c("sample", "batch", "order", "group", "time", "paired", "amount")]
  return(sequence)
}


#' Check Duplicates Function
#'
#' This function checks for duplicates in the provided columns. If any duplicates are found, it sends a warning alert.
#'
#' @param columns A character vector representing the columns to check for duplicates.
#'
#' @return A logical value indicating whether any duplicates were found.
checkDuplicates <- function(columns) {
  has_duplicates <- any(duplicated(columns))
  if(has_duplicates) {
    sendSweetAlert(title = "Info", text = "Duplicate names are not allowed.", type = "warning")
  }
  return(has_duplicates)
}


#' Is Valid Name Function
#'
#' This function checks whether a provided string is a valid name. A valid name contains only alphanumeric characters and is no more than 10 characters long.
#'
#' @param s A character string representing the name to check.
#'
#' @return A logical value indicating whether the name is valid.
isValidName <- function(s) {
  if (grepl("^[A-Za-z0-9]+$", s) && nchar(s) <= 10) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}