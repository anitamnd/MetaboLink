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
    }  else {
      "-"
    }
  })
  if(sum(labels == "Name") > 1) {
    labels[labels == "Name" & duplicated(labels)] <- "-"
  }
  if((sum(labels == "Adduct_pos") + sum(labels == "Adduct_neg")) > 1) {
    sendSweetAlert(title = "Info", text = "Multiple adduct columns found. Only one is allowed. Please check the data.", type = "info")
  }
  labels <- factor(labels, levels = c("Name", "Blank", "QC", "Sample", "RT", "Mass", "Adduct_pos", "Adduct_neg", "-"))
  return(labels)
}


checkSequence <- function(sequence) {
  columnsToCheck <- c("sample", "batch", "order", "group", "time", "paired", "amount")
  colnames(sequence)[colnames(sequence) == "class"] <- "group"
  missingColumns <- setdiff(columnsToCheck, colnames(sequence))
  if (length(missingColumns) > 0) {
    sequence[missingColumns] <- lapply(seq_along(missingColumns), function(x) sequence[missingColumns[x]] <- NA)
  }
  sequence <- sequence[, c("sample", "batch", "order", "group", "time", "paired", "amount")]
  return(sequence)
}

checkDuplicates <- function(columns) {
  has_duplicates <- any(duplicated(columns))
  if(has_duplicates) {
    sendSweetAlert(title = "Info", text = "Duplicate names are not allowed.", type = "info")
  }
  return(has_duplicates)
}

isValidName <- function(s) {
  if (grepl("^[A-Za-z0-9]+$", s) && nchar(s) <= 10) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}