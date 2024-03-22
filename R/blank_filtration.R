#' Blank Filtration Function
#'
#' This function filters the data based on the signal strength comparison between blanks and quality controls (QCs).
#' It can optionally include internal standards in the filtered data.
#'
#' @param data A data frame containing the data to be filtered.
#' @param sequence A data frame containing the sequence of the data.
#' @param signalStrength A numeric value representing the signal strength to be used in the filtration.
#' @param keepIS A boolean value indicating whether to keep internal standards in the filtered data.
#'
#' @return A data frame or matrix containing the filtered data.
blankFiltration <- function(data, sequence, signalStrength, keepIS) {
  blanks <- sequence[, 1] %in% "Blank"
  qcs <- sequence[, 1] %in% "QC"

  blankMeans <- apply(data[blanks], 1, mean, na.rm = TRUE) * signalStrength
  qcMeans <- apply(data[qcs], 1, mean, na.rm = TRUE)

  bf <- blankMeans < qcMeans # Filter based on signal strength comparison

  if(keepIS) {
    isInternalStandard <- detect_internal_standards(data, sequence)
    filter <- bf | isInternalStandard # Combine filters
  } else {
    filter <- bf
  }
  return(data[filter, ])
}


#' Apply Blank Filtration Function
#'
#' This function applies the blank filtration to the data and optionally discards the blanks from the filtered data.
#'
#' @param data A data frame containing the data to be filtered.
#' @param sequence A data frame containing the sequence of the data.
#' @param signalStrength A numeric value representing the signal strength to be used in the filtration.
#' @param keepIS A boolean value indicating whether to keep internal standards in the filtered data.
#' @param discardBlank A boolean value indicating whether to discard the blanks from the filtered data.
#'
#' @return A list containing the filtered data and the sequence.
applyBlankFiltration <- function(data, sequence, signalStrength, keepIS, discardBlank) {
  
  filteredData <- blankFiltration(data, sequence, signalStrength, keepIS)

  if(discardBlank) {
    nonBlankIndices <- !sequence[, 1] %in% "Blank"
    filteredData <- filteredData[, nonBlankIndices]
    sequence <- sequence[nonBlankIndices, ]
  }

  return(list(data = filteredData, sequence = sequence))
}