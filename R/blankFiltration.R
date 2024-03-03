detectInternalStandards <- function(data, sequence) {
  namesColumn <- sequence[, 1] == "Name"
  grepl("\\(IS\\)", toupper(data[, namesColumn]))
}

blankFiltration <- function(data, sequence, signalStrength, keepIS) {
  blanks <- sequence[, 1] %in% "Blank"
  qcs <- sequence[, 1] %in% "QC"

  blankMeans <- apply(data[blanks], 1, mean, na.rm = TRUE) * signalStrength
  qcMeans <- apply(data[qcs], 1, mean, na.rm = TRUE)

  bf <- blankMeans < qcMeans # Filter based on signal strength comparison

  if(keepIS) {
    isInternalStandard <- detectInternalStandards(data, sequence)
    filter <- bf | isInternalStandard # Combine filters
  } else {
    filter <- bf
  }
  return(data[filter, ])
}

applyBlankFiltration <- function(data, sequence, signalStrength, keepIS, discardBlank) {
  filteredData <- blankFiltration(data, sequence, signalStrength, keepIS)

  if(discardBlank) {
    nonBlankIndices <- !sequence[, 1] %in% "Blank"
    filteredData <- filteredData[, nonBlankIndices]
    sequence <- sequence[nonBlankIndices, ]
  }

  return(list(data = filteredData, sequence = sequence))
}
