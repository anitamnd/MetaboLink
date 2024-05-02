blankFiltration <- function(data, sequence, signalStrength, keepIs) {
  data[sequence[, 1] %in% "Blank"][is.na(data[sequence[, 1] %in% "Blank"])] <- 0
  bf <- apply(data[sequence[, 1] %in% "Blank"], 1, mean) * signalStrength < 
                    apply(data[sequence[, 1] %in% "QC"], 1, mean, na.rm = TRUE)
  if (keepIs) { #TODO
    is <- grepl("\\(IS\\)", toupper(data[sequence[, 1] %in% "Name"][, 1]))
    data <- data[bf | is, ]
  } else {
    data <- data[bf, ]
  }
  return(data)
}