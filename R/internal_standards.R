normalizationIS <- function(data, sequence, is, method, qc) {
  progressSweetAlert(id = "pb", title = "Work in progress...", display_pct = T, value = 0, striped = T)
  rt <- which(sequence[, 1] == "RT")
  isname <- is
  is <- as.numeric(gsub(" .*$", "", is))
  sel <- if (qc) c("Sample", "QC") else "Sample"
  sdat <- data[sequence[, 1] %in% sel]
  updateProgressBar(id = "pb", value = 10)
  sdat[sdat == 0] <- NA
  is <- is[complete.cases(sdat[is, ])]
  near <- sapply(data[, rt], function(y) {
    which.min(abs(data[is, rt] - y))
  })
  updateProgressBar(id = "pb", value = 20)
  if (method == "Same lipid structure") {
    name <- data[sequence[, 1] %in% "Name"]
    istype <- gsub(" .*$", "", name[is, ])
    near <- sapply(seq(name[, 1]), function(x) {
      if (gsub(" .*$", "", name[x, 1]) %in% istype) {
        which(istype %in% gsub(" .*$", "", name[x, 1]))
      } else {
        near[x]
      }
    })
  }
  updateProgressBar(id = "pb", value = 30)
  sdat <- sapply(seq(ncol(sdat)), function(j) {
    sapply(seq(nrow(sdat)), function(i) {
      sdat[i, j] <- sdat[i, j] / sdat[is, j][near[i]]
    })
  })
  updateProgressBar(id = "pb", value = 60)
  isnorm <- sapply(seq(nrow(sdat)), function(x) {
    isname[near[x]]
  })
  data[sequence[, 1] %in% sel] <- sdat
  updateProgressBar(id = "pb", value = 100)
  data <- cbind(data, data.frame(isnorm = isnorm))
  closeSweetAlert()
  return(data)
}

findInternalStandards <- function(data) {
  isIndex <- grepl("\\(IS\\)", toupper(data[, 1]))
  if (sum(isIndex) > 0) {
    featureName <- as.vector(data[isIndex, 1])
    internalStandards <- paste(which(isIndex), " - ", featureName)
    return(internalStandards)
  } else {
    return(character(0))
  }
}
