normalizationIS <- function(data, sequence, is, method, qc) {
  rt <- which(sequence[, 1] == "RT")
  isname <- is
  is <- as.numeric(gsub(" .*$", "", is))
  sel <- if (qc) c("Sample", "QC") else "Sample"
  sdat <- data[sequence[, 1] %in% sel]
  sdat[sdat == 0] <- NA
  is <- is[complete.cases(sdat[is, ])]
  near <- sapply(data[, rt], function(y) {
    which.min(abs(data[is, rt] - y))
  })
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
  sdat <- sapply(seq(ncol(sdat)), function(j) {
    sapply(seq(nrow(sdat)), function(i) {
      sdat[i, j] <- sdat[i, j] / sdat[is, j][near[i]]
    })
  })
  isnorm <- sapply(seq(nrow(sdat)), function(x) {
    isname[near[x]]
  })
  data[sequence[, 1] %in% sel] <- sdat
  data <- cbind(data, data.frame(isnorm = isnorm))
  return(data)
}

optimizeIS <- function(data, sequence, is, method, qc) {
  iscomb <- Map(combn, list(is), seq_along(is), simplify = FALSE)
  iscomb <- lapply(rapply(iscomb, enquote, how = "unlist"), eval)
  progressSweetAlert(id = "pbis", title = "Finding best IS combination", value = 0, total = length(iscomb), striped = T, display_pct = T)
  islow <- lapply(iscomb, function(x) {
    isdat <- normalizationIS(data, sequence, x, method, qc)
    mean(apply(isdat[, sequence[, 1] %in% "QC"], 1, sd, na.rm = T) / apply(isdat[, sequence[, 1] %in% "QC"], 1, mean, na.rm = T) * 100)
    # updateProgressBar(id = "pbis", value = which(iscomb %in% iscomb[x]), total = length(iscomb))
  })
  closeSweetAlert()
  return(unlist(iscomb[which.min(unlist(islow))]))
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
