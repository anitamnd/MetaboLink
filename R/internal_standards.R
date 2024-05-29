normalizationIS <- function(data, sequence, is, method, qc) {
  sendSweetAlert(title = "Normalizing...", text = "This modal will close automatically when done. Please wait...", type = "info")
  rt <- which(sequence[, 1] == "RT")
  isname <- is
  is <- as.numeric(gsub(" .*$", "", is))
  sel <- if (qc) c("Sample", "QC") else "Sample"
  sdat <- data[sequence[, 1] %in% sel]
  sdat[sdat == 0] <- NA
  is <- is[complete.cases(sdat[is, ])]
  if(length(is) == 0) {
    closeSweetAlert()
    return(NULL)
  }
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
  closeSweetAlert()
  return(data)
}


normalizationIS_2 <- function(data, sequence, internalStandards, method, qc) {
  retention_time_index <- which(sequence[, 1] == "RT")
  is_names <- internalStandards
  is_index <- as.numeric(gsub(" .*$", "", internalStandards))

  selected_samples <- if (qc) c("Sample", "QC") else "Sample"
  sample_data <- data[sequence[, 1] %in% selected_samples]
  sample_data[sample_data == 0] <- NA

  is_index <- is_index[complete.cases(sample_data[is_index, ])]

  # Find the nearest internal standard
  nearest_rt_index <- sapply(data[, retention_time_index], function(y) {
    which.min(abs(data[is_index, retention_time_index] - y))
  })
  # If the method is "Same lipid structure", find the nearest internal standard with the same lipid structure
  if (method == "Same lipid structure") {
    lipid_names <- data[sequence[, 1] %in% "Name"]
    # Extract the lipid type from the internal standard names
    is_lipid_types <- gsub(" .*$", "", lipid_names[is_index, ])

    nearest_rt_index <- sapply(seq(lipid_names[, 1]), function(x) {
      if (gsub(" .*$", "", lipid_names[x, 1]) %in% is_lipid_types) {
        which(is_lipid_types %in% gsub(" .*$", "", lipid_names[x, 1]))
      } else {
        nearest_rt_index[x]
      }
    })
  }

  # Normalize the data
  sample_data <- sapply(seq(ncol(sample_data)), function(j) {
    sapply(seq(nrow(sample_data)), function(i) {
      sample_data[i, j] <- sample_data[i, j] / sample_data[is_values, j][nearest_rt_index[i]]
    })
  })
  used_is_names <- sapply(seq(nrow(sample_data)), function(x) {
    is_names[nearest_rt_index[x]]
  })
  data[sequence[, 1] %in% selected_samples] <- sample_data
  data <- cbind(data, data.frame(isnorm = used_is_names))
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
