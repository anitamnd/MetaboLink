imp_median <- function(data, seq) {
  datm <- data.frame(seq$group, t(data))
  datm <- datm %>%
    group_by(seq.group) %>%
    mutate_if(
      is.numeric,
      function(x) {
        ifelse(is.na(x), median(x, na.rm = T), x)
      }
    )
  datm <- data.frame(t(datm[, -1]))
  colnames(datm) <- colnames(data)
  row.names(datm) <- row.names(data)
  return(datm)
}

imp_minx <- function(data, seq, minx) {
  datm <- data.frame(seq$group, t(data))
  datm <- datm %>%
    group_by(seq.group) %>%
    mutate_if(
      is.numeric,
      function(x) {
        ifelse(is.na(x),
          min(x, na.rm = T) / minx,
          x
        )
      }
    )
  datm <- data.frame(t(datm[, -1]))
  colnames(datm) <- colnames(data)
  row.names(datm) <- row.names(data)
  datm[datm == "Inf"] <- NA
  return(datm)
}

imputation <- function(data, seq, method, minx = 1, onlyqc, remaining) {
  if (onlyqc) {
    qcData <- data[seq[, 1] %in% "QC"]
    qcSequence <- seq[seq[, 1] %in% "QC", ]
  } else {
    qcData <- data[seq[, 1] %in% c("Sample", "QC")]
    qcSequence <- seq[seq[, 1] %in% c("Sample", "QC"), ]
  }
  qcData[qcData == 0] <- NA
  qcSequence[qcSequence[, 1] %in% c("QC"), ]$group <- "QC"
  qcSequence$group[is.na(qcSequence$group)] <- "Sample"

  if (method == "KNN") {
    qcData <- as.matrix(qcData)
    knndat <- impute.knn(qcData, k = 10, rowmax = .99, colmax = .99, maxp = 15000) # TODO skal k sættes anderledes evt mindste class -1?
    impsqdat <- as.data.frame(knndat$data)
  } else if (method == "Median") {
    impsqdat <- imp_median(qcData, qcSequence)
  } else if (method == "Min/X") {
    impsqdat <- imp_minx(qcData, qcSequence, minx)
  }

  if (sum(is.na(impsqdat)) > 0) {
    if (remaining == "Min/X") {
      for (i in 1:nrow(impsqdat)) {
        impsqdat[i, is.na(impsqdat[i, ])] <- min(impsqdat[i, ], na.rm = T) / minx
      }
    } else if (remaining == "zero") {
      for (i in 1:nrow(impsqdat)) {
        impsqdat[i, is.na(impsqdat[i, ])] <- 0
      }
    } else if (remaining == "Median") {
      for (i in 1:nrow(impsqdat)) {
        impsqdat[i, is.na(impsqdat[i, ])] <- median(as.numeric(impsqdat[i, ]), na.rm = T)
      }
    }
  }
  if (onlyqc) {
    data[seq[, 1] %in% "QC"] <- impsqdat
  } else {
    data[seq[, 1] %in% c("Sample", "QC")] <- impsqdat
  }
  return(data)
}