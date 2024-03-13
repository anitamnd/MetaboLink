performRFCorrection <- function(data, qcOrder, ntree, frame, totalRows) {
  dcdata <- data
  for (i in seq_len(dcdata)) {
    forest <- randomForest(data.frame(qcOrder), as.numeric(dcdata[i, qcOrder]), ntree = ntree, na.action = na.roughfix)
    predicted <- predict(forest, frame)
    dcdata[i, ] <- as.numeric(dcdata[i, ]) / predicted
    updateProgressBar(id = "pbdc", value = i, total = totalRows)
  }
  return(dcdata)
}

performLOESSCorrection <- function(dcdat, qcid, degree, QCspan, frame, totalRows) {
  for (i in seq_len(dcdat)) {
    loessFit <- loess(dcdat[i, qcid] ~ qcid, span = QCspan, degree = degree, na.action = na.roughfix)
    pv <- predict(loessFit, frame)
    dcdat[i, ] <- as.numeric(dcdat[i, ]) / pv
    updateProgressBar(id = "pbdc", value = i, total = totalRows)
  }
  return(dcdat)
}

driftCorrection <- function(data, sequence, method, ntree = 500, degree = 2, QCspan) {
  seqsq <- sequence[sequence[, 1] %in% c("Sample", "QC"), ]
  datsq <- data[, sequence[, 1] %in% c("Sample", "QC")]
  
  datsqsorted <- datsq[order(seqsq$order)] #TODO ordered samples (check that it doesnt skip numbers)

  qcid <- sort(as.numeric(seqsq[seqsq[, 1] == "QC", "order"]))

  frame <- data.frame("qcid" = 1:ncol(datsqsorted)) 
  dcdat <- as.matrix(datsqsorted)

  progressSweetAlert(id = "pbdc", title = "Correcting drift", value = 0, total = nrow(dcdat), striped = TRUE, display_pct = TRUE)

  if (method == "QC-RFSC (random forest)") {
    dcdat <- performRFCorrection(dcdat, qcid, ntree, frame, nrow(dcdat))
  } else if (method == "QC-RLSC (robust LOESS)") {
    dcdat <- performLOESSCorrection(dcdat, qcid, degree, QCspan, frame, nrow(dcdat))
  }

  data[, sequence[, 1] %in% c("Sample", "QC")] <- dcdat[, seqsq$order]
  closeSweetAlert()
  return(data)
}
