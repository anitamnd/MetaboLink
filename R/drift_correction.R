### Drift correction functions

#' Perform RF Correction Function
#'
#' This function performs a drift correction on the data using a Random Forest (RF) model. It trains the model on the QC order and the corresponding data values, then uses the model to predict values for the entire data. The original data values are then divided by the predicted values to perform the correction.
#'
#' @param data A numeric matrix representing the data to be corrected.
#' @param qcOrder A numeric vector representing the order of the QC samples.
#' @param ntree An integer representing the number of trees to grow in the Random Forest model.
#' @param frame A data frame to be used for prediction.
#' @param totalRows An integer representing the total number of rows in the data.
#'
#' @return A numeric matrix representing the corrected data.
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


#' Perform LOESS Correction Function
#'
#' This function performs a drift correction on the data using a LOESS model. It fits the model to the data and the corresponding QC IDs, then uses the model to predict values for the entire data. The original data values are then divided by the predicted values to perform the correction.
#'
#' @param dcdat A numeric matrix representing the data to be corrected.
#' @param qcid A numeric vector representing the IDs of the QC samples.
#' @param degree An integer representing the degree of the polynomial to be used in the LOESS model.
#' @param QCspan A numeric value representing the span of the LOESS model.
#' @param frame A data frame to be used for prediction.
#' @param totalRows An integer representing the total number of rows in the data.
#'
#' @return A numeric matrix representing the corrected data.
performLOESSCorrection <- function(dcdat, qcid, degree, QCspan, frame, totalRows) {
  for (i in seq_len(dcdat)) {
    loessFit <- loess(dcdat[i, qcid] ~ qcid, span = QCspan, degree = degree, na.action = na.roughfix)
    pv <- predict(loessFit, frame)
    dcdat[i, ] <- as.numeric(dcdat[i, ]) / pv
    updateProgressBar(id = "pbdc", value = i, total = totalRows)
  }
  return(dcdat)
}


#' Drift Correction Function
#'
#' This function performs a drift correction on the data using either a Random Forest or a LOESS model. The method of correction is specified by the 'method' parameter. The function first sorts the data based on the order in the sequence, then performs the correction using the specified method. After the correction, the function reorders the data back to its original order.
#'
#' @param data A numeric matrix representing the data to be corrected.
#' @param sequence A data frame representing the sequence of the data.
#' @param method A character string representing the method to be used for the correction. Can be 'QC-RFSC (random forest)' or 'QC-RLSC (robust LOESS)'.
#' @param ntree An integer representing the number of trees to grow in the Random Forest model. Only used if method is 'QC-RFSC (random forest)'.
#' @param degree An integer representing the degree of the polynomial to be used in the LOESS model. Only used if method is 'QC-RLSC (robust LOESS)'.
#' @param QCspan A numeric value representing the span of the LOESS model. Only used if method is 'QC-RLSC (robust LOESS)'.
#'
#' @return A numeric matrix representing the corrected data.
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
