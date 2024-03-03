cutoffrm <- function(data, seq, cutoff, method) {
  cutoff <- cutoff / 100
  if ("entire data" %in% method) {
    datm <- data[seq[, 1] %in% "Sample"]
    datm[datm == 0] <- NA
    keep <- rowSums(!is.na(datm)) / ncol(datm) >= cutoff
  }
  if ("in QC" %in% method) {
    datm <- data[seq[, 1] %in% "QC"]
    datm[datm == 0] <- NA
    keep <- rowSums(!is.na(datm)) / ncol(datm) >= cutoff
  }
  if ("in group" %in% method) {
    datm <- data[seq[, 1] %in% "Sample"]
    datm[datm == 0] <- NA
    classes <- factor(seq[, 4], exclude = NA)
    nseq <- seq[seq[, 1] %in% "Sample", ]
    keep_m <- matrix(FALSE, nrow(datm), ncol = length(levels(classes)))
    for (cl in 1:length(levels(classes))) {
      cl_f <- datm[, nseq[, 4] %in% levels(classes)[cl]]
      keep_m[, cl] <- rowSums(!is.na(cl_f)) / ncol(cl_f) >= cutoff
    }
    keep <- apply(keep_m, 1, function(x) any(x))
  }
  data <- data[keep, ]
  return(data)
}