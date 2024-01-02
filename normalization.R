# Normalization

probQuotientNormalization <- function(x, reference) {
  x/median(as.numeric(x/reference), na.rm=TRUE)
}

medianNormalization <- function(x) {
  x/median(x, na.rm=TRUE)
}

sumNormalization <- function(x){
  1000*x/sum(x, na.rm=TRUE)
}

normalization <- function(data, sequence, qualityControls, method) {
  filteredData <- data[, sequence[, 1] %in% c("QC", "Sample")]
  rowNames <- rownames(filteredData)
  colNames <- colnames(filteredData)
  if(method == "QC (PQN)") {
    meanQC <- rowMeans(qualityControls)
    normalizedData <- apply(filteredData, 2, probQuotientNormalization, meanQC)
  } else if(method == "Median") {
    normalizedData <- apply(filteredData, 2, medianNormalization)
  } else if(method == "Sum") {
    normalizedData <- apply(filteredData, 2, sumNormalization)
  }
  rownames(normalizedData) <- rowNames
  colnames(normalizedData) <- colNames
  return(normalizedData)
}

# Transformation
meanCenter <- function(x) {
  x - mean(x)
}
autoNorm <- function(x) {
  (x - mean(x))/sd(x, na.rm=T)
}

selectLogMethod <- function(data, method) {
  switch(method,
    log2 = log2(data),
    log10 = log10(data),
    ln = log(data),
    None = data
  )
}

selectScalingMethod <- function(data, method) {
  switch(method,
    "Mean center" = t(apply(data, 1, meanCenter)),
    "Auto scale" = t(apply(data, 1, autoNorm)),
    "None" =  data
  )
}

cleanData <- function(data) {
  if(sum(data==Inf, na.rm=TRUE)>0){
    inx <- data == Inf;
    data[inx] <- NA;
    data[inx] <- max(data, na.rm=T)*2
  }
  if(sum(data==-Inf, na.rm=TRUE)>0){
    inx <- data == -Inf;
    data[inx] <- NA;
    data[inx] <- min(data, na.rm=T)/2
  }
  return(data)
}

logTransform <- function(data, sequence, method) {
  filtered <- data[, sequence[, 1] %in% c("QC", "Sample")]
  filtered[is.na(filtered)] <- 0

  transformed <- selectLogMethod(filtered, method)
  rownames(transformed) <- rownames(filtered)
  colnames(transformed) <- colnames(filtered)
  
  clean <- cleanData(transformed)
  return(clean)
}

scaleData <- function(data, sequence, method) {
  filtered <- data[, sequence[, 1] %in% c("QC", "Sample")]
  filtered[is.na(filtered)] <- 0

  scaled <- selectScalingMethod(filtered, method)
  rownames(scaled) <- rownames(filtered)
  colnames(scaled) <- colnames(filtered)
  
  clean <- cleanData(scaled)
  return(clean)
}

transformation <- function(data, sequence, logMethod, scaleMethod) {
  filtered <- data[, sequence[, 1] %in% c("QC", "Sample")]
  filtered[is.na(filtered)] <- 0

  transformed <- selectLogMethod(filtered, logMethod)
  scaled <- selectScalingMethod(transformed, scaleMethod)
  rownames(scaled) <- rownames(filtered)
  colnames(scaled) <- colnames(filtered)
  
  clean <- cleanData(scaled)
  return(clean)
}