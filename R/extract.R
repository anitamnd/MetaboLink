# Functions to filter dataset columns

extractFeatureNames <- function(dataset, labels) {
  return(dataset[, labels == "Name"])
}

extractMass <- function(dataset, labels) {
  return(dataset[, labels == "Mass"])
}

extractRetentionTime <- function(dataset, labels) {
  return(dataset[, labels == "RT"])
}

extractAdducts <- function(dataset, labels) {
  return(dataset[, labels %in% c("Adduct_pos", "Adduct_neg")])
}

extractQCs <- function(dataset, labels) {
  return(dataset[, labels == "QC"])
}

extractSamples <- function(dataset, labels) {
  return(dataset[, labels == "Sample"])
}

extractSamplesAndQCs <- function(dataset, labels) {
  return(dataset[, labels %in% c("Sample", "QC")])
}

extractBlanks <- function(dataset, labels) {
  return(dataset[, labels == "Blank"])
}