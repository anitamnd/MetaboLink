# Functions to select data

select_feature_names <- function(dataset, labels) {
  return(dataset[, labels == "Name"])
}

select_mass <- function(dataset, labels) {
  return(dataset[, labels == "Mass"])
}

select_retention_time <- function(dataset, labels) {
  return(dataset[, labels == "RT"])
}

select_adducts <- function(dataset, labels) {
  return(dataset[, labels %in% c("Adduct_pos", "Adduct_neg")])
}

select_quality_controls <- function(dataset, labels) {
  return(dataset[, labels == "QC"])
}

select_samples <- function(dataset, labels) {
  return(dataset[, labels == "Sample"])
}

select_samples_and_QCs <- function(dataset, labels) {
  return(dataset[, labels %in% c("Sample", "QC")])
}

select_blanks <- function(dataset, labels) {
  return(dataset[, labels == "Blank"])
}

select_mass_and_adduct <- function(dataset, labels, ion_mode) {
  mass <- dataset[, labels == "Mass"]
  adduct <- dataset[, labels == paste("Adduct", ion_mode, sep = "_")]
  return(list(mass = mass, adduct = adduct))
}

determine_ion_mode <- function(labels) {
  return(ifelse("Adduct_pos" %in% labels, "pos", "neg"))
}

detect_internal_standards <- function(data, sequence) {
  names <- sequence[, 1] == "Name"
  is_internal_standard <- grepl("\\(IS\\)", toupper(data[, names]))
  return(is_internal_standard)
}