selectGroups <- function(data, sequence, groups) {
  selected <- data[, sequence[, 4] %in% groups]
  return(selected)
}


cvmean <- function(data) {
  mean(apply(data, 1, sd, na.rm = T) / apply(data, 1, mean, na.rm = T), na.rm = T) * 100
}

cv <- function(data) {
  round(apply(data, 1, sd, na.rm = T) / apply(data, 1, mean, na.rm = T) * 100, 2)
}


windowselect <- function(input) {
  hide("welcome_panel")
  if (input == "pca") show("pca_panel") else hide("pca_panel")
  if (input == "drift") show("drift_panel") else hide("drift_panel")
  if (input == "feature") show("boxplot_panel") else hide("boxplot_panel")
  if (input == "sequence") show("sequence_panel") else hide("sequence_panel")
  if (input == "datatable") show("datatable_panel") else hide("datatable_panel")
  if (input == "statistics") show("statistics_panel") else hide("statistics_panel")
  if (input == "export") show("export_panel") else hide("export_panel")
  if (input == "info") show("info_panel") else hide("info_panel")
}