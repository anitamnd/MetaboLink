pcaplot <- function(data, group, islog) {
  # Validate input data
  if (ncol(data) < 2 || nrow(data) < 2) {
    stop("PCA requires data with at least two variables and two observations.")
  }
  if (length(unique(group)) <= 1) {
    stop("PCA requires at least two different groups.")
  }

  # Replace zeros with NA and prepare data for PCA
  data[data == 0] <- NA
  data <- t(data)  # Transpose data for PCA
  
  # Apply log transformation if specified
  if (islog) {
    data <- log(data)
  }

  # Ensure data has complete cases and corresponding groups are aligned
  completeRows <- complete.cases(data)
  data <- data[completeRows, ]
  group <- group[completeRows]

  # Ensure there's no 'NA' class left unattended
  group[is.na(group)] <- "No class"

  # Perform PCA, catching potential errors
  tryCatch({
    prin <- prcomp(data, rank. = 2, center = TRUE, scale = FALSE)
  }, error = function(e) {
    stop("Error in PCA computation: ", e$message)
  })

  # Extract variance explained by the first two principal components
  pov <- round(prin$sdev^2 / sum(prin$sdev^2) * 100, 2)
  
  # Prepare data for plotting
  components <- data.frame(prin$x[, 1:2])
  components$group <- group
  components$label <- paste(row.names(components), ": ", group)

  # Dynamically assign colors to groups
  uniqueGroups <- unique(group)
  col <- colorRampPalette(RColorBrewer::brewer.pal(min(8, length(uniqueGroups)), "Set1"))(length(uniqueGroups))
  groupColors <- setNames(col, uniqueGroups)

  # Create the PCA plot
  pcaPlot <- plot_ly(data = components, x = ~PC1, y = ~PC2, type = 'scatter', mode = 'markers', 
                     text = ~label, hoverinfo = "text", color = ~group, colors = groupColors) %>%
            layout(title = 'PCA Plot',
                   xaxis = list(title = paste0("PC1 (", pov[1], "% variance)")),
                   yaxis = list(title = paste0("PC2 (", pov[2], "% variance)")),
                   plot_bgcolor = "#e5ecf6", 
                   legend = list(title = list(text = "Group")),
                   hovermode = 'closest')

  return(pcaPlot)
}
