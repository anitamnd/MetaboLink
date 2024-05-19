pcaplot <- function(data, group, islog) {
  data[data == 0] <- NA
  data <- data[complete.cases(data), ]
  group[!is.na(group)] <- group[!is.na(group)]
  group[is.na(group)] <- "No class"
  shinyCatch({
      ifelse(islog, data <- t(data), data <- log(t(data)))
    },
    blocking_level = 'warning',
    shiny = FALSE
  )
  prin <- prcomp(data, rank. = 2, center = T, scale = F)
  pov <- summary(prin)[["importance"]]["Proportion of Variance", ]
  pov <- round(pov * 100, 2)
  components <- prin[["x"]]
  components <- data.frame(components)
  label <- paste0(row.names(components), ": ", group)
  col <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(length(unique(group)))
  pca <- plot_ly(components, x = ~PC1, y = ~PC2, type = "scatter", mode = "markers", text = label, hoverinfo = "text", color = group, colors = col)
  pca <- pca %>% layout(
    legend = list(title = list(text = "color")),
    plot_bgcolor = "#e5ecf6",
    xaxis = list(
      title = paste0("PC1 (", pov[1], "% explained var.)"),
      zerolinecolor = "#ffff",
      zerolinewidth = 2,
      gridcolor = "#ffff"
    ),
    yaxis = list(
      title = paste0("PC2 (", pov[2], "% explained var.)"),
      zerolinecolor = "#ffff",
      zerolinewidth = 2,
      gridcolor = "#ffff"
    )
  )
  return(pca)
}