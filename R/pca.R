pcaplot <- function(data, sequence, islog) {
  # Modify the sequence$sample vector to prepend "X" only to numeric labels
  rownames(sequence) <- ifelse(grepl("^[0-9]+$", rownames(sequence)), 
                               paste0("X", rownames(sequence)), 
                               rownames(sequence))
  
  # Perform data cleaning
  data[data == 0] <- NA
  data <- na.omit(data)
  
  # Check if sequence$sample is identical to colnames(data)
  # shinyCatch({
  #   identical_check <- if (identical(rownames(sequence), colnames(data))) {
  #     "The two are identical."
  #     } else {
  #       "The two are NOT identical."
  #       }
  #   print(identical_check)  # Print result of the comparison
  #   },
  #   blocking_level = 'warning',  # Block warnings
  #   shiny = FALSE  # Do not run in shiny
  #   )
  
  # Check if data is log-transformed 
  shinyCatch({ # Catch shiny errors
    ifelse(islog,
           data <- as.data.frame(t(data)),
           data <- as.data.frame(log(t(data)))) # Log transform data
  },
  blocking_level = 'warning', # Block warnings
  shiny = FALSE # Do not run in shiny
  )
  
  # run PCA 
  pca_results <- prcomp(data, rank. = NULL, center = T, scale = F)
  # Calculate proportion of variance in percentage
  pov <- summary(pca_results)[["importance"]]["Proportion of Variance", ] * 100
  # Calculate cumulative proportion of variance
  cpov <- cumsum(pov)
  # Extract components
  components <- pca_results[["x"]]
  components <- as.data.frame(components)
  # make a data frame with the components
  label <- paste0(row.names(components), ": ", sequence)
  # Make color palette
  col <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(length(unique(sequence$group)))
  
  # Create a data frame with the sample
  pca_df <- data.frame(
    sample = row.names(components),
    group = as.factor(sequence$group),
    PC1 = pca_results$x[, "PC1"],
    PC2 = pca_results$x[, "PC2"])
  
  # Create a data frame with the components
  PC_df <- data.frame(
    PCs = 1:length(pov),
    `Variance Explained` = pov,
    `Cumulative Variance Explained` = cpov,
    check.names = FALSE)
  
  # Make a scree plot 
  scree_plot <- ggplot(PC_df, aes(x = PCs)) + 
    # Variance Explained
    geom_point(aes(y = `Variance Explained`, color = "Variance Explained"), size = 2) + 
    geom_line(aes(y = `Variance Explained`, color = "Variance Explained"), linewidth = 1, linetype = "dashed") +
    # Cumulative Variance Explained
    geom_point(aes(y = `Cumulative Variance Explained`, color = "Cumulative Variance Explained"), size = 2) +
    geom_line(aes(y = `Cumulative Variance Explained`, color = "Cumulative Variance Explained"), linewidth = 1,linetype = "solid") +
    # Labels and title
    labs(title = "Scree Plot with Variance Explained and Cumulative Variance",
         x = "Principal Components",
         y = "Variance Explained (%)") +
    # Customizing theme and colors
    scale_color_manual(values = c("Variance Explained" = "blue",
                                  "Cumulative Variance Explained" = "red")) +
    scale_x_continuous(breaks = seq(0, max(PC_df$PC, na.rm = TRUE)+1, by = 5)) + 
    theme_bw() + 
    theme(
      legend.title = element_text(size = 10), # Change to desired title size
      legend.text = element_text(size = 5) # Change to desired text size
    )
  scree_plot_plotly <- ggplotly(scree_plot)
  
  # Create the PCA plot with rounded hover text for PC1 and PC2
  pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = group)) + 
    geom_point(size = 1) + 
    labs( 
      x = paste0("PC1 (", round(PC_df[1,2], digits = 2), "% explained var.)"),
      y = paste0("PC2 (", round(PC_df[2,2], digits = 2), "% explained var.)")) +
    theme_bw() +
    scale_color_manual(values = col)
  
  # Convert ggplot to plotly object and add hover text
  pca_plot_plotly <- ggplotly(pca_plot, tooltip = c("x", "y", "color")) %>%
    layout(
      # Legend settings
      legend = list(
        title = list(text = "Color") # Set the title of the legend
      ),
      # Set plot background color
      plot_bgcolor = "#e5ecf6",
      # X-axis customization
      xaxis = list(
        title = paste0("PC1 (", round(PC_df[1,2], digits = 2), "% explained var.)"), # Title for PC1
        zerolinecolor = "#ffff", # Zero line color
        zerolinewidth = 2,       # Zero line width
        gridcolor = "#ffff"       # Grid line color
      ),
      # Y-axis customization
      yaxis = list(
        title = paste0("PC2 (", round(PC_df[2,2], digits = 2), "% explained var.)"), # Title for PC2
        zerolinecolor = "#ffff", # Zero line color
        zerolinewidth = 2,       # Zero line width
        gridcolor = "#ffff"       # Grid line color
      )
    )
  
  # Display the interactive plotly PCA plot
  return(list(
    pca_plotly = pca_plot_plotly,
    scree_plotly = scree_plot_plotly,
    pca_df = pca_df,
    PC_df = PC_df
  ))
}