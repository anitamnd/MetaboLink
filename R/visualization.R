# ------------- #
# Visualization #
# ------------- #

# Heatmap ----
plot_heatmap <- function(data, seq, TOP_X = 50, dataset_name = "") {
  feature_labels <- rownames(data)
  
  # Make dataset_name more readable
  dataset_name_readable <- gsub("_", " ", dataset_name)
  message(paste0("Processing dataset: ", dataset_name_readable))
  
  # Column annotation
  annotation_col <- data.frame(Group = seq$group)
  rownames(annotation_col) <- rownames(seq)
  
  # Factor of group
  group_factor <- factor(annotation_col$Group)
  unique_group <- unique(seq$group)
  
  # Make all columns numeric
  data <- apply(data, 2, as.numeric)
  rownames(data) <- feature_labels
  
  # Log base 10 transform
  data <- log10(data + 1e-6)
  
  # Autoscale each feature (row)
  # Mean-center & SD-scale across samples (columns)
  data_matrix <- as.matrix(data)
  data_matrix <- t(scale(t(data_matrix)))
  
  # Omit rows with NA 
  initial_row_count <- nrow(data_matrix)
  data_matrix <- data_matrix[complete.cases(data_matrix), ]
  final_row_count <- nrow(data_matrix)
  message(paste0("Removed ", initial_row_count - final_row_count, " rows containing NA values."))
  
  # Optional T-test or ANOVA to select top features
  # If exactly 2 groups => T-test
  # If >= 3 groups    => ANOVA
  cat("\n--- Statistical Testing ---\n")
  if (nlevels(group_factor) == 2) {
    # T-test row by row
    pvals <- apply(data_matrix, 1, function(x_row) {
      t_out <- t.test(x_row ~ group_factor)
      t_out$p.value
    })
    message("Using T-test for 2-group comparison.")
  } else {
    # ANOVA row by row
    pvals <- apply(data_matrix, 1, function(x_row) {
      df_test <- data.frame(value = x_row, group = group_factor)
      aov_out <- aov(value ~ group, data = df_test)
      summary(aov_out)[[1]][["Pr(>F)"]][1]
    })
    message("Using ANOVA for multi-group comparison.")
  }
  
  stats_df <- data.frame(
    Feature = rownames(data_matrix),
    p_value = pvals,
    stringsAsFactors = FALSE
  )
  
  # Sort by ascending p-value
  stats_df <- stats_df[order(stats_df$p_value), ]
  
  # Pick top X features
  top_features <- head(stats_df$Feature, n = TOP_X)
  
  data_matrix_top <- data_matrix[top_features, , drop = FALSE]
  data_matrix_top <- data_matrix_top[, colnames(data_matrix_top)]
  
  
  # Define abbreviations and pattern
  abbreviations <- c(
    "Hex2Cer", "HexCer", "GlcCer", "GalCer", "LacCer", "C1P", "S1P", "SPH",
    "PGM", "PIP", "CDCA", "UDCA", "HDCA",
    "FA", "MG", "DG", "TG", "BMP", "CL", "PA", "PC", "PE", "PG",
    "PI", "PS", "Cer", "SM", "St", "SE", "FC", "CE", "CA", "CAR", "DCA",
    "LCA", "GCA", "TCA"
  )
  abbreviations <- abbreviations[order(-nchar(abbreviations))]  # longer first
  pattern <- paste0("^(", paste(abbreviations, collapse = "|"), ")(?=_|\\s|\\(|-|$)")
  
  # Feature grouping
  feature_group <- data.frame(
    Name       = rownames(data_matrix_top),
    Clean_name = gsub("_[^_]+_[^_]+$", "", rownames(data_matrix_top)),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      abbrevi = str_extract(Name, regex(pattern)),
      abbrevi = ifelse(is.na(abbrevi), "non-classified", abbrevi)
    )
  
  unique_classes <- unique(feature_group$abbrevi)
  
  # Color for "CAR" => green (Note: Original color was "#FFC107", which is amber)
  fixed_colors <- c("CAR" = "#FFC107")
  other_classes <- setdiff(unique_classes, names(fixed_colors))
  colors_other  <- rep(c("black","grey"), length(other_classes))
  names(colors_other) <- other_classes
  
  right_annotation_colors <- c(fixed_colors, colors_other)
  # Keep the order consistent with the factor levels in feature_group
  right_annotation_colors <- right_annotation_colors[unique_classes]
  right_annotation_colors[is.na(right_annotation_colors)] <- "black"
  
  
  # Row annotation labeling CAR
  right_annotation_2 <- rowAnnotation(
    # 1. Special marking for CAR
    CAR = anno_mark(
      at         = which(feature_group$abbrevi == "CAR"),
      labels     = feature_group$Clean_name[feature_group$abbrevi == "CAR"],
      labels_gp  = gpar(fontsize = 8, fontface = "bold"),
      which      = "row"
    )
    # ,
    # # 2. Another marking for non-CAR
    # Others = anno_mark(
    #   at         = which(feature_group$abbrevi != "CAR"),
    #   labels     = feature_group$abbrevi[feature_group$abbrevi != "CAR"],
    #   labels_gp  = gpar(fontsize = 8),
    #   which      = "row"
    # )
  )
  
  # Prepare left/bottom annotation bars
  # Left annotation bar with blocks
  left_annotation <- rowAnnotation(
    foo = anno_block(gp = gpar(fill = c("black", "grey"))),
    width = unit(0.40, "mm")
  )
  
  # Bottom annotation
  bottom_annotation = HeatmapAnnotation(
    block = anno_block(gp = gpar(fill = rep("black",
                                            ncol(data_matrix_top))),
                       height = unit(0.40, "mm")),
    foo = anno_block(
      labels    = unique(seq$group),
      labels_gp = gpar(col = "black", fontsize = 14, fontface = "bold"),
      # Set fill and border to transparent/none
      gp = gpar(fill = NA, col = NA, lty = 0)
    )
  )
  
  # Build color function
  col_fun <- colorRamp2(c(-2, 0, 2), c("#33568a", "#e8e3e0", "#5e0e21"))
  
  # Hierarchical clustering with Ward + Euclidean:
  CLUSTER_ROWS <- TRUE
  CLUSTER_METHOD <- "ward.D2"
  DIST_METHOD   <- "euclidean"
  
  # Title
  title_txt <- paste0(
    dataset_name_readable, ": ",
    paste(unique_group, collapse = " / ")
  )

  # Column order
  column_order <- seq$samples
  column_split <- factor(seq$group, levels = unique(seq$group))
  
  # Primary Heatmap
  cat("\n--- Building Heatmap ---\n")
  hm_list <- Heatmap(
    data_matrix_top,
    name                 = "Scaled Log10",
    column_title         = title_txt,
    column_title_gp      = gpar(fontsize = 16, fontface = "bold"),
    row_title            = "cluster %s",  # Title for row clusters if you have splits
    col                  = col_fun,
    
    show_column_names    = FALSE,
    show_row_names       = FALSE,
    
    # Clustering for rows
    cluster_rows         = CLUSTER_ROWS,
    clustering_distance_rows = DIST_METHOD,
    clustering_method_rows   = CLUSTER_METHOD,
    show_row_dend        = FALSE,
    row_split            = 2,
    row_gap              = unit(0, "mm"),
    
    # No column clustering
    cluster_columns      = FALSE,
    cluster_column_slices= FALSE,
    
    column_split         = column_split,
    column_order         = column_order,
    column_gap           = unit(0, "mm"),
    
    # Annotation bars
    left_annotation      = left_annotation,
    bottom_annotation    = bottom_annotation,
    
    # Legend and other aesthetics
    heatmap_legend_param = list(
      title     = "Scaled Log10",
      direction = "horizontal",
      title_gp  = gpar(fontsize = 12, fontface = "bold"),
      labels_gp = gpar(fontsize = 10)
    ),
    border_gp = gpar(col = "black", lty = 1)
    
  ) + 
    # Second heatmap for feature classes
    Heatmap(
      feature_group$abbrevi,
      name                = "Class",  # Must not be empty
      col                 = right_annotation_colors,
      cluster_columns     = FALSE,
      cluster_rows        = FALSE,
      show_heatmap_legend = TRUE,
      width               = unit(3, "mm"),
      
      # Label CAR rows
      right_annotation    = right_annotation_2,
      
      heatmap_legend_param = list(
        title     = "Class",
        at        = c("CAR", other_classes[order(-nchar(other_classes))]),
        title_gp  = gpar(fontsize = 12, fontface = "bold"),
        labels_gp = gpar(fontsize = 10)
      ),
      border_gp           = gpar(col = "black", lty = 1)
    )
  
  # Debug: Confirm heatmap creation
  cat("\n--- Heatmap Created ---\n")
  
  return(hm_list)
}

# Volcano ----
calculate_stats <- function(data, meta,
                            group_col = "group",
                            adjust.method = "BH",
                            min_reps = 2) {
  # 'data': a matrix or data.frame of intensities, rows = features, columns = samples
  # 'meta': a data.frame with rownames=sample IDs (matching colnames(data))
  #         and a factor (or character) column 'Group' (or group_col) for group labels
  # 'min_reps': if you want to ensure at least 2 replicates per group
  
  library(limma)
  
  on.exit(detach("package:limma", unload = TRUE))
  
  # --- Match sample order between 'data' and 'meta' ---
  sampleIDs_data <- colnames(data)
  sampleIDs_meta <- rownames(meta)
  if (!identical(sampleIDs_data, sampleIDs_meta)) {
    # Attempt to reorder 'data' columns to match rownames of 'meta'
    data <- data[, sampleIDs_meta, drop = FALSE]
    # Check again
    if (!identical(colnames(data), rownames(meta))) {
      stop("Could not match columns of 'data' to row names of 'meta'.")
    }
  }
  
  # --- Convert group column to factor, if not already ---
  meta[[group_col]] <- as.factor(meta[[group_col]])
  groups <- meta[[group_col]]
  n_groups <- nlevels(groups)
  
  # --- Basic sanity checks ---
  if (n_groups < 2) {
    stop("You must have at least 2 groups to do differential analysis.")
  }
  # Optionally check each group has >= min_reps
  group_counts <- table(groups)
  if (any(group_counts < min_reps)) {
    warning("Some groups have fewer than 'min_reps' samples. This may affect the analysis.")
  }
  
  # --- Create design matrix (no intercept => one column per group) ---
  design <- model.matrix(~ 0 + groups)
  colnames(design) <- levels(groups)
  
  # --- Fit model ---
  fit <- limma::lmFit(data, design)
  
  # --- Construct all pairwise contrasts automatically ---
  # For example, if groups are c("A","B","C"), we want: B-A, C-A, C-B, etc.
  group_levels <- levels(groups)
  # Generate all combinations of factor levels taken 2 at a time
  all_pairs <- t(combn(group_levels, 2))
  # each row of 'all_pairs' is like c("A","B"), c("A","C"), c("B","C"), ...
  
  # Build a named list/vector of contrast strings, e.g. "B - A", "C - A", etc.
  # We'll also give them a name like "B_vs_A", "C_vs_A", ...
  contrast_list <- apply(all_pairs, 1, function(x) {
    g1 <- x[1]
    g2 <- x[2]
    paste0(g2, " - ", g1)
  })
  
  # We'll create nice names for each contrast
  contrast_names <- apply(all_pairs, 1, function(x) {
    paste0(x[2], "_vs_", x[1])
  })
  
  # Now build the contrast matrix
  contrast.matrix <- limma::makeContrasts(
    contrasts = contrast_list,
    levels = design
  )
  
  # --- Apply contrasts and empirical Bayes ---
  fit2 <- limma::contrasts.fit(fit, contrast.matrix)
  fit2 <- limma::eBayes(fit2)
  
  # --- Extract results for each contrast ---
  # We'll combine them into one big data.frame with a "Contrast" column
  final_list <- list()
  
  for (i in seq_along(contrast_list)) {
    contrast_label <- contrast_names[i]
    tmp <- limma::topTable(fit2, coef = i, number = Inf, adjust.method = adjust.method)
    
    # rename columns for clarity
    names(tmp)[names(tmp) == "logFC"]   <- "log2FC"
    names(tmp)[names(tmp) == "P.Value"] <- "p.value"
    names(tmp)[names(tmp) == "adj.P.Val"] <- "p.adj"
    
    # add normal fold change
    tmp$FC <- 2^tmp$log2FC
    # add a column indicating which contrast this row belongs to
    tmp$Contrast <- contrast_label
    
    # reorder columns or select which columns to keep
    tmp <- tmp[, c("Contrast", "FC", "log2FC", "p.value", "p.adj", 
                   "AveExpr", "t", "B")]
    
    final_list[[contrast_label]] <- tmp
  }
  
  # Combine all results into one data.frame
  final_res <- do.call(rbind, final_list)
  
  # Return final results
  return(final_res)
}

pretty_volcano_plot <- function(statistical_data, volcano_df_name = "volcano",
                                log2FC_tresh, pval_tresh,
                                fill_up, outline_up,
                                fill_down, outline_down,
                                fill_ns, outline_ns) {
  
  data <- statistical_data
  clean_dataset_name <- gsub("_", " ", volcano_df_name)
  
  # Print the first few rows to verify the data
  print(paste("Processing:", clean_dataset_name))
  
  # data$Clean_name <- gsub("_[^_]+_[^_]+$", "", rownames(data)) # Adjust based on your actual cleaning logic
  # Rearrange data
  data <- data[, c("Contrast",
                   # "Clean_name", 
                   "FC", "log2FC", "p.value","p.adj", "AveExpr", "t", "B")]
  
  print(head(data))
  
  # Convert columns to numeric if necessary
  data$log2FC <- as.numeric(as.character(data$log2FC))
  data$p.adj <- as.numeric(as.character(data$p.adj))
  
  # Check for NA values after conversion
  if(any(is.na(data$log2FC)) | any(is.na(data$p.adj))) {
    warning(paste("NA values found in numeric columns for", volcano_df_name))
    # Optionally, handle NA values (e.g., remove them)
    data <- na.omit(data)
  }
  
  # Add Significance column based on criteria
  data$Significance <- "Non Sig"  # Default category
  data$Significance[data$log2FC > log2FC_tresh & data$p.adj < pval_tresh] <- "Up"
  data$Significance[data$log2FC < -log2FC_tresh & data$p.adj < pval_tresh] <- "Down"
  
  # Ensure Significance is a factor with the desired order
  data$Significance <- factor(data$Significance, levels = c("Non Sig", "Up", "Down"))
  
  # Add Color_Category column: "CAR" overrides Significance
  # data$Color_Category <- ifelse(grepl("^CAR", data$X, ignore.case = TRUE), "CAR", as.character(data$Significance))
  
  # Convert Color_Category to factor with desired levels
  # data$Color_Category <- factor(data$Color_Category, levels = c("CAR", "Up", "Down", "Non Sig"))
  
  # Check the distribution of Color_Category
  # print(table(data$Color_Category))
  
  # Check the maximum values for log2FC and p.adj
  print("Max values for log2FC:")
  print(max(abs(data$log2FC)))
  print("Max values for p.adj:")
  print(max(-log10(data$p.adj)))
  
  # Create the volcano plot
  p <- ggplot(data, aes(x = log2FC,
                        y = -log10(p.adj),
                        color = Significance,
                        fill = Significance)) +
    geom_point(shape = 21, size = 3, alpha = 0.8) +
    scale_color_manual(values = c(
      "Non Sig" = outline_ns,
      "Up"      = outline_up,
      "Down"    = outline_down
    )) +
    scale_fill_manual(values = c(
      "Non Sig" = fill_ns,
      "Up"      = fill_up,
      "Down"    = fill_down
    )) +
    geom_vline(xintercept = c(-log2FC_tresh, log2FC_tresh), 
               linetype = "dotted", color = "red") +
    geom_hline(yintercept = -log10(pval_tresh), 
               linetype = "dotted", color = "red") +
    # scale_x_continuous(limits = c(-6,6)) + # 9.3 or 6.4 or 6 
    # scale_y_continuous(limits = c(0, 5)) + # 4 or 5 
    theme_bw() +
    labs(
      title = paste(clean_dataset_name, ":", unique(data$Contrast)),
      x     = "Log2FC",
      y     = "-Log10(p-value)"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) 
  # + 
  #   geom_text_repel(
  #     data = subset(data, Color_Category == "CAR"),  # Only label CAR
  #     aes(label = Clean_name,
  #         colour = "black"),                # Label text = Clean_name
  #     size = 3,
  #     box.padding = 0.5,
  #     point.padding = 0.3,
  #     max.overlaps = 50
  #   )
  
  return(p)
}