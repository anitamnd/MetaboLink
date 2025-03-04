# ------------- #
# Visualization #
# ------------- #

# Heatmap ----
plot_heatmap <- function(data_subset, data, seq, TOP_X = 50, dataset_name = "", 
                         clustering_distance_rows = "euclidean", clustering_method_rows = "ward.D2",
                         show_column_names = FALSE, show_row_names = FALSE, cluster_rows = FALSE,
                         show_row_dend = FALSE, labels = "", enable_groups = FALSE, groups = "") {
  
  # Store original feature labels
  feature_labels <- rownames(data_subset)
  
  # Debug output
  print(head(data_subset))
  print(head(data))
  print(seq)
  
  message(paste0("TOP_X: ", TOP_X))
  message(paste0("Show column names: ", show_column_names))
  message(paste0("Show row names: ", show_row_names))
  message(paste0("Cluster rows: ", cluster_rows))
  message(paste0("Show row dendrogram: ", show_row_dend))
  message(paste0("labels: ", labels))
  message(paste0("enable groups: ", enable_groups))
  message(paste0("groups: ", groups))
  
  # Make the dataset name more readable
  dataset_name_readable <- gsub("_", " ", dataset_name)
  message(paste0("Heatmap title: ", dataset_name_readable))
  
  # Column annotation for the primary heatmap (using seq$group)
  annotation_col <- data.frame(Group = seq$group)
  rownames(annotation_col) <- rownames(seq)
  group_factor <- factor(annotation_col$Group)
  unique_group <- unique(seq$group)
  
  # Process data_subset: convert columns to numeric, log-transform and autoscale
  data_subset <- apply(data_subset, 2, as.numeric)
  rownames(data_subset) <- feature_labels
  data_subset <- log10(data_subset + 1e-6)
  # Save the unscaled log10 data for fold-change calculation
  data_log <- data_subset
  data_matrix <- as.matrix(data_subset)
  data_matrix <- t(scale(t(data_matrix)))
  
  # Omit rows with NA values
  initial_row_count <- nrow(data_matrix)
  data_matrix <- data_matrix[complete.cases(data_matrix), ]
  final_row_count <- nrow(data_matrix)
  message(paste0("Removed ", initial_row_count - final_row_count, " rows containing NA values."))
  
  # Perform statistical testing (T-test if 2 groups, ANOVA if >=3)
  cat("\n--- Statistical Testing ---\n")
  if (nlevels(group_factor) == 2) {
    pvals <- apply(data_matrix, 1, function(x_row) {
      t_out <- t.test(x_row ~ group_factor)
      t_out$p.value
    })
    message("Using T-test for 2-group comparison.")
  } else {
    pvals <- apply(data_matrix, 1, function(x_row) {
      df_test <- data.frame(value = x_row, group = group_factor)
      aov_out <- aov(value ~ group, data = df_test)
      summary(aov_out)[[1]][["Pr(>F)"]][1]
    })
    message("Using ANOVA for multi-group comparison.")
  }
  
  stats_df <- data.frame(Feature = rownames(data_matrix), p_value = pvals, stringsAsFactors = FALSE)
  stats_df <- stats_df[order(stats_df$p_value), ]
  
  # Select top X features
  top_features <- head(stats_df$Feature, n = TOP_X)
  data_matrix_top <- data_matrix[top_features, , drop = FALSE]
  
  # Compute log2 fold change if exactly 2 groups
  if (nlevels(group_factor) == 2) {
    group_levels <- levels(group_factor)
    data_log_top <- data_log[rownames(data_matrix_top), , drop = FALSE]
    sample_groups <- annotation_col$Group[match(colnames(data_log_top), rownames(annotation_col))]
    fc <- sapply(top_features, function(f) {
      vals <- data_log_top[f, ]
      mean1 <- mean(vals[sample_groups == group_levels[1]], na.rm = TRUE)
      mean2 <- mean(vals[sample_groups == group_levels[2]], na.rm = TRUE)
      log2FC <- (mean1 - mean2) * log2(10)
      return(log2FC)
    })
    top_stats <- stats_df[stats_df$Feature %in% top_features, ]
    top_stats$log2FC <- fc[match(top_stats$Feature, names(fc))]
  } else {
    top_stats <- stats_df[stats_df$Feature %in% top_features, ]
  }
  
  # Add the group information if grouping is enabled
  if (enable_groups) {
    top_stats$group <- as.character(data[[groups]])[match(top_stats$Feature, rownames(data))]
  }
  
  rownames(top_stats) <- 1:nrow(top_stats)
  
  # Build the second heatmap using groups if enabled
  if (enable_groups) {
    # Extract grouping information from data using the specified column; ensure row names match
    m <- match(rownames(data_matrix_top), rownames(data))
    grouping_vector <- as.character(data[[groups]])
    grouping_vector <- grouping_vector[m]
    grouping_vector <- factor(grouping_vector)  # Ensure it's a factor
    unique_groups2 <- levels(grouping_vector)
    group_colors <- setNames(rainbow(length(unique_groups2)), unique_groups2)
    
    second_heatmap <- Heatmap(
      grouping_vector,
      name = "Group",
      col = group_colors,
      cluster_columns = FALSE,
      cluster_rows = cluster_rows,
      show_heatmap_legend = TRUE,
      width = unit(3, "mm"),
      border_gp = gpar(col = "black", lty = 1),
      heatmap_legend_param = list(
        title = "Group",
        at = unique_groups2,
        col = group_colors
      )
    )
  } else {
    second_heatmap <- NULL
  }
  
  # Prepare left and bottom annotations for the primary heatmap
  left_annotation <- rowAnnotation(
    foo = anno_block(gp = gpar(fill = c("black", "grey"))),
    width = unit(0.40, "mm")
  )
  
  bottom_annotation <- HeatmapAnnotation(
    block = anno_block(gp = gpar(fill = rep("black", ncol(data_matrix_top))),
                       height = unit(0.40, "mm")),
    foo = anno_block(
      labels = unique(seq$group),
      labels_gp = gpar(col = "black", fontsize = 14, fontface = "bold"),
      gp = gpar(fill = NA, col = NA, lty = 0)
    )
  )
  
  col_fun <- colorRamp2(c(-2, 0, 2), c("#33568a", "#e8e3e0", "#5e0e21"))
  
  title_txt <- paste0(dataset_name_readable, ": ", paste(unique_group, collapse = " / "))
  
  column_order <- seq$samples
  column_split <- factor(seq$group, levels = unique(seq$group))
  
  hm_primary <- Heatmap(
    data_matrix_top,
    name = "Scaled Log10",
    column_title = title_txt,
    column_title_gp = gpar(fontsize = 16, fontface = "bold"),
    row_title = "cluster %s",
    col = col_fun,
    show_column_names = show_column_names,
    show_row_names = show_row_names,
    cluster_rows = cluster_rows,
    clustering_distance_rows = clustering_distance_rows,
    clustering_method_rows = clustering_method_rows,
    show_row_dend = show_row_dend,
    row_split = 2,
    row_gap = unit(0, "mm"),
    cluster_columns = FALSE,
    cluster_column_slices = FALSE,
    column_split = column_split,
    column_order = column_order,
    column_gap = unit(0, "mm"),
    left_annotation = left_annotation,
    bottom_annotation = bottom_annotation,
    heatmap_legend_param = list(
      title = "Scaled Log10",
      direction = "horizontal",
      title_gp = gpar(fontsize = 12, fontface = "bold"),
      labels_gp = gpar(fontsize = 10)
    ),
    border_gp = gpar(col = "black", lty = 1)
  )
  
  if (!is.null(second_heatmap)) {
    hm_list <- hm_primary + second_heatmap
  } else {
    hm_list <- hm_primary
  }
  
  cat("\n--- Heatmap Created ---\n")
  
  return(list(heatmap = hm_list, top_stats = top_stats))
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
  
  # library(limma)
  
  # on.exit(detach("package:limma", unload = TRUE))
  
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