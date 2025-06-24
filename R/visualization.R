# ------------- #
# Visualization #
# ------------- #

# statistical test ----
calculate_stats <- function(data, meta,
                            group_col = "group",
                            adjust.method = "BH",
                            min_reps = 2) {

  
  # Match sample order between 'data' and 'meta'
  sampleIDs_data <- colnames(data)
  sampleIDs_meta <- rownames(meta)
  if (!identical(sampleIDs_data, sampleIDs_meta)) {
    # Attempt to reorder 'data' columns to match rownames of 'meta'
    data <- data[, sampleIDs_meta, drop = FALSE]
    # Check again
    if (!identical(colnames(data), rownames(meta))) {
      return()
    }
  }
  
  # Convert group column to factor
  meta[[group_col]] <- as.factor(meta[[group_col]])
  groups <- meta[[group_col]]
  n_groups <- nlevels(groups)
  
  # Basic sanity checks
  if (n_groups < 2) {
    stop("You must have at least 2 groups to do differential analysis.")
  }
  # Check each group has >= min_reps
  group_counts <- table(groups)
  if (any(group_counts < min_reps)) {
    warning("Some groups have fewer than 'min_reps' samples. This may affect the analysis.")
  }
  
  # Create design matrix (no intercept => one column per group)
  design <- model.matrix(~ 0 + groups)
  colnames(design) <- levels(groups)
  
  # Fit model 
  fit <- limma::lmFit(data, design)
  
  # Construct all pairwise contrasts automatically
  group_levels <- levels(groups)
  # Generate all combinations of factor levels taken 2 at a time
  all_pairs <- t(combn(group_levels, 2))
  
  # Build a named list/vector of contrast strings
  contrast_list <- apply(all_pairs, 1, function(x) {
    g1 <- x[1]
    g2 <- x[2]
    paste0(g2, " - ", g1)
  })
  
  # Create names for each contrast
  contrast_names <- apply(all_pairs, 1, function(x) {
    paste0(x[2], "_vs_", x[1])
  })
  
  # Build the contrast matrix
  contrast.matrix <- limma::makeContrasts(
    contrasts = contrast_list,
    levels = design
  )
  
  # Apply contrasts and empirical Bayes
  fit2 <- limma::contrasts.fit(fit, contrast.matrix)
  fit2 <- limma::eBayes(fit2)
  
  # Extract results for each contrast 
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
  
  message("Stats results: ")
  print(head(final_res))
  
  final_res <- final_res[order(final_res$p.adj), ]
  message("Sorted stats results: ")
  print(head(final_res))
  
  # Return final results
  return(final_res)
}

# Heatmap ----
plot_heatmap <- function(data_subset, data, seq, TOP_X = 50, dataset_name = "", 
                         clustering_distance_rows = "euclidean", clustering_method_rows = "ward.D2",
                         show_column_names = FALSE, show_row_names = FALSE, cluster_rows = FALSE,
                         show_row_dend = FALSE, labels = "", enable_groups = FALSE, groups = "", islog = FALSE) {
  
  # Store original feature labels
  feature_labels <- rownames(data_subset)
  
  message(paste0("TOP_X: ", TOP_X))
  message(paste0("Show column names: ", show_column_names))
  message(paste0("Show row names: ", show_row_names))
  message(paste0("Cluster rows: ", cluster_rows))
  message(paste0("Show row dendrogram: ", show_row_dend))
  message(paste0("labels: ", labels))
  message(paste0("enable groups: ", enable_groups))
  message(paste0("groups: ", groups))
  message(paste0("islog: ", islog))
  
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
  # If statement if islog is true 
  if (!islog) {
    data_subset <- log10(data_subset + 1e-6)
  }
  
  # Save the unscaled log10 data for fold-change calculation
  data_log <- data_subset
  data_matrix <- as.matrix(data_subset)
  data_matrix <- t(scale(t(data_matrix))) # Z-score for plotting 
  
  # Omit rows with NA values
  initial_row_count <- nrow(data_matrix)
  data_matrix <- data_matrix[complete.cases(data_matrix), ]
  final_row_count <- nrow(data_matrix)
  message(paste0("Initial row count: ", initial_row_count))
  message(paste0("Removed ", initial_row_count - final_row_count, " rows containing NA values."))
  
  # TODO: Statistical testing should be done on the original data, not the scaled one!!!
  # TODO: Statistical testing should be done on the original data, not the scaled one!!!
  # TODO: Statistical testing should be done on the original data, not the scaled one!!!
  
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
  
  pvals_adj <- p.adjust(pvals, method = "BH")
  
  
  stats_df <- data.frame(Feature = rownames(data_matrix), pvals.adj = pvals_adj, stringsAsFactors = FALSE)
  stats_df <- stats_df[order(stats_df$pvals.adj), ]
  
  print(head(stats_df))
  
  
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
    row_groups <- as.character(data[[groups]])[match(rownames(data_matrix_top), rownames(data))]
    row_groups <- factor(row_groups)
  } else {
    row_groups <- 2
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
    n <- length(unique_groups2)
    group_colors <- setNames( hue_pal()(n), unique_groups2 )
    
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
    row_title_rot = 0, 
    col = col_fun,
    show_column_names = show_column_names,
    show_row_names = show_row_names,
    cluster_rows = cluster_rows,
    clustering_distance_rows = clustering_distance_rows,
    clustering_method_rows = clustering_method_rows,
    show_row_dend = show_row_dend,
    row_split = row_groups,
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
volcano_plot <- function(data, volcano_df_name = "volcano",
                         log2FC_tresh, pval_tresh,
                         fill_up, outline_up,
                         fill_down, outline_down,
                         fill_ns, outline_ns,
                         enable_feature_selection = FALSE, enable_group_selection = FALSE,
                         available_features = "", available_groups = "", group_color_df = NULL,
                         x_param = 5 , y_param = 5, apply_axis_limits = FALSE,
                         pval_col = "p.adj") {
  
  library(ggplot2)
  library(plotly)
  library(ggrepel)
  library(dplyr)
  
  clean_dataset_name <- gsub("_", " ", volcano_df_name) 
  message(paste("Processing:", clean_dataset_name))

  if (enable_group_selection) {
    print("Group color df:")
    print(group_color_df)
  }
  if(enable_feature_selection) {
    print("Available features:")
    print(available_features)
  }
  # Convert columns to numeric
  data$log2FC <- as.numeric(as.character(data$log2FC))
  data[[pval_col]] <- as.numeric(as.character(data[[pval_col]]))
  
  
  if(any(is.na(data$log2FC)) | any(is.na(data$p.adj))) {
    warning(paste("NA values found in numeric columns for", volcano_df_name))
    data <- na.omit(data)
  }
  
  # Define significance based on thresholds
  data$Significance <- "Non Sig"
  is_sig  <- data[[pval_col]] < pval_tresh
  data$Significance[data$log2FC >  log2FC_tresh & is_sig] <- "Up"
  data$Significance[data$log2FC < -log2FC_tresh & is_sig] <- "Down"
  data$Significance <- factor(data$Significance, levels = c("Non Sig", "Up", "Down"))
  
  print(head(data))
  
  print("Max values for log2FC:")
  print(max(abs(data$log2FC)))
  print("Max values for p.adj:")
  print(max(-log10(data$p.adj)))
  
  # Generate hover text for plotly tooltips
  data$hover_text <- paste(
    "Feature:", data$Feature_ID,
    "<br>Group:", data$Group,
    "<br>Significance:", data$Significance,
    "<br>Log2FC:", round(data$log2FC, 3),
    "<br>-Log10(p.adj):", round(-log10(data$p.adj), 3)
  )
  
  # Create a subset with only significant points (if needed later)
  data_sig <- data[data$Significance != "Non Sig",]
  # round specific columns but keep all columns 
  data_sig <- data_sig %>% mutate_at(vars(FC, log2FC, AveExpr,t,B), round, 3)
  
  if(enable_group_selection && length(available_groups) > 0 && !is.null(group_color_df)) {
    
    data <- data %>%
      mutate(
        fill = case_when(
          Significance == "Up"     ~ fill_up,
          Significance == "Down"   ~ fill_down,
          Significance == "Non Sig" ~ fill_ns
        ),
        outline = case_when(
          Significance == "Up"     ~ outline_up,
          Significance == "Down"   ~ outline_down,
          Significance == "Non Sig" ~ outline_ns
        )
      )
    
    data <- data %>%
      left_join(group_color_df, by = "Group") %>%
      mutate(
        fill = coalesce(Fill, fill),
        outline = coalesce(Outline, outline)
      ) %>%
      select(-Fill, -Outline)
    
    print(head(data))
    
    data$Group <- as.factor(data$Group)
    
    # Static mappings for groups not defined dynamically
    static_outline <- c("Up" = outline_up, "Down" = outline_down, "Non Sig" = outline_ns)
    static_fill <- c("Up" = fill_up, "Down" = fill_down, "Non Sig" = fill_ns)
    
    # Create the dynamic mapping vectors from group_color_df
    fill_values <- setNames(as.character(group_color_df$Fill), group_color_df$Group)
    outline_values <- setNames(as.character(group_color_df$Outline), group_color_df$Group)
    
    # Merge dynamic and static mappings (note the removal of the extra comma)
    final_outline <- c(static_outline, outline_values)
    final_fill <- c(static_fill, fill_values)
    
    p <- ggplot() + 
      # First layer: non-selected groups (not in group_color_df)
      geom_point(
        data = subset(data, !(Group %in% group_color_df$Group)),
        aes(x = log2FC, y = -log10( .data[[pval_col]]),
            color = Significance, fill = Significance, text = hover_text),
        shape = 21, size = 3, alpha = 0.7
      ) +
      # Second layer: selected groups (in group_color_df)
      geom_point(
        data = subset(data, Group %in% group_color_df$Group),
        aes(x = log2FC, y = -log10( .data[[pval_col]] ),
            color = Group, fill = Group, text = hover_text),
        shape = 21, size = 3, alpha = 0.7
      ) + 
      scale_color_manual(values = final_outline) +
      scale_fill_manual(values = final_fill) + 
      geom_vline(xintercept = c(-log2FC_tresh, log2FC_tresh), 
                 linetype = "dotted", color = "red") +
      geom_hline(yintercept = -log10(pval_tresh), 
                 linetype = "dotted", color = "red") +
      theme_bw() +
      labs(
        title = paste(clean_dataset_name, ":", gsub("_", " ", unique(data$Contrast))),
        x     = "Log2FC",
        y = paste0("-Log10(", pval_col, ")")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()
      )
    
    if(apply_axis_limits) {
      p <- p +
        scale_x_continuous(limits = c(-x_param, x_param)) +
        scale_y_continuous(limits = c(0, y_param))
    }
    
    p_interactive <- ggplotly(p, tooltip = "text") 
    
    return(list(plot = p_interactive,
                df = data_sig))
  }
  
  # Base volcano plot using Significance for color and fill
  p <- ggplot(data, aes(
    x = log2FC,
    y = -log10( .data[[pval_col]] ),
    color = Significance,
    fill = Significance,
    text = hover_text
  )) +
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
    theme_bw() +
    labs(
      title = paste(clean_dataset_name, ":", gsub("_", " ", unique(data$Contrast))),
      x     = "Log2FC",
      y = paste0("-Log10(", pval_col, ")")
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title = element_blank()
    )
  
  if(apply_axis_limits) {
    p <- p +
      scale_x_continuous(limits = c(-x_param, x_param)) +
      scale_y_continuous(limits = c(0, y_param))
  }
  
  # TODO: This feature is not available in the current version of ggplot2
  if(enable_feature_selection && length(available_features) > 0) {
    p <- p +
      geom_text_repel(
        data = subset(data, Feature_ID %in% available_features),
        aes(x = log2FC, y = -log10( .data[[pval_col]] ), label = Feature_ID),
        size = 3,
        max.overlaps = Inf
      )
  }
  
  # Convert ggplot to an interactive plotly object
  p_interactive <- ggplotly(p, tooltip = "text") 
  
  return(list(plot = p_interactive,
              df = data_sig))
}
