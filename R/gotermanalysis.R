#---------------------------------------------------------#
#                   GO TERM ANALYSIS                      #
#---------------------------------------------------------#

# Load libraries
library(webchem)
library(tibble)
# library(dplyr)
# library(rJava)
# library(rcdk)
# library(KEGGREST)

# GO2heatmap {annotate}	R Documentation !!!!!!!!!!!!!!!!!!!! 

# Function to update InChI in the data frame
update_inchi <- function(data, compound_column, identifier_column, query) {
  
  message(paste0("Features with InChI: ",
               sum(!is.na(data[[identifier_column]]) & !data[[identifier_column]] %in% c("", " ", "NA", "N/A"))))
  message(paste0("Features without InChI: ",
               sum(is.na(data[[identifier_column]]) | data[[identifier_column]] %in% c("", " ", "NA", "N/A"))))
  
  # Identify rows with missing or invalid InChI
  missing_idx <- which(is.na(data[[identifier_column]]) | data[[identifier_column]] %in% c("", " ", "NA", "N/A"))
  
  if (length(missing_idx) == 0) {
    message("Features has no missing InChI's")
    return(data)
  }
  
  # Create lookup from query (Identifier -> InChI)
  inchi_map <- setNames(query$InChI, query$Identifier)
  
  # Update InChI from query
  compounds_missing <- data[[compound_column]][missing_idx]
  data[[identifier_column]][missing_idx] <- ifelse(
    compounds_missing %in% names(inchi_map),
    inchi_map[compounds_missing],
    data[[identifier_column]][missing_idx]
  )
  
  message(paste0("Features with InChI after local update: ",
               sum(!is.na(data[[identifier_column]]) & !data[[identifier_column]] %in% c("", " ", "NA", "N/A"))))
  message(paste0("Features without InChI after local update: ",
               sum(is.na(data[[identifier_column]]) | data[[identifier_column]] %in% c("", " ", "NA", "N/A"))))
  
  # Handle still-missing InChI entries using cir_query()
  still_missing_idx <- which(is.na(data[[identifier_column]]) | data[[identifier_column]] %in% c("", " ", "NA", "N/A"))
  if (length(still_missing_idx) > 0) {
    no_inchi_compounds <- unique(data[[compound_column]][still_missing_idx])
    message("Fetching InChI using cir_query for:\n", paste(no_inchi_compounds, collapse = ",\n"))
    
    # Replace the following line with your `cir_query()` implementation
    inchi_results <- vapply(no_inchi_compounds, function(compound) {
      res <- tryCatch({ cir_query(compound, from = "name", to = "inchi") }, error = function(e) NA_character_)
      if (length(res) != 1) {
        # Handle unexpected length, e.g. take first element or force NA
        NA_character_
      } else {
        as.character(res)
      }
    }, character(1))
    
    # Update data with fetched InChI
    for (i in seq_along(no_inchi_compounds)) {
      idx <- which(data[[compound_column]] == no_inchi_compounds[i])
      data[[identifier_column]][idx] <- inchi_results[no_inchi_compounds[i]]
    }
  }
  message(paste0("Features with InChI after online update: ",
                 sum(!is.na(data[[identifier_column]]) & !data[[identifier_column]] %in% c("", " ", "NA", "N/A"))))
  message(paste0("Features without InChI after online update: ",
               sum(is.na(data[[identifier_column]]) | data[[identifier_column]] %in% c("", " ", "NA", "N/A"))))
  
  return(data)
}

# Main function to gather CID's
update_cid <- function(data, compound_column, identifier_column, query) {
  
  message(paste0("Number of features: ", nrow(data)))
  
  message(paste0("Features with CID: ",
                 sum(!is.na(data$CID) & !data$CID %in% c("", " ", "NA", "N/A"))))
  message(paste0("Features without CID: ",
                 sum(is.na(data$CID) | data$CID %in% c("", " ", "NA", "N/A"))))
  
  Start <- Sys.time()
  
  # Ensure identifier_column is a vector
  identifiers <- data[[identifier_column]]
  if (!is.vector(identifiers)) {
    stop("identifier_column must be a vector.")
  }
  
  # ---- Local Update from Query ----
  # Update data$CID by matching the identifier (e.g. InChI) with query$InChI
  data <- data %>%
    mutate(CID = coalesce(CID, query$CID[match(.data[[identifier_column]], query$InChI)]))
  
  message(paste0("Features with CID after local update: ",
                 sum(!is.na(data$CID) & !data$CID %in% c("", " ", "NA", "N/A"))))
  message(paste0("Features without CID after local update: ",
                 sum(is.na(data$CID) | data$CID %in% c("", " ", "NA", "N/A"))))
  
  # Identify rows still missing a valid CID
  missing_idx <- which(is.na(data$CID) | data$CID %in% c("", " ", "NA", "N/A"))
  
  if (length(missing_idx) == 0) {
    message("All missing CIDs updated from local.")
    message(paste0("Run Time CID collection: ", Sys.time() - Start, " min"))
    return(data)
  }
  
  # ---- Online Lookup for Remaining Missing CIDs ----
  # Get the unique InChI values from rows missing a CID, cleaning out invalid ones
  missing_inchi <- unique(data[[identifier_column]][missing_idx])
  missing_inchi <- missing_inchi[!is.na(missing_inchi) & missing_inchi != "" &
                                   missing_inchi != " " & missing_inchi != "NA" & missing_inchi != "N/A"]
  
  message(paste0("Unique, non-NA missing InChI to query online: ", length(missing_inchi)))
  
  # Attempt to retrieve CIDs for all missing InChI values in one call.
  # Assumes get_cid() is vectorized and returns a data frame with columns "inchi" and "cid"
  cid_results <- tryCatch({
    get_cid(missing_inchi, from = "inchi")
  }, error = function(e) {
    data.frame(inchi = missing_inchi, cid = NA_character_, stringsAsFactors = FALSE)
  })
  
  print(head(cid_results))
  
  # Mutate data$CID to be character type
  data$CID <- as.character(data$CID)
  
  # Use a dplyr left_join to update data$CID:
  # We join on the identifier column (e.g. "InChI") matching to cid_results$inchi,
  # then use coalesce() to update CID where missing.
  data <- data %>%
    left_join(cid_results, by = setNames("query", identifier_column)) %>%
    mutate(CID = coalesce(CID, cid)) %>%
    select(-cid)
  
  message(paste0("Features with CID after online update: ",
                 sum(!is.na(data$CID) & !data$CID %in% c("", " ", "NA", "N/A"))))
  message(paste0("Features without CID after online update: ",
                 sum(is.na(data$CID) | data$CID %in% c("", " ", "NA", "N/A"))))
  
  message(paste0("Number of features after CID collection: ", nrow(data)))
  message(paste0("Run Time - CID collection: ", Sys.time() - Start, " min"))
  
  return(data)
}


get_kegg_pathways <- function(data) {
  # Extract KEGG IDs from data
  kegg_ids <- data[["kegg_id"]]
  
  # Remove NA, empty strings, and trim whitespace
  kegg_ids <- kegg_ids[!is.na(kegg_ids) & kegg_ids != ""]
  kegg_ids <- trimws(kegg_ids)
  
  # Initialize an empty data frame for the long format
  pathways_long <- data.frame(
    kegg_id      = character(),
    pathway_code = character(),
    pathway_name = character(),
    stringsAsFactors = FALSE
  )
  
  # Loop over KEGG IDs to retrieve pathway info
  for (kegg_id in kegg_ids) {
    # Query KEGG
    res <- keggGet(kegg_id)
    
    # Extract pathway information
    pathway_info <- res[[1]]$PATHWAY
    
    if (!is.null(pathway_info) && length(pathway_info) > 0) {
      # Create a temporary data frame for each KEGG ID
      temp_df <- data.frame(
        kegg_id      = kegg_id,
        pathway_code = names(pathway_info),
        pathway_name = unname(pathway_info),
        stringsAsFactors = FALSE
      )
      
      # Append to the main long-format data frame
      pathways_long <- rbind(pathways_long, temp_df)
    } else {
      # Add a row with NA if no pathways are found
      temp_df <- data.frame(
        kegg_id      = kegg_id,
        pathway_code = NA_character_,
        pathway_name = NA_character_,
        stringsAsFactors = FALSE
      )
      pathways_long <- rbind(pathways_long, temp_df)
    }
  }
  
  # Concatenate pathway information into a single string for each KEGG ID
  pathways_summary <- pathways_long %>%
    group_by(kegg_id) %>%
    summarize(
      pathways = paste(
        paste0(pathway_code, ": ", pathway_name),
        collapse = "; "
      ),
      .groups = "drop"
    )
  
  # Merge concatenated pathways back to the original dataset
  data_merged <- data %>%
    left_join(pathways_summary, by = "kegg_id")
  
  # Insert the "pathways" column in the correct position
  data_final <- insertColumn(
    df              = data_merged,
    column_name     = "kegg_id",
    new_column_name = "pathways",
    new_column      = data_merged$pathways
  )
  
  # Remove the old "pathways" column and rename the new column
  data_final <- data_final[, -ncol(data_final)]
  colnames(data_final)[colnames(data_final) == "pathways"] <- "pathways"
  
  return(list(data_joined = data_final, pathways_long = pathways_long))
}


# Function to merge data based on the identifier column
merge_data <- function(main_df, identifier_df, identifier_column) {
  
  # Ensure the identifier column has the same name in both data frames
  names(identifier_df)[1] <- identifier_column
  
  # Identify overlapping columns other than the identifier_column
  common_cols <- intersect(names(main_df), names(identifier_df))
  common_cols <- setdiff(common_cols, identifier_column)
  
  # Rename overlapping columns in identifier_df to avoid duplication
  if(length(common_cols) > 0) {
    names(identifier_df)[names(identifier_df) %in% common_cols] <- paste0(common_cols, "_id")
  }
  
  # Debugging
  # print(head(identifier_df))
  # print(dim(identifier_df))
  # print(head(main_df[[identifier_column]]))
  # print(dim(main_df))
  
  # Merge main_df and identifier_df on the identifier column
  merged_df <- merge(main_df, identifier_df, by = identifier_column, all.x = TRUE)
  
  # Get the position of the identifier_column in main_df
  identifier_index <- which(names(main_df) == identifier_column)
  
  # Get the names of the columns from identifier_df (excluding identifier_column)
  id_cols <- setdiff(names(identifier_df), identifier_column)
  
  # Build the new column order
  # Start with columns before the identifier_column in main_df
  if(identifier_index > 1) {
    left_cols <- names(main_df)[1:(identifier_index - 1)]
  } else {
    left_cols <- character(0)
  }
  
  # Columns after the identifier_column in main_df
  if(identifier_index < ncol(main_df)) {
    right_cols <- names(main_df)[(identifier_index + 1):ncol(main_df)]
  } else {
    right_cols <- character(0)
  }
  
  # New column order: left_cols, identifier_column, id_cols (from identifier_df), right_cols
  new_col_order <- c(left_cols, identifier_column, id_cols, right_cols)
  
  # Reorder the merged_df according to new_col_order
  final_df <- merged_df[, new_col_order]
  
  return(final_df)
}

# Make a function that take in a df and a column name and insert a new column to the right of the column
insertColumn <- function(df, column_name, new_column_name, new_column) {
  # Get the position of the column
  column_position <- which(names(df) == column_name)
  if (length(column_position) == 0) {
    stop("Column not found")
  }
  
  # Check if new column has the correct length
  if (length(new_column) != nrow(df)) {
    stop("The new column must have the same number of rows as the data frame")
  }
  
  # Split the data frame into two parts
  left_df <- df[, 1:column_position, drop = FALSE]
  right_df <- df[, (column_position + 1):ncol(df), drop = FALSE]
  
  # Insert the new column as a data frame
  new_col_df <- data.frame(new_column)
  names(new_col_df) <- new_column_name
  
  # Combine the data frames
  new_df <- cbind(left_df, new_col_df, right_df)
  return(new_df)
}

# Function to perform GO term enrichment analysis
run_gene_enrichment <- function(data, all_kegg) {
  
  # gene-centric enrichment analysis.
  gce_enrich_result <- enrichKEGG(
    data[,"kegg_id"],
    # universe = all_kegg,
    keyType = "kegg",
    organism = "cpd",
    pvalueCutoff = 0.05,
    pAdjustMethod = "fdr",
    minGSSize=1)
  # gce_enrich_result_df <- as.data.frame(gce_enrich_result)
  # gce_enrich_result_df$GeneRatio <- sapply(strsplit(gce_enrich_result_df$GeneRatio, "/"), 
  #                                          function(x) as.numeric(x[1]) / as.numeric(x[2]))
  # gce_enrich_result_df$BgRatio <- sapply(strsplit(gce_enrich_result_df$BgRatio, "/"), 
  #                                        function(x) as.numeric(x[1]) / as.numeric(x[2]))

  return(gce_enrich_result)
}
run_module_enrichment <- function(data, all_kegg) {
  # metabolite or compound-centric enrichment analysis
  mcce_enrich_result <- enrichMKEGG(
    gene          = data[,"kegg_id"],
    # universe      = as.character(all_kegg),
    keyType       = "kegg",         # "kegg" is appropriate for compound IDs like CXXXXXX
    organism      = "cpd",          # "cpd" is used for compound pathways
    pvalueCutoff  = 0.05,
    pAdjustMethod = "fdr",
    minGSSize     = 1,
    qvalueCutoff  = 1)
  
  # mcce_enrich_result_df <- as.data.frame(mcce_enrich_result)
  # mcce_enrich_result_df$GeneRatio <- sapply(strsplit(mcce_enrich_result_df$GeneRatio, "/"), 
  #                                           function(x) as.numeric(x[1]) / as.numeric(x[2]))
  # mcce_enrich_result_df$BgRatio <- sapply(strsplit(mcce_enrich_result_df$BgRatio, "/"), 
  #                                         function(x) as.numeric(x[1]) / as.numeric(x[2]))
  
  return(mcce_enrich_result)
}

bar_dot_plot <- function(data, title = NULL, top_x = 5) {
  bar <- barplot(data,
          title = paste0("Barplot ", title),
          showCategory = top_x
  )
  dot <- dotplot(data,
          x = "GeneRatio",
          color = "p.adjust",
          showCategory = top_x,
          size = NULL,
          split = NULL,
          font.size = 12,
          title = paste0("Dotplot ", title))
  
  return(list(bar = bar, dot = dot))
}
plot_cnetplot_subcat <- function(enrichment_data, main_data, top_n = 5) {
  # Subset top_n
  enrichment_data <- enrichment_data %>%
    arrange(p.adjust) %>%
    head(top_n)
  
  # Prepare long format data
  df_long <- enrichment_data %>%
    mutate(geneID = str_split(geneID, "/")) %>%
    unnest(cols = geneID) %>%
    select(ID, geneID, subcategory, p.adjust)
  
  # Create edge list
  df_edges <- df_long %>%
    select(ID, geneID) %>%
    rename(from = ID, to = geneID)
  
  # Create node data for Pathway and Compound
  df_nodes_pathway <- enrichment_data %>%
    select(ID, category, subcategory, Description, GeneRatio, BgRatio, RichFactor,
           FoldEnrichment, zScore, pvalue, p.adjust, qvalue, Count) %>%
    rename(id = ID) %>%
    mutate(type = "Pathway") 
  
  df_nodes_compound <- data.frame(
    id = unique(df_edges$to),
    type = "Compound",
    category = NA,
    subcategory = NA,
    p.adjust = NA,
    Description = NA
  )
  
  # Add new column to df_nodes with refmet_name from main_df if kegg_id matches to id 
  df_nodes_compound <- df_nodes_compound %>%
    left_join(main_data[,c("kegg_id","refmet_name")], by = c("id" = "kegg_id"))
  
  # Combine nodes
  df_nodes <- bind_rows(df_nodes_pathway, df_nodes_compound)

  # Create graph
  net <- graph_from_data_frame(d = df_edges, vertices = df_nodes, directed = FALSE)
  
  pathway_labels <- setNames(df_nodes$subcategory, df_nodes$id)
  V(net)$label <- ifelse(
    V(net)$type == "Pathway",  # Pathways: Use subcategory
    df_nodes$subcategory[match(V(net)$name, df_nodes$id)],  
    ifelse(
      !is.na(df_nodes$refmet_name[match(V(net)$name, df_nodes$id)]),  # Compounds: Use refmet_name if available
      df_nodes$refmet_name[match(V(net)$name, df_nodes$id)],  
      V(net)$name  # Otherwise, use the original KEGG ID
    )
  )
  
  pathway_size <- setNames(df_nodes$Count, df_nodes$id)
  V(net)$size <- ifelse(V(net)$type == "Pathway", pathway_size[V(net)$name], 10)
  
  V(net)$color <- ifelse(V(net)$type == "Pathway", "tomato", "gold")
  
  V(net)$shape <- ifelse(V(net)$type == "Pathway", "csquare", "circle")
  
  # Assign colors to edges based on subcategory
  subcategory_colors <- setNames(rainbow(length(unique(df_long$subcategory))),
                                 unique(df_long$subcategory))
  E(net)$color <- subcategory_colors[df_long$subcategory]
  
  # Define layout
  layout_pos <- layout_with_fr(net, niter = 500)  # More iterations for better spreading
  
  # Plot graph
  plot(net,
       layout = layout_pos,
       edge.curve = 0.1,
       main = "Pathway-Compound Connection Graph",
       vertex.label = V(net)$label,
       vertex.label.color = "black",
       vertex.size = V(net)$size,
       vertex.shape = V(net)$shape,
       edge.color = E(net)$color)
  
  # **Move Node Legend to Bottom**
  legend(
    x = "bottomleft",
    title = "Node Types",
    legend = c("Pathway", "Compound"),
    pch = c(15, 16),
    col = c("tomato", "gold"),
    cex = 0.8
  )
  
  # **Move Edge Legend to Bottom Below Node Legend**
  legend(
    x = "bottomright",
    title = "Subcategory Colors",
    legend = names(subcategory_colors),
    col = subcategory_colors,
    lwd = 1,
    horiz = TRUE,  # Horizontal layout
    cex = 0.4,
    ncol = 
  )
  
  complete_plot <- recordPlot()
  
  return(list(net = net,
              plot = complete_plot))
}
plot_cnetplot_desc <- function(enrichment_data, main_data, top_n = 5) {
  # Subset top_n
  enrichment_data <- enrichment_data %>%
    arrange(p.adjust) %>%
    head(top_n)
  print(enrichment_data)
  
  # Prepare long format data
  df_long <- enrichment_data %>%
    mutate(geneID = str_split(geneID, "/")) %>%
    unnest(cols = geneID) %>%
    select(ID, geneID, Description, p.adjust)
  print(df_long)
  
  # Create edge list
  df_edges <- df_long %>%
    select(ID, geneID) %>%
    rename(from = ID, to = geneID)
  print(df_edges)
  
  # Create node data for Pathway and Compound
  df_nodes_pathway <- enrichment_data %>%
    select(ID, Description, GeneRatio, BgRatio, RichFactor,
           FoldEnrichment, zScore, pvalue, p.adjust, qvalue, Count) %>%
    rename(id = ID) %>%
    mutate(type = "Pathway") 
  print(df_nodes_pathway)
  
  df_nodes_compound <- data.frame(
    id = unique(df_edges$to),
    type = "Compound",
    category = NA,
    p.adjust = NA,
    Description = NA
  )
  print(df_nodes_pathway)
  
  # Add new column to df_nodes with refmet_name from main_df if kegg_id matches to id 
  df_nodes_compound <- df_nodes_compound %>%
    left_join(main_data[,c("kegg_id","refmet_name")], by = c("id" = "kegg_id"))
  
  # Combine nodes
  df_nodes <- bind_rows(df_nodes_pathway, df_nodes_compound)
  
  # Create graph
  net <- graph_from_data_frame(d = df_edges, vertices = df_nodes, directed = FALSE)
  
  pathway_labels <- setNames(df_nodes$Description, df_nodes$id)
  V(net)$label <- ifelse(
    V(net)$type == "Pathway",  # Pathways: Use Description
    df_nodes$Description[match(V(net)$name, df_nodes$id)],  
    ifelse(
      !is.na(df_nodes$refmet_name[match(V(net)$name, df_nodes$id)]),  # Compounds: Use refmet_name if available
      df_nodes$refmet_name[match(V(net)$name, df_nodes$id)],  
      V(net)$name  # Otherwise, use the original KEGG ID
    )
  )
  
  pathway_size <- setNames(df_nodes$Count, df_nodes$id)
  V(net)$size <- ifelse(V(net)$type == "Pathway", pathway_size[V(net)$name], 10)
  
  V(net)$color <- ifelse(V(net)$type == "Pathway", "tomato", "gold")
  
  V(net)$shape <- ifelse(V(net)$type == "Pathway", "csquare", "circle")
  
  # Assign colors to edges based on Description
  description_colors <- setNames(rainbow(length(unique(df_long$Description))),
                                 unique(df_long$Description))
  E(net)$color <- description_colors[df_long$Description]
  
  # Define layout
  layout_pos <- layout_with_fr(net, niter = 500)  # More iterations for better spreading
  
  # Plot graph
  plot(net,
       layout = layout_pos,
       edge.curve = 0.1,
       main = "Pathway-Compound Connection Graph",
       vertex.label = V(net)$label,
       vertex.label.color = "black",
       vertex.size = V(net)$size,
       vertex.shape = V(net)$shape,
       edge.color = E(net)$color)
  
  # **Move Node Legend to Bottom**
  legend(
    x = "bottomleft",
    title = "Node Types",
    legend = c("Pathway", "Compound"),
    pch = c(15, 16),
    col = c("tomato", "gold"),
    cex = 0.8
  )
  
  # **Move Edge Legend to Bottom Below Node Legend**
  legend(
    x = "bottomright",
    title = "Description Colors",
    legend = names(description_colors),
    col = description_colors,
    lwd = 1,
    horiz = TRUE,  # Horizontal layout
    cex = 0.4
    )
  
  complete_plot <- recordPlot()
  
  return(list(net = net,
              plot = complete_plot))
}
