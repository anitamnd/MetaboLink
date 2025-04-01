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
run_gene_enrichment <- function(data, all_kegg,
                                pvalueCutoff = 0.05, pAdjustMethod = "fdr", 
                                minGSSize = 10,
                                maxGSSize = 500, qvalueCutoff = 0.02) {
  
  universe <- all_kegg
  
  gce_enrich_result <- enrichKEGG(
    gene = data$kegg_id,
    organism = "cpd",
    keyType = "kegg",
    pvalueCutoff = pvalueCutoff,
    pAdjustMethod = pAdjustMethod,
    universe,
    minGSSize = minGSSize,
    maxGSSize = maxGSSize,
    qvalueCutoff = qvalueCutoff,
    use_internal_data = FALSE
  )
  
  return(gce_enrich_result)
}
run_module_enrichment <- function(data, all_kegg) {
  
  universe <- all_kegg
  
  # metabolite or compound-centric enrichment analysis
  mcce_enrich_result <- enrichMKEGG(
    
    gene          = data[,"kegg_id"],
    universe,
    keyType       = "kegg",         # "kegg" is appropriate for compound IDs like CXXXXXX
    organism      = "cpd",          # "cpd" is used for compound pathways
    pvalueCutoff  = 0.05,
    pAdjustMethod = "fdr",
    minGSSize     = 1,
    qvalueCutoff  = 1)
  
  return(mcce_enrich_result)
}

bar_dot_plot <- function(data, title = NULL, top_x = 20, color_conditions = NULL) {
  
  message("Inside bar_dot_plot:: ")
  print(head(data))
  
  bar <- barplot(data,
                 col = color_conditions,
                 title = paste0("Barplot ", title),
                 showCategory = top_x) + 
    facet_grid(~ Regulation)
  
  dot <- enrichplot::dotplot(data,
                 x = "GeneRatio",
                 color = color_conditions,
                 showCategory = top_x,
                 font.size = 12,
                 label_format = 30,
                 title = paste0("Dotplot ", title)) + 
     facet_grid(~ Regulation)
  
  
  return(list(bar = bar, dot = dot))
}
NetGraphPlot <- function(enrichment_data, data, classification_level = "super_class") {
  # Convert enrichResult to a data frame
  df <- as.data.frame(enrichment_data)
  
  message("Enrichment Data: ")
  print(head(df))
  message("Original Data: ")
  print(head(data))
  
  # Make an edge list: split geneID by "/" and then unnest the list
  edge_list <- df %>%
    mutate(geneID = str_split(geneID, "/")) %>%
    unnest(cols = geneID) %>%
    select(ID, geneID, RichFactor, FoldEnrichment, zScore,
           pvalue, p.adjust, qvalue, Count) %>%
    rename(source = ID,
           target = geneID)
  
  # Edge attributes: transparency, color based on zScore
  edge_list <- edge_list %>%
    mutate(transparency = 1 - p.adjust / max(p.adjust)) %>%
    mutate(color = scales::col_numeric(c("#FFD700", "#FF4500"), domain = NULL)(zScore))
  
  # Create node attributes 
  node_list <- tibble(ID = unique(c(edge_list$source, edge_list$target))) %>% 
    mutate(type = ifelse(str_detect(ID, "^C"), "Compound", "Module")) %>%
    left_join(df %>% select(ID, Description), by = "ID") %>%
    left_join(data %>% select(kegg_id, refmet_name, super_class, main_class, sub_class), 
              by = c("ID" = "kegg_id")) %>%
    mutate(name = ifelse(type == "Module", Description, refmet_name)) %>%
    mutate(
      super_class = ifelse(type == "Compound", super_class, "Module"),
      main_class  = ifelse(type == "Compound", main_class, "Module"),
      sub_class   = ifelse(type == "Compound", sub_class, "Module")
    ) %>%
    select(ID, type, name, super_class, main_class, sub_class)
  
  node_list$name <- gsub(",.*", "", node_list$name)
  
  # Select classification level for coloring
  node_list <- node_list %>%
    mutate(classification = case_when(
      classification_level == "super_class" ~ super_class,
      classification_level == "main_class"  ~ main_class,
      classification_level == "sub_class"   ~ sub_class
    ))
  
  # Assign colors dynamically based on classification
  unique_classes <- unique(na.omit(node_list$classification))
  class_colors <- setNames(rainbow(length(unique_classes)), unique_classes)
  
  # Ensure proper color mapping
  node_list <- node_list %>%
    mutate(color = ifelse(is.na(classification), "#B0B0B0", class_colors[as.character(classification)]))
  
  # Define visual properties
  node_list <- node_list %>%
    mutate(shape = ifelse(type == "Compound", "circle", "square")) %>%
    mutate(frame = "#000000") %>%
    mutate(font = 2)
  
  message("Edge List: ")
  print(head(edge_list))
  message("Node List: ")
  print(node_list)
  
  # Create igraph object
  graph <- graph_from_data_frame(d = edge_list,
                                 vertices = node_list,
                                 directed = FALSE)
  
  # Compute layout to reduce overlapping
  layout <- layout_with_fr(graph)  # Fruchterman-Reingold for spacing
  
  # Adjust label size dynamically
  num_nodes <- vcount(graph)
  label_cex <- ifelse(num_nodes > 50, 0.5, ifelse(num_nodes > 30, 0.7, 1))
  
  # Plot network graph
  plot(graph,
       layout = layout,
       main = paste("Network Graph of Enrichment Data (Colored by", classification_level, ")"),
       sub = "*Insert data title and group*",
       
       # Edge attributes 
       edge.width = E(graph)$RichFactor * 3,
       edge.color = E(graph)$color,
       
       # Vertex attributes
       vertex.color = node_list$color,
       vertex.shape = node_list$shape,
       vertex.frame.color = node_list$frame,
       
       # Label attributes
       vertex.label = node_list$name,
       vertex.label.cex = label_cex,
       vertex.label.font = node_list$font,
       vertex.label.family = "Times",
       vertex.label.dist = 0.8  # Pushes labels away from nodes
  )
  
  # Legend for Node Type (Fixed)
  legend("topright", legend = c("Module", "Compound"), 
         col = "black", 
         pch = c(15, 19), # Square for modules, Circle for compounds
         pt.cex = 2, title = "Node Type")
  
  # Dynamic Legend for Classification Level
  if (length(unique_classes) > 0) {
    legend("bottomright", legend = unique_classes, 
           col = class_colors, 
           pch = 19, pt.cex = 1.5, title = paste("Color by", classification_level))
  }
  
  # Edge Legend for Enrichment Strength
  legend("bottomleft", legend = c("High RichFactor", "Low RichFactor"), 
         col = c("#FF4500", "#FFD700"), 
         lwd = 3, title = "Edge Strength")
  complete_plot <- recordPlot()
  return(list(graph = graph,
              plot = complete_plot))
}
NetGraphPlotWithGgraph <- function(enrichment_data, data, title = "Network Graph",
                                   layout_option = "nicely", node_size_mult = 1,
                                   node_text_size = 3, edge_alpha = 0.5,
                                   edge_width_scale = 1, node_color_by = "super_class" ) {
  
  message("Inside NetGraphPlotWithGgraph:: ")
  # Convert enrichment data to a DataFrame
  df <- as.data.frame(enrichment_data)
  
  message("Enrichment Data: ")
  print(head(df))
  message("Original Data: ")
  print(head(data))
  
  # Construct Edge List
  edge_list <- df %>%
    mutate(geneID = str_split(geneID, "/")) %>%
    unnest(cols = geneID) %>%
    select(ID, Regulation, geneID, GeneRatio, BgRatio, RichFactor, FoldEnrichment, zScore, pvalue, p.adjust, qvalue, Count) %>%
    rename(from = ID, to = geneID) %>%
    relocate(Regulation, .after = Count)
  
  message("Edge List: ")
  print(edge_list)
  
  # Create Graph
  graph <- as_tbl_graph(edge_list) %>%
    mutate(
      type = ifelse(str_detect(name, "^C"), "Compound", "Module"),
      InDegree  = centrality_degree(mode = 'in'),
      OutDegree = centrality_degree(mode = 'out'),
      # Make a total that if the inDegree is zero use Outdegree and vice versa
      TotalDegree = ifelse(InDegree == 0, OutDegree, InDegree)
    )
  
  message("Graph Node Names: ")
  print(graph %>% activate(nodes) %>% as_tibble())
  
  # message("Data KEGG IDs: ")
  # print(data$kegg_id)
  
  # message("Enrichment Data IDs: ")
  # print(df$ID)
  
  # Merge node metadata and assign a node_color based on user selection.
  graph <- graph %>%
    left_join(data %>% select(Name, 'Original annotation', kegg_id, refmet_name, super_class, main_class, sub_class),
              by = c("name" = "kegg_id")) %>%
    left_join(df %>% select(ID, Description), by = c("name" = "ID")) %>%
    mutate(
      # For any missing metadata, default to "Module"
      super_class = coalesce(super_class, "Module"),
      main_class  = coalesce(main_class, "Module"),
      sub_class   = coalesce(sub_class, "Module"),
      
      # Determine node_color based on the user's choice:
      node_color = case_when(
        node_color_by == "super_class" ~ super_class,
        node_color_by == "main_class" ~ main_class,
        node_color_by == "sub_class" ~ sub_class,
        TRUE ~ super_class
      ),
      
      # Assign labels based on type and clean them up
      label = ifelse(type == "Compound", refmet_name, Description),
      label = coalesce(label, `Original annotation`, Name),
      label = str_trunc(label, width = 30, side = "right")
    ) %>%
    select(-refmet_name, -Description, -`Original annotation`, -Name) %>%
    mutate(label = gsub(",.*", "", label))
  
  print(graph)
  
  Graph_plot <- ggraph(graph, layout = layout_option) +
    geom_edge_link0(aes(color = -log10(p.adjust),
                        width = edge_width_scale),
                    alpha = edge_alpha) +
    geom_node_point(aes(color = node_color,
                        size = TotalDegree * node_size_mult,
                        shape = type)) +
    scale_shape_manual(values = c("Compound" = 16, "Module" = 15),
                       guide = guide_legend(order = 1,
                                            override.aes = list(size = 5))) +
    scale_color_discrete(guide = guide_legend(order = 2,
                                              override.aes = list(size = 5))) +
    scale_size_continuous(guide = guide_legend(order = 3)) +
    geom_node_text(aes(label = label),
                   repel = TRUE,
                   size = node_text_size) +
    theme_void() +
    guides(edge_width = "none") +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(
      title = title,
      color = "Biochemical Class",
      size = "Node Importance",
      shape = "Node Type"
    )
  
  return(plot = Graph_plot)
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
  
  message("Inside plot_cnetplot_desc:: ")
  print("Enrichment Data: ")
  print(head(enrichment_data))
  print("Main Data: ")
  print(head(main_data))
  
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
