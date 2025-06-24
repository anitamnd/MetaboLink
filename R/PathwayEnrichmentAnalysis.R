#---------------------------------#
#   PATHWAY ENRICHMENT ANALYSIS   #
#---------------------------------#

# Load libraries
library(tibble)

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
    
    # Use cir_query to fetch InChI for compounds without it
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

# Function to get KEGG pathways
get_kegg_pathways <- function(data) {
  
  print(data[["kegg_id"]])

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
  
  print(head(pathways_summary))
  
  # Merge concatenated pathways back to the original dataset
  data_merged <- data %>%
    left_join(pathways_summary, by = "kegg_id")
  
  # Insert the "pathways" column in the correct position
  data_final <- data_merged %>%
    relocate(pathways, .after = kegg_id)
  
  # Remove the old "pathways" column and rename the new column
  data_final <- data_final[, -ncol(data_final)]
  colnames(data_final)[colnames(data_final) == "pathways"] <- "pathways"
  
  return(list(data_joined = data_final, pathways_long = pathways_long))
}

# Visualization of enrichment results
bar_dot_plot <- function(data, title = NULL, top_x = 20, color_conditions = NULL) {
  
  message("Inside bar_dot_plot:: ")
  print(head(data))
  
  bar <- barplot(data,
                 col = color_conditions,
                 title = paste0("Barplot ", title),
                 showCategory = top_x) 
  # + facet_grid(~ Regulation)
  
  dot <- enrichplot::dotplot(data,
                 x = "GeneRatio",
                 color = color_conditions,
                 showCategory = top_x,
                 font.size = 12,
                 label_format = 30,
                 title = paste0("Dotplot ", title)) 
  # + facet_grid(~ Regulation)
  
  
  return(list(bar = bar, dot = dot))
}

NetGraphPlot <- function(enrichment_data, data, title = "Network Graph",
                                   layout_option = "nicely", node_size_mult = 1,
                                   node_text_size = 3, edge_alpha = 0.5,
                                   edge_width_scale = 1, node_color_by = "super_class") {

  message("Inside NetGraphPlotWithGgraph:: ")
  # Convert enrichment data to a DataFrame
  df <- as.data.frame(enrichment_data)

  message("Enrichment Data: ")
  print(head(df))
  message("Original Data: ")
  print(head(data))
  
  valid_cols <- c("super_class", "main_class", "sub_class")
  if (node_color_by %in% valid_cols && !(node_color_by %in% colnames(data))) {
    node_color_by <- "type"
  }
  
  print(node_color_by)

  # Construct Edge List
  edge_list <- df %>%
    mutate(geneID = str_split(geneID, "/")) %>%
    unnest(cols = geneID) %>%
    select(ID, Regulation, geneID, GeneRatio, BgRatio, RichFactor, FoldEnrichment,
           zScore, pvalue, p.adjust, qvalue, Count) %>%
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

  # Merge node metadata and assign a node_color based on user selection.
  graph <- graph %>%
    left_join(data %>% select(any_of(c("Name",
                                     "name",
                                     'Original annotation',
                                     "kegg_id",
                                     "refmet_name",
                                     "super_class",
                                     "main_class",
                                     "sub_class"))),
              by = c("name" = "kegg_id")) %>%
    left_join(df %>% select(ID, Description), by = c("name" = "ID")) %>%
    mutate(
      # For any missing metadata, default to "Module"
      super_class = if ("super_class" %in% colnames(data)) {
        coalesce(super_class, "Module")
      } else {
        NA
      },
      main_class = if ("main_class" %in% colnames(data)) {
        coalesce(main_class, "Module")
      } else {
        NA
      },
      sub_class = if ("sub_class" %in% colnames(data)) {
        coalesce(sub_class, "Module")
      } else {
        NA
      },
      
      # OLD 
      # super_class = coalesce(super_class, "Module"),
      # main_class  = coalesce(main_class, "Module"),
      # sub_class   = coalesce(sub_class, "Module"),

      # Determine node_color based on the user's choice:
      node_color = case_when(
        node_color_by == "super_class" ~ super_class,
        node_color_by == "main_class"  ~ main_class,
        node_color_by == "sub_class"   ~ sub_class,
        node_color_by == "type"        ~ type,
        TRUE                           ~ type
      ),

      # Assign labels based on type and clean them up
      base_label = if ("refmet_name" %in% colnames(data)) {
        ifelse(type == "Compound", refmet_name, Description)
      } else if ("Original annotation" %in% colnames(data)) {
        ifelse(type == "Compound", `Original annotation`, Description)
      } else if ("Name" %in% colnames(data)) {
        ifelse(type == "Compound", Name, Description)
      } else {
        ifelse(type == "Compound", 'name', Description)
      },
      label = coalesce(
        base_label,
        if ("Original annotation" %in% colnames(data)) `Original annotation` else NA_character_,
        if ("Name"               %in% colnames(data)) Name                    else NA_character_,
        name
      ),
      label = str_trunc(label, width = 30, side = "right"),
      label = gsub(",.*", "", label)
      
      # OLD
      # label = ifelse(type == "Compound", refmet_name, Description),
      # label = coalesce(label, Original annotation, Name),
      # label = str_trunc(label, width = 30, side = "right"),
      # label = gsub(",.*", "", label)
      
    ) %>%
  select( -any_of(c(
      "refmet_name",
      "Description",
      "Original annotation",
      "Name",
      "base_label")))
  
  print(graph)

  Graph_plot <- ggraph(graph, layout = layout_option) +
    geom_edge_link0(aes(color = p.adjust,
                        width = edge_width_scale),
                    alpha = edge_alpha) +
    scale_edge_colour_gradient(
      low  = "#d16d67",
      high = "#477cb5"
    ) + 
    
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