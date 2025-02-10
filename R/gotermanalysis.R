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

# Function to subset data based on some criteria in the identifier column
is_valid <- function(x) {
  !(is.na(x) | x == "" | x == " " | x == "N/A" | x == "NA")
}

subset_data <- function(data, compound_column) {
  # Keep rows where either the identifier_column OR the compound_column is valid ie. not missing
  subset <- data[ is_valid(data[[compound_column]]), ]
  
  cat("The number of rows and columns in the data are:", nrow(subset), "and", ncol(subset), "\n")
  return(subset)
}


# THE NEXT SECTION CAN BE DELETED AS IT HAS BEEN UPDATED 
# # Main function to gather identifiers
# gather_identifiers <- function(data, identifier_column) {
#   # Initialize the results df
#   identifier_df <- data.frame()
#   # Use a progress bar to track progress
#   shiny::withProgress(message = "Gathering identifiers...", value = 0, {
#     for (i in 1:nrow(data)) { # nrow(data) instead of 2
#       # Increment the progress bar
#       incProgress(1 / nrow(data), detail = paste("Processing metabolite", i, "of", nrow(data)))
#       
#       # Get the start time
#       start <- Sys.time()
#       # Get the InChI from the data and other identifiers using webchem
#       InChI <- data[[identifier_column]][i]
#       # InChIKey <- tryCatch(cs_convert(InChI, from = "inchi", to = "inchikey"), error = function(e) NA)
#       # SMILES <- tryCatch(cs_convert(InChI, from = "inchi", to = "smiles"), error = function(e) NA)
#       CID <- tryCatch(get_cid(InChI, from = "inchi") %>% slice(1) %>% pull(cid), error = function(e) NA)
#       # CSID <- tryCatch(get_csid(InChI, from = "inchi") %>% pull(csid), error = function(e) NA)
#       IUPACName <- tryCatch(pc_prop(as.integer(CID)) %>% slice(1) %>% pull(IUPACName), error = function(e) NA)
#       WDID <- tryCatch(pc_sect(CID, "wikidata")%>% slice(1) %>% pull(SourceID), error = function(e) NA)
#       HMDBID <- tryCatch(pc_sect(CID, "hmdb id")%>% slice(1) %>% pull(Result), error = function(e) NA)
#       CASID <- tryCatch(pc_sect(CID, "cas")%>% slice(1) %>% pull(Result), error = function(e) NA)
#       KEGGID <- tryCatch(pc_sect(CID, "kegg id")%>% slice(1) %>% pull(Result), error = function(e) NA)
#       ChEBIID <- tryCatch(get_chebiid(InChI, from = "inchi")%>% slice(1) %>% pull(chebiid), error = function(e) NA)
#       
#       # Append each row to identifier_df
#       identifier_df <- rbind(identifier_df, data.frame(
#         InChI = InChI,
#         # InChIKey = InChIKey,
#         # SMILES = SMILES,
#         CID = CID,
#         # CSID = CSID,
#         IUPACName = IUPACName,
#         WDID = WDID,
#         HMDBID = HMDBID,
#         CASID = CASID,
#         KEGGID = KEGGID,
#         ChEBIID = ChEBIID,
#         stringsAsFactors = FALSE
#       ))
#       
#       # Print runtime for debugging
#       cat("Metabolite:", i, "Run Time:", Sys.time() - start, "seconds\n")
#     }
#   })
#   
#   # Return the identifier_df
#   return(identifier_df)
# }

# Function to update InChI in the data frame
update_inchi <- function(data, compound_column, identifier_column, query) {
  
  print(paste0("# of entries: ", nrow(data)))
  print(paste0("# of inchi entries in data: ",
               sum(!is.na(data[[identifier_column]]) & !data[[identifier_column]] %in% c("", " ", "NA", "N/A"))))
  print(paste0("# of missing InChI entries in data: ",
               sum(is.na(data[[identifier_column]]) | data[[identifier_column]] %in% c("", " ", "NA", "N/A"))))
  
  # Identify rows with missing or invalid InChI
  missing_idx <- which(is.na(data[[identifier_column]]) | data[[identifier_column]] %in% c("", " ", "NA", "N/A"))
  
  if (length(missing_idx) == 0) {
    message("No missing InChI entries found.")
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
  
  print(paste0("# of inchi entries in data after local update: ",
               sum(!is.na(data[[identifier_column]]) & !data[[identifier_column]] %in% c("", " ", "NA", "N/A"))))
  print(paste0("# of missing InChI entries in data after local update: ",
               sum(is.na(data[[identifier_column]]) | data[[identifier_column]] %in% c("", " ", "NA", "N/A"))))
  
  # Handle still-missing InChI entries using cir_query()
  still_missing_idx <- which(is.na(data[[identifier_column]]) | data[[identifier_column]] %in% c("", " ", "NA", "N/A"))
  if (length(still_missing_idx) > 0) {
    no_inchi_compounds <- unique(data[[compound_column]][still_missing_idx])
    message("Fetching InChI using cir_query for: ", paste(no_inchi_compounds, collapse = ", "))
    
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
  print(sapply(data,function(x) sum(!is.na(x) & x != "" & x != " " & x != "NA" & x != "N/A")))
  
  print(data[[identifier_column]])
  
  print(paste0("# of missing InChI entries in data after online update: ",
               sum(is.na(data[[identifier_column]]) | data[[identifier_column]] %in% c("", " ", "NA", "N/A"))))
  
  return(data)
}

# pos1 <- update_inchi(pos, compound_column = "compound", identifier_column = "inchi")
# # pos1 <- update_inchi(pos[1:10, ], compound_column = "compound", identifier_column = "inchi")
# # Investigate pos1 inchi 
# sum(is.na(pos1$inchi))
# 
# cat("Before: ", "\n")
# cat("Number of features: ", nrow(pos), "\n")
# cat("Number of compounds: ", nrow(pos)-sum(pos$compound == "" | pos$compound == " " | pos$compound == "N/A"), "\n")
# cat("Number of missing compounds: ", sum(pos$compound == "" | pos$compound == " " | pos$compound == "N/A"), "\n")
# cat("Number of InChI: ", nrow(pos)-sum(pos$inchi == "" | pos$inchi == " " | pos$inchi == "N/A"), "\n")
# cat("Number of missing InChI: ", sum(pos$inchi == "" | pos$inchi == " " | pos$inchi == "N/A"), "\n")
# cat("After: ", "\n")
# cat("Number of features: ", nrow(pos1), "\n")
# cat("Number of InChI: ", 
#     nrow(pos1)-sum(is.na(pos1$inchi) | pos1$inchi == "" | pos1$inchi == " " | pos1$inchi == "NA"), 
#     "\n")
# cat("Number of missing InChI: ", 
#     sum(is.na(pos1$inchi) | pos1$inchi == "" | pos1$inchi == " " | pos1$inchi == "NA"), 
#     "\n")
# 
# # Counts of successes (InChI found) before and after
# counts <- c(120, 399)
# totals <- c(4511, 4511)
# 
# # Run a two-proportion test
# test_result <- prop.test(counts, totals)
# 
# test_result

# Main function to gather CID's
update_cid <- function(data, identifier_column, query) {

  print(paste0("# of entries: ", nrow(data)))
  
  # Start timer for performance monitoring
  Start <- Sys.time()
  
  # Ensure identifier_column is a vector
  identifiers <- data[[identifier_column]]
  if (!is.vector(identifiers)) {
    stop("identifier_column must be a vector.")
  }
  # debuggin: 
  print(paste0("# of identifiers: ", length(identifiers)))
  
  # Check that 'query' data frame has required columns
  required_cols <- c("Identifier", "InChI", "CID")
  if (!all(required_cols %in% names(query))) {
    stop("The 'query' data frame must contain 'Identifier', 'InChI', and 'CID' columns.")
  }
  
  # Initialize a tibble with queries (InChI) and initially unknown CIDs
  # Here, 'query' column represents the InChI from data
  CIDs <- tibble(query = identifiers, cid = NA_character_)
  print(paste0("# of CIDs: ", nrow(CIDs)))
  
  # Function to count missing CIDs
  count_missing <- function(x) sum(is.na(x) | x == "" | x == " " | x == "NA" | x == "N/A")
  
  # Print initial missing count
  cat("Initial missing CIDs:", count_missing(CIDs$cid), "\n")
  
  # Step 1: Local Update from 'query'
  # Create a lookup map from query InChI to CID
  cid_map_inchi <- setNames(query$CID, query$InChI)
  print(paste0("# of CID map InChI: ", length(cid_map_inchi)))
  
  # Update CIDs where possible from local data using InChI
  local_idx <- CIDs$query %in% names(cid_map_inchi)
  print(paste0("# of local idx: ", sum(local_idx)))
  CIDs$cid[local_idx] <- cid_map_inchi[CIDs$query[local_idx]]
  print(paste0("# of CIDs after local update: ", sum(!is.na(CIDs$cid))))
  
  # Print count after local update
  cat("After local update missing CIDs:", count_missing(CIDs$cid), "\n")
  
  # Check if all missing CIDs are resolved
  still_missing_idx <- which(is.na(CIDs$cid) | CIDs$cid %in% c("", " ", "NA", "N/A"))
  print(paste0("# of still missing idx: ", length(still_missing_idx)))
  if (length(still_missing_idx) == 0) {
    cat("All missing CIDs updated from local cache.\n")
    cat("Run Time CID collection:", Sys.time() - Start, "min\n")
    
    merged_data <- merge_data(data, CIDs, identifier_column)
    return(merged_data)
  }
  
  # Step 2: Online Lookup for Remaining Missing CIDs
  # For each missing InChI, attempt to retrieve CID online
  for (i in still_missing_idx) {
    input_for_cid <- CIDs$query[i]  # This is the InChI
    print(paste0("Input for CID: ", input_for_cid))
    
    # Attempt to retrieve CID from online service
    res <- tryCatch({
      get_cid(input_for_cid, from = "inchi")
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(res) && "cid" %in% names(res) && !is.na(res$cid)) {
      CIDs$cid[i] <- res$cid
    } else {
      # Remains NA if we cannot retrieve CID
      CIDs$cid[i] <- NA
    }
  }
  
  # Print final missing count after online update
  cat("After online update missing CIDs:", count_missing(CIDs$cid), "\n")
  
  # Check if some remain missing
  final_missing <- which(is.na(CIDs$cid) | CIDs$cid %in% c("", " ", "NA", "N/A"))
  if (length(final_missing) > 0) {
    warning("No CID found for some queries even after online lookup: ", 
            paste(CIDs$query[final_missing], collapse = ", "))
  }
  
  #remove duplicates from CIDs
  CIDs <- CIDs[!duplicated(CIDs$cid), ]
  
  # use the merge_data function to merge data with the CID
  merged_data <- merge_data(data, CIDs, identifier_column)
  
  # Print run time
  cat("Run Time CID collection:", Sys.time() - Start, "min\n")
  
  return(merged_data)
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

# cleaning names function ---- 
clean_compound_names <- function(names_vec) {
  sapply(names_vec, function(x) {
    # If there's a semicolon, take only the part before the first semicolon
    if (grepl(";", x)) {
      x <- strsplit(x, ";", fixed = TRUE)[[1]][1]
    }
    x <- trimws(x)
    
    # If there's a comma, take only the part before the first comma
    if (grepl(", ", x)) {
      x <- strsplit(x, ", ", fixed = TRUE)[[1]][1]
    }
    x <- trimws(x)
    
    # Remove " cation" and " anion" from the end of the name
    x <- gsub(" cation$", "", x)
    x <- gsub(" anion$", "", x)
    x <- trimws(x)
    # Remove " - [0-9.]+ eV" from the end of the name
    x <- gsub(" - [0-9.]+ eV$", "", x)
    x <- trimws(x)
    
    return(x)
  }, USE.NAMES = FALSE)
}

remove_top_level_comma <- function(s) {
  chars <- strsplit(s, "")[[1]]
  depth <- 0
  comma_pos <- NA
  for (i in seq_along(chars)) {
    c <- chars[i]
    if (c == "(") {
      depth <- depth + 1
    } else if (c == ")") {
      depth <- depth - 1
    } else if (c == "," && depth == 0) {
      # Found a comma at top-level (not inside parentheses)
      comma_pos <- i
      break
    }
  }
  if (!is.na(comma_pos)) {
    # Keep only the part before the top-level comma
    s <- substr(s, 1, comma_pos - 1)
  }
  s <- trimws(s)
  return(s)
}

# insert column ----
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


# Function to perform pathway enrichment analysis







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
    # universe      = all_kegg, 
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
  
  # Prepare long format data
  df_long <- enrichment_data %>%
    mutate(geneID = str_split(geneID, "/")) %>%
    unnest(cols = geneID) %>%
    select(ID, geneID, Description, p.adjust)
  
  # Create edge list
  df_edges <- df_long %>%
    select(ID, geneID) %>%
    rename(from = ID, to = geneID)
  
  # Create node data for Pathway and Compound
  df_nodes_pathway <- enrichment_data %>%
    select(ID, Description, GeneRatio, BgRatio, RichFactor,
           FoldEnrichment, zScore, pvalue, p.adjust, qvalue, Count) %>%
    rename(id = ID) %>%
    mutate(type = "Pathway") 
  
  df_nodes_compound <- data.frame(
    id = unique(df_edges$to),
    type = "Compound",
    category = NA,
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


