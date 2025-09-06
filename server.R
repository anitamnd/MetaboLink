shinyServer(function(session, input, output) {
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  # Global variables
  rv <- reactiveValues(data = list(), # List of data frames
                       sequence = list(), # List of sequences
                       activeFile = NULL, # Index of active file
                       results = list(), # List of PCA results
                       tmpData = NULL, # Temporary data
                       tmpSequence = NULL, # Temporary sequence
                       choices = NULL, # List of choices
                       drift_plot_select = 1, # Drift plot selection
                       info = vector("character"), # Vector of info
                       pca_results = list(), # List of PCA results
                       outlier_df = list(), # List of outlier data frames
                       identifier_df = list(), # List of identifier data frames
                       multipleLipidNamesDf = NULL) 
  
  userConfirmation <- reactiveVal(FALSE)
  disable("upload")
  
  rankings_merge <- data.frame(
    name = c("High", "Medium", "Low"),
    priority = c(1, 2, 3)
  )
  ##############
  # Load files #
  ##############
  
  massCorrection <- read.csv("./csvfiles/adducts.csv") # Import mass correction data
  refmet <- read.csv("./csvfiles/refmet.csv") # Import reference metabolite data
  query <- read.csv("./csvfiles/queried_properties.csv") # Import query data
  # Hidden features
  quotes <- readLines("./csvfiles/quotes.csv")
  
  ##########################
  # Window/panel selection #
  ##########################
  
  observeEvent(list(c(input$sequence, input$example, input$upload)), {
    windowselect("sequence")
  }, ignoreInit = T)
  observeEvent(input$explore, {
    windowselect("datatable")
  })
  observeEvent(input$export, {
    windowselect("export")
  })
  observeEvent(input$statistics_button, {
    windowselect("statistics")
  })
  
  
  #############
  # Functions #
  #############
  
  initializeVariables <- function() {
    rv$results[[length(rv$results) + 1]] <- list()
  }
  
  createDownloadHandler <- function(type, fileExtension, dataFunc) {
    function(x) {
      output[[paste0("dwn_", type, x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), "_", type, fileExtension)
        },
        content = function(file) {
          dataToWrite <- dataFunc(rv, x)  # Pass rv and x to the data function
          if(fileExtension == ".csv") {
            write.csv(dataToWrite, file, row.names = FALSE)
          } else if(fileExtension == ".xlsx") {
            write_xlsx(dataToWrite, file)
          } else if(fileExtension == ".txt") {
            writeLines(dataToWrite, file)
          }
        }
      )
    }
  }
  
  renderDownloadUI <- function(idPrefix, labelSuffix) {
    renderUI({
      lapply(seq_len(length(rv$choices)), function(x) {
        fluidRow(column(12, downloadLink(paste0(idPrefix, x), paste0(rv$choices[x], labelSuffix))))
      })
    })
  }
  
  updateDataAndSequence <- function(notificationMessage, newFileInput, suffix, additionalInfo = NULL) {
    if (is.null(rv$tmpData)) {
      showNotification(notificationMessage, type = "error")
    } else {
      if (newFileInput) {
        newIndex <- length(rv$data) + 1
        rv$data[[newIndex]] <- rv$tmpData
        rv$sequence[[newIndex]] <- rv$tmpSequence
        newName <- paste0(names(rv$data)[rv$activeFile], suffix)
        names(rv$data)[newIndex] <- newName
        if (!is.null(additionalInfo)) {
          rv$info[newIndex] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), additionalInfo, "\n")
        }
        initializeVariables()
        rv$activeFile <- newIndex
      } else {
        rv$data[[rv$activeFile]] <- rv$tmpData
        rv$sequence[[rv$activeFile]] <- rv$tmpSequence
        names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], suffix)
        if (!is.null(additionalInfo)) {
          rv$info[rv$activeFile] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), additionalInfo, "\n")
        }
      }
      rv$choices <- paste(seq_along(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL
    }
  }
  
  updateSequence <- function(seq, data, identifier_column, fill_label = NA) {
    # Get the column names from the data
    data_colnames <- colnames(data)
    
    # Identify missing rows in the sequence (new columns in data)
    missing_seq_rows <- setdiff(data_colnames, rownames(seq))
    
    # Proceed to add new identifier rows
    if (length(missing_seq_rows) > 0) {
      num_new_rows <- length(missing_seq_rows)
      
      # Create new sequence entries for the missing columns
      new_seq_entries <- data.frame(
        labels = rep(fill_label, num_new_rows),
        batch = rep(NA, num_new_rows),
        order = rep(NA, num_new_rows),
        group = rep(NA, num_new_rows),
        time = rep(NA, num_new_rows),
        paired = rep(NA, num_new_rows),
        amount = rep(NA, num_new_rows),
        stringsAsFactors = FALSE,
        row.names = missing_seq_rows
      )
      
      # Find the position of the identifier_column in seq
      identifier_position <- which(rownames(seq) == identifier_column)
      
      if (length(identifier_position) == 0) {
        # identifier_column not found in seq
        # Split seq into upper_seq and lower_seq as per your needs
        # For demonstration, we'll consider the entire seq as upper_seq and leave lower_seq empty
        upper_seq <- seq
        lower_seq <- data.frame()
      } else {
        # Split seq into upper_seq and lower_seq
        upper_seq <- seq[1:identifier_position, , drop = FALSE]
        lower_seq <- seq[(identifier_position + 1):nrow(seq), , drop = FALSE]
      }
      
      # Combine upper_seq, new_seq_entries, and lower_seq
      seq <- rbind(upper_seq, new_seq_entries, lower_seq)
    }
    
    # Reorder sequence to match data columns
    seq <- seq[data_colnames, , drop = FALSE]
    # print(seq)
    return(seq)
  }
  
  observeEvent(input$inputFile, { # Ensure file is uploaded before pressing Upload button
    inputFile <<- read.csv(input$inputFile$datapath,
                           header = 1,
                           stringsAsFactors = F,
                           check.names = FALSE)
    enable("upload")
  })
  
  observeEvent(input$upload, {
    shinyCatch({
      #inputFile <- read.csv(input$inputFile$datapath, header = 1, stringsAsFactors = F, check.names = FALSE)
      if(input$fileType == "Samples in rows") {
        inputFile <- t(inputFile)
      }
      
      # Look for column "Name" or "name" and make it the first column
      if("Name" %in% colnames(inputFile)) {
        inputFile <- inputFile[, c("Name", setdiff(colnames(inputFile), "Name"))]
      } else if("name" %in% colnames(inputFile)) {
        inputFile <- inputFile[, c("name", setdiff(colnames(inputFile), "name"))]
      }
      
      # Remove the special charactor special ± character (Unicode U+00B1) from data[,1]
      inputFile[,1] <- iconv(inputFile[,1], "WINDOWS-1252", "UTF-8", sub = "")
      inputFile[,1] <- gsub("\\(±\\)", "", inputFile[,1])
      
      # TODO: maybe used for data type in future
      # inputFile$Data_Type <- ifelse(length(input$dataType) > 0,
      #                               paste(input$dataType,
      #                                     collapse = ", "),
      #                               "Not Specified")
      # inputFile <- inputFile %>% 
      #   relocate(Data_Type, .after = Name)
      
    },
    blocking_level = 'message'
    )
    if(any(duplicated(names(inputFile)))) {
      sendSweetAlert(session,
                     title = "Error",
                     text = paste("Duplicate columns found."),
                     type = "error")
    } else {
      labels <- identifyLabels(inputFile)
      initializeVariables()
      rv$sequence[[length(rv$sequence) + 1]] <- data.frame(labels,
                                                           batch = NA,
                                                           order = NA,
                                                           group = NA,
                                                           time = NA,
                                                           paired = NA,
                                                           amount = NA)
      rv$data[[length(rv$data) + 1]] <- inputFile
      names(rv$data)[length(rv$data)] <- substr(input$inputFile$name, 1, nchar(input$inputFile$name) - 4)
      
      rv$choices <- paste(seq_along(rv$data), ": ", names(rv$data))
      rv$activeFile <- length(rv$data)
      updateTabItems(session, "tabs", selected = "Datainput")
      show("buttons")
    }
  })
  
  observeEvent(input$inputSequence, {
    shinyCatch({
      inputSequence <- read.csv(input$inputSequence$datapath, header = 1, stringsAsFactors = FALSE)
      colnames(inputSequence) <- tolower(colnames(inputSequence))
      inputSequence <- checkSequence(inputSequence)
    },
    blocking_level = 'message'
    )
    sequence <- rv$sequence[[rv$activeFile]]
    labeledSequence <- data.frame("sample" = row.names(sequence), sequence)
    inputSequence["sample"] <- lapply(inputSequence["sample"], as.character)
    sequence <- left_join(labeledSequence[, 1:2], inputSequence, by = "sample")
    row.names(sequence) <- sequence[, 1]
    sequence <- sequence[, -1]
    rv$sequence[[rv$activeFile]] <- sequence
    
    if(any(grepl("[^[:alnum:]_]", sequence$group))) {
      showModal(
        modalDialog(
          title = "Invalid group names", size = "m", easyClose = TRUE,
          footer = list(actionButton("group_name_format", "Format names"), modalButton("Dismiss")),
          fluidRow(
            column(12, p("Invalid group names found. Group names must be alphanumeric and not include spaces. The use of ´_´ has been allowed."))
          )
        )
      )
    }
  })
  
  observeEvent(input$group_name_format, {
    sequence <- rv$sequence[[rv$activeFile]]
    sequence$group <- gsub("[^[:alnum:]]", "", sequence$group)
    rv$sequence[[rv$activeFile]] <- sequence
    removeModal()
  })
  
  observeEvent(input$reuseSequence, {
    inputSequence <- read.csv(input$inputSequence$datapath, header = 1, stringsAsFactors = FALSE)
    colnames(inputSequence) <- tolower(colnames(inputSequence))
    inputSequence <- checkSequence(inputSequence)
    sequence <- rv$sequence[[rv$activeFile]]
    labeledSequence <- data.frame("sample" = row.names(sequence), sequence)
    inputSequence["sample"] <- lapply(inputSequence["sample"], as.character)
    sequence <- left_join(labeledSequence[, 1:2], inputSequence, by = "sample")
    row.names(sequence) <- sequence[, 1]
    sequence <- sequence[, -1]
    rv$sequence[[rv$activeFile]] <- sequence
  })
  
  observeEvent(input$addRefmet, {
    show_modal_spinner(
      spin = "atom",
      color = "#0A4F8F",
      text = "Extracting RefMet information... This may take a few minutes."
    )
    
    if(is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
     } else {
      sequence <- rv$sequence[[rv$activeFile]]
      data <- rv$data[[rv$activeFile]]
      identifier <- input$identifier_column_refmet
      
      print(identifier)
      
      data_query <- data %>%
        left_join(
          query %>%
            select(
              any_of(
                c(
                  "CID", "InChI","CanonicalSMILES", "IsomericSMILES",  "InChIKey", "IUPACName"))),                                 # close select(...)
          by           = setNames("InChI", identifier),
          relationship = "many-to-many"
        )
      
      if ("InChIKey.x" %in% colnames(data_query)) {
        # Merge InChIKey.x and InChIKey.y into InChIKey
        data_query <- data_query %>%
          mutate(InChIKey = coalesce(InChIKey.x, InChIKey.y)) %>%
          select(-c(InChIKey.x, InChIKey.y))
      }
      
      data_query <- data_query %>%
        relocate(any_of(c("InChIKey", "CID", "CanonicalSMILES", "IsomericSMILES", "IUPACName")), .after = identifier)
      
      
      # Remove duplicated columns based on InChI 
      data_query <- data_query %>% distinct(.keep_all = TRUE)
      
      colnames_refmet <- setdiff(names(refmet), "inchi_key")
      
      # Step 2: Merge the result with refmet using the InChIKey
      data_final <- data_query %>%
        left_join(refmet,
                  by = c("InChIKey" = "inchi_key"),
                  relationship = "many-to-many") %>%
        relocate(any_of(colnames_refmet), .after = all_of(identifier))
      
      if (input$online_refmet) {
        print("Online RefMet")
        
        update_modal_spinner(
          session = session,
          text = "Ohh so you choose to look online? This might take an extra bit of time. Please be patient."
        )
        
        sub_data <- data_final[is.na(data_final$refmet_name),]
        sub_data <- sub_data[ !grepl("^_", sub_data[[1]]), ]
        # remove duplicated rows 
        sub_data <- sub_data[!duplicated(sub_data[,identifier]),]
        
        # subset sub_data if Original annotation exist else use Name
        if("Original annotation" %in% colnames(sub_data)) {
          sub_data <- sub_data %>% 
            select("Original annotation") %>% 
            # us gsub to remove everything after ;
            mutate(`Original annotation` = gsub(";.*", "", `Original annotation`))
        } else if ("Original.annotation" %in% colnames(sub_data)) {
            sub_data <- sub_data %>% 
              select("Original.annotation") %>% 
              # us gsub to remove everything after ;
              mutate("Original.annotation" = gsub(";.*", "", `Original.annotation`))
        } else {
          sub_data <- sub_data %>% 
            select(Name)
        }
        
        # make intermediate df for online look up 
        desired_properties <- c(
          "CanonicalSMILES","IsomericSMILES","InChI","InChIKey","IUPACName")
        
        all_results <- data.frame()
        chunk_size <- 5
        
        print(head(sub_data))
        
        if (length(sub_data) > 0) {
          num_rows <- nrow(sub_data)
          print(paste0("# of entries: ", num_rows))
          
          row_indices <- seq(1, num_rows, by = chunk_size)
          print(paste0("Row indices: ", row_indices))
          
          for (start_idx in row_indices) {
            end_idx <- base::min(start_idx + chunk_size - 1, num_rows)
            print(paste0("Index: ", start_idx, "-", end_idx))
            
            sub_data_chunk <- sub_data[start_idx:end_idx,]
            print(sub_data_chunk)
            
            props_chunk <- tryCatch({
              get_properties(
                properties = desired_properties,
                identifier = sub_data_chunk,
                namespace = "name",
                propertyMatch = list(.ignore.case = TRUE, type = "contain")
              )
            }, error = function(e) {
              warning("Failed to get properties for compounds: ", paste(sub_data_chunk, collapse = ", "))
              return(NULL)
            })
            
            if (is.null(props_chunk) || !inherits(props_chunk, "PubChemInstanceList")) {
              print("props_chunk is NULL or not a PubChemInstanceList, skipping this batch.")
              Sys.sleep(1) # respect rate limit
              next
            }
            
            props_retrieved <- tryCatch({
              retrieve(object = props_chunk, .to.data.frame = TRUE, .combine.all = TRUE)
            }, error = function(e) {
              warning("Failed to retrieve data for compounds: ", paste(sub_data_chunk, collapse = ", "))
              return(data.frame())
            })
            
            if (!is.null(props_retrieved) && nrow(props_retrieved) > 0) {
              all_results <- dplyr::bind_rows(all_results, props_retrieved)
            } else {
              print("No valid rows retrieved for this batch.")
            }
            
            print(Sys.time())
            # Pause to respect the 5 queries/second limit
            Sys.sleep(1)
            
          }
        } 
        
        if(!is.null(all_results)) {
          print(head(all_results))
        }

        data_query_online <- data_final %>%
          left_join(
            all_results %>% select(InChI, CanonicalSMILES, InChIKey),
            by = setNames("InChI", identifier),
            relationship = "many-to-many"
          )
        
        # merge two columns with the same name
        data_query_online <- data_query_online %>%
          mutate(
            CanonicalSMILES = coalesce(CanonicalSMILES.x, CanonicalSMILES.y),
            InChIKey = coalesce(InChIKey.x, InChIKey.y)
          ) %>%
          select(-ends_with(".x"), -ends_with(".y")) %>%
          relocate(c("CanonicalSMILES", "InChIKey"), .after = identifier)
        
        ref_cols <- c(
          "refmet_id", "refmet_name", "super_class", "main_class", 
          "sub_class", "formula", "exactmass", "pubchem_cid", 
          "chebi_id", "hmdb_id", "lipidmaps_id", "kegg_id"
        )
        
        data_final <- data_query_online %>%
          left_join(refmet,
                    by = c("InChIKey" = "inchi_key"),
                    relationship = "many-to-many") %>%
          mutate(
            across(
              .cols = any_of(ref_cols),
              .fns = ~ coalesce(
                .data[[ paste0(cur_column(), ".x") ]],
                .data[[ paste0(cur_column(), ".y") ]]
              ),
              .names = "{.col}"
            )
          ) %>%
          # then drop all the .x and .y helper columns
          select(-ends_with(".x"), -ends_with(".y")) %>%
          relocate(any_of(c("refmet_id", "refmet_name", "super_class",
                     "main_class","sub_class", "formula",
                     "exactmass", "pubchem_cid", "chebi_id",
                     "hmdb_id", "lipidmaps_id", "kegg_id")), .after = InChIKey)
        
        data_final[data_final == ""] <- NA
        data_final <- data_final[order(data_final$super_class),]
          
      } else {
        print("Offline RefMet")
        
        update_modal_spinner(
          session = session,
          text = "Very you in a hurry? This does not take as long as online lookup. Please be patient."
        )
        
        data_final <- data_final %>%
          { 
            if ("smiles" %in% colnames(.)) {
              relocate(., CanonicalSMILES, .after = smiles)
            } else {
              relocate(., CanonicalSMILES, .after = !!sym(identifier))
            }
          } %>%
          relocate(InChIKey, .after = !!sym(identifier))
        
        data_final[data_final == ""] <- NA
        data_final <- data_final[order(data_final$super_class),]
      }
      
      data_final <- data_final %>%
        relocate(InChIKey, .after = identifier) %>%
        relocate(CID, .after = InChIKey) %>%
        relocate(IsomericSMILES, .after = CanonicalSMILES) %>%
        relocate(IUPACName, .after = IsomericSMILES)
      
      data_final$CID <- as.character(data_final$CID)
      
      common_cols <- setdiff(intersect(names(data_final), names(refmet)), c("Name", "refmet_name"))
      
      # Perform the left join using data_final[[compound]] and refmet$refmet_name.
      final_data_updated <- data_final %>%
        left_join(refmet, by = setNames("refmet_name", "Name"), suffix = c("", ".refmet")) %>%
        # For each overlapping column (excluding the join key columns), update with the value from refmet if available.
        mutate(across(
          .cols = all_of(common_cols),
          .fns = ~ coalesce(get(paste0(cur_column(), ".refmet")), .x)
        )) %>%
        # Remove the temporary columns from refmet (those ending in ".refmet")
        select(-ends_with(".refmet"))
      
      additional_keys <- c("Original annotation", "Normalized.Name", "Species.Name")  # example additional keys
      
      for(key in additional_keys) {
        if(key %in% names(final_data_updated)) {
          final_data_updated <- final_data_updated %>%
            left_join(refmet, by = setNames("refmet_name", key), suffix = c("", paste0(".", key))) %>%
            mutate(across(
              .cols = all_of(common_cols),
              .fns = ~ coalesce(get(paste0(cur_column(), ".", key)), .x)
            )) %>%
            select(-ends_with(paste0(".", key)))
        }
      }
      
      final_data_updated <- final_data_updated %>%
        # make the pubchem_cid as charactor
        mutate(pubchem_cid = as.character(pubchem_cid))
      
      final_data_updated <- final_data_updated %>%
        mutate(
          CID = coalesce(CID, pubchem_cid),
          InChIKey = coalesce(InChIKey, inchi_key)
        ) %>%
        select(-pubchem_cid, -inchi_key)
      
      data_final <- final_data_updated
      
      
      update_modal_spinner(
        session = session,
        text = "Just a second. I'm updating your sequence and data file. Please be patient."
      )
      
      # Update sequence
      print("Updating sequence...")
      updated_seq <- updateSequence(sequence, data_final, colnames_refmet, "-")
      
      # Store temporarily
      rv$tmpData <- data_final
      rv$tmpSequence <- updated_seq
      
      # Debug
      print("Are data column names and sequence row names identical?")
      print(identical(colnames(rv$tmpData), rownames(rv$tmpSequence)))
      
      # Update sequence
      print("Updating data file...")
      updateDataAndSequence(
        notificationMessage = paste("Identifiers gathered from column:", identifier),
        newFileInput = TRUE,
        suffix = "_RefMet",
        additionalInfo = NULL
      )
      
      remove_modal_spinner()
      sendSweetAlert(session, "Success", "Identifiers gathered and data updated.", type = "success")
      message(sample(quotes, 1))
    }
  })
  
  observeEvent(input$cleanedLipidGroup, {
    show_modal_spinner(
      spin = "atom",
      color = "#0A4F8F",
      text = "Cleaning lipid names - might take a few seconds."
    )
    
    if(is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else {
      sequence <- rv$sequence[[rv$activeFile]]
      data <- rv$data[[rv$activeFile]]
      identifier <- input$name_column_lipids
      
      print(identifier)
      
      original_colnames <- colnames(data)
      message("Cleaning lipid names")
      
      # Extract and clean lipid names
      lipid_names <- data[[identifier]]
      
      # remove NA or empty lipidnames
      lipid_names <- lipid_names[!is.na(lipid_names) & lipid_names != ""]
      
      # only unique names from lipid_names is allowed
      lipid_names <- unique(lipid_names)
      
      multipleLipidNamesDf <- parseLipidNames(lipid_names)
      
      # Save the cleaned lipid names for download
      rv$multipleLipidNamesDf <- multipleLipidNamesDf
      
      print(head(multipleLipidNamesDf[,1:6]))
      
      # if the multipleLipidNamesDf first column is only NA then return NULL 
      if (all(is.na(multipleLipidNamesDf[[1]]))) {
        sendSweetAlert(session, "No lipid names found", "No lipid names found in the data. Try selecting another column. ", type = "error")
        remove_modal_spinner()
        return(NULL)
      }
      
      multipleLipidNamesDf <- multipleLipidNamesDf %>%
        select(Normalized.Name,	Original.Name, Species.Name, Extended.Species.Name)
      
      # join left the data and multipleLipidNamesDf by identifier and Original.Name
      data <- left_join(
        data,
        multipleLipidNamesDf,
        by = setNames("Original.Name", identifier)) %>%
        relocate(c(Normalized.Name, Species.Name, Extended.Species.Name), .after = identifier)
      
      # rename extended species name to lipid abbreviation
      data <- data %>%
        rename(Lipid.Abbreviation = Extended.Species.Name)
      
      cat("# row data", nrow(data), "\n")
      
      if (any(is.na(data$Normalized.Name))) {
        
        message("some normalized names is NA")
        
        # Define known abbreviations to ensure complete coverage
        abbreviations_list <- c(
          "Hex3Cer", "Hex2Cer", "HexCer", "GlcCer", "GalCer", "LacCer", "C1P", "S1P", "SPH",
          "PGM", "PIP", "CDCA", "UDCA", "HDCA",
          "FA", "MG", "DG", "TG", "BMP", "CL", "PA", "PC", "PE", "PG",
          "PI", "PS", "Cer", "SM", "ST", "SE", "FC", "CE", "CA", "CAR", "DCA",
          "LCA", "GCA", "TCA", "LPE", "LNAPE"
        )
        
        # Extract all valid lipid subclasses from refmet
        unique_abbre <- sort(unique(refmet$sub_class[grepl("^[A-Z]+$", refmet$sub_class)]))
        
        # Ensure abbreviations_list is combined with known refmet subclasses
        unique_abbre <- sort(unique(c(unique_abbre, abbreviations_list)))
        
        # Sort abbreviations by length (LPC before PC)
        unique_abbre <- unique_abbre[order(nchar(unique_abbre), decreasing = TRUE)]
        
        # Create abbreviation mapping table
        abbreviations <- unique(refmet[refmet$sub_class %in% unique_abbre, c("main_class", "sub_class")])
        
        # Add missing manual mappings
        manual_mappings <- data.frame(
          main_class = c("Fatty Acid", "Ether Phospholipids","Acylcarnitines", "Ether Glycerophosphocholines", "Ether Diacylglycerols",
                         "Ether Glycerophosphoethanolamines", "Diacylglycerols", "Phosphatidylserines", "Ether Phosphatidylinositol",
                         "Triacylglycerols", "Ether Triacylglycerols", "Cholesterol Esters","Sphingoid bases",
                         "glycosphingolipids","glycosphingolipids","glycosphingolipids", "Phosphatidylcholines","Sterols", "Phosphatidylglycerol"),
          sub_class = c("FA", "O-LPE", "CAR", "O-PC", "O-DG","O-PE", "DG",
                        "O-PS", "O-PI", "TG", "O-TG", "CE", "SPB", "HexCer", "Hex2Cer", "Hex3Cer", "LPC", "ST", "PG")
        )
        
        # Combine with abbreviations
        abbreviations <- bind_rows(abbreviations, manual_mappings)
        
        # Build regex pattern, ensuring LPC is before PC
        abbreviation_pattern <- paste0("\\b(", paste(unique_abbre, collapse = "|"), ")(\\(O-)?")
        
        # print(abbreviation_pattern)
        
        # subset data to only contain those without normalized names 
        data_sub <- data[is.na(data$Normalized.Name),]
        
        data_sub <- as.data.frame(data_sub[,all_of(identifier)])

        # remove duplicated rows
        data_sub <- as.data.frame(data_sub[!duplicated(data_sub),])
        names(data_sub) <- c(identifier)
        
        data_sub <- data_sub %>%
          mutate(lipid_abbreviation = str_extract(.data[[identifier]], abbreviation_pattern))
        
        calculate_sum_name <- function(lipid_name) {
          # Extract the head group (assumed to be the first word)
          head_group <- str_extract(lipid_name, "^[A-Za-z]+")
          
          # Define a regex pattern that captures:
          #   (1) an optional "O-" prefix,
          #   (2) the carbon count (digits),
          #   (3) the double bond count (digits),
          #   (4) an optional oxygen info starting with ";" (if present),
          #   (5) an optional extra trailing info (e.g. "-SN1")
          chain_pattern <- "(O-)?(\\d+):(\\d+)(;[^\\s\\)\\/_]+)?(-[A-Za-z0-9]+)?"
          
          chains <- str_match_all(lipid_name, chain_pattern)[[1]]
          
          # If no matches are found, return NA
          if(nrow(chains) == 0) return(NA)
          
          # According to our pattern:
          #   Column 2: optional "O-" prefix
          #   Column 3: carbons
          #   Column 4: double bonds
          #   Column 5: oxygen info (e.g. ";O2")
          #   Column 6: extra trailing info (e.g. "-SN1")
          carbons <- as.numeric(chains[,3])
          dblbonds <- as.numeric(chains[,4])
          
          total_carbons <- sum(carbons, na.rm = TRUE)
          total_db <- sum(dblbonds, na.rm = TRUE)
          
          # Special case: if there's a fatty acid specification in parentheses and head group is SM,
          # add extra 2 double bonds.
          if(grepl("\\(FA", lipid_name) && head_group == "SM") {
            total_db <- total_db + 2
          }
          
          # Collect extra information from each chain match (using the first non-empty occurrence)
          extra_info <- ""
          for(i in 1:nrow(chains)) {
            prefix    <- ifelse(is.na(chains[i,2]), "", chains[i,2])  # "O-" prefix
            oxy_info  <- ifelse(is.na(chains[i,5]), "", chains[i,5])   # oxygen info (e.g. ";O2")
            trailing  <- ifelse(is.na(chains[i,6]), "", chains[i,6])   # trailing extra info (e.g. "-SN1")
            
            combined <- paste0(prefix, oxy_info, trailing)
            if(combined != "") {
              extra_info <- combined
              break
            }
          }
          
          # Adjust placement of extra info:
          # If the extra info is exactly "O-", we want it to come before the numeric chain.
          if(extra_info == "O-") {
            summed_chain <- paste0(extra_info, total_carbons, ":", total_db)
          } else {
            summed_chain <- paste0(total_carbons, ":", total_db, extra_info)
          }
          
          # For some lipids (example: when the name contains "/" and head_group is SM),
          # you may want to force the head group to lowercase.
          if(grepl("/", lipid_name) && head_group == "SM") {
            head_group <- tolower(head_group)
          }
          
          sum_name <- paste0(head_group, " ", summed_chain)
          return(sum_name)
        }
        
        # Apply the function only when lipid_abbreviation is not NA:
        data_sub <- data_sub %>%
          mutate(sum_name = mapply(function(name, abbr) {
            if (is.na(abbr)) {
              NA_character_
            } else {
              calculate_sum_name(name)
            }
          }, .data[[identifier]], lipid_abbreviation)) %>%
          relocate(sum_name, .after = .data[[identifier]])
        
        # Fix O- notation dynamically
        data_sub$lipid_abbreviation <- ifelse(
          grepl("\\(O-", data_sub$lipid_abbreviation),
          paste0("O-", gsub("\\(O-", "", data_sub$lipid_abbreviation)),
          data_sub$lipid_abbreviation
        )
        
        # Replace NA values in abbreviations with NA
        data_sub$lipid_abbreviation[is.na(data_sub$lipid_abbreviation)] <- NA
        
        # Join with abbreviations to get lipid_class
        data_sub <- data_sub %>%
          left_join(abbreviations, by = c("lipid_abbreviation" = "sub_class"),
                    relationship = "many-to-many") %>%
          { 
            if("main_class.x" %in% names(.)) {
              rename(., main_class = main_class.x) %>% select(-main_class.y)
            } else {
              .
            }
          } %>%
          rename(lipid_class = main_class) %>%
          relocate(lipid_class, .after = lipid_abbreviation)
        
        # Replace NA in lipid_class with NA
        data_sub$lipid_class[is.na(data_sub$lipid_class)] <- NA
        
        data_sub <- data_sub %>%
          distinct_at(vars(identifier), .keep_all = TRUE)
        
        message("Data with NA in sum_name or lipid_abbreviation:")
        print(data_sub[is.na(data_sub$sum_name) | is.na(data_sub$lipid_abbreviation),])
        
      }
      
      message("Head of data file: ")
      print(head(data[,1:6]))
      
      cat("# row data", nrow(data), "\n")
      
      data <- data %>%
        left_join(data_sub,
                  by = identifier,
                  relationship = "many-to-many") %>%
        mutate(
          Normalized.Name   = coalesce(Normalized.Name, sum_name),
          Species.Name      = coalesce(Species.Name, sum_name),
          Lipid.Abbreviation = coalesce(Lipid.Abbreviation, lipid_abbreviation)
        ) %>%
        rename(Lipid.Class = lipid_class) %>% 
        relocate(Lipid.Class, .after = Lipid.Abbreviation) %>%
        select(-sum_name, -lipid_abbreviation, -Lipid.Class)
      
      cat("# row data", nrow(data), "\n")

      colnames_cleaned <- setdiff(colnames(data), original_colnames)
      
      # Debug
      print("Are data column names and sequence row names identical?")
      print(identical(colnames(rv$tmpData),
                      rownames(rv$tmpSequence)))
      
      update_modal_spinner(
        session = session,
        text = "Just a second. I'm updating your sequence and data file. Please be patient."
      )
      
      # Update sequence
      print("Updating sequence...")
      updated_seq <- updateSequence(sequence, data, colnames_cleaned, "-")
      
      # Store temporarily
      rv$tmpData <- data
      rv$tmpSequence <- updated_seq
      
      # Update sequence
      print("Updating data file...")
      updateDataAndSequence(
        notificationMessage = paste("Identifiers gathered from column:", identifier),
        newFileInput = TRUE,
        suffix = "_LipClea",
        additionalInfo = NULL
      )
      
    }
    
    remove_modal_spinner()
    sendSweetAlert(session, "Success",
                   "Lipid names have been cleaned to standardization and column with lipid group has been added.",
                   type = "success")
    message(sample(quotes, 1))
  })
  
  observeEvent(input$RemoveUnannotated, {
    show_modal_spinner(
      spin = "atom",
      color = "#0A4F8F",
      text = "Remove unannotated features."
    )
    
    if(is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else {
      sequence <- rv$sequence[[rv$activeFile]]
      data <- rv$data[[rv$activeFile]]
      identifier <- input$name_column_annotate
      
      print(identifier)
      
      original_colnames <- colnames(data)
      
      num_feature_before <- nrow(data)
      
      
      # remove rows with NA in identifier column
      data <- data[!is.na(data[[identifier]]),]
      # remove rows that is empty string
      data <- data[data[[identifier]] != "",]
      
      num_feature_after <- nrow(data)
      
      colnames_cleaned <- setdiff(colnames(data), original_colnames)
      
      # Debug
      print("Are data column names and sequence row names identical?")
      print(identical(colnames(rv$tmpData),
                      rownames(rv$tmpSequence)))
      
      update_modal_spinner(
        session = session,
        text = "Just a second. I'm updating your sequence and data file. Please be patient."
      )
      
      # Update sequence
      print("Updating sequence...")
      updated_seq <- updateSequence(sequence, data, colnames_cleaned, "-")
      
      # Store temporarily
      rv$tmpData <- data
      rv$tmpSequence <- updated_seq
      
      # Debug
      print("Are data column names and sequence row names identical?")
      print(identical(colnames(rv$tmpData), rownames(rv$tmpSequence)))
      
      # Update sequence
      print("Updating data file...")
      updateDataAndSequence(
        notificationMessage = paste("Unnanotated features removed from column: ", identifier),
        newFileInput = TRUE,
        suffix = "_Anno",
        additionalInfo = NULL
      )
      
    }
    
    remove_modal_spinner()
    sendSweetAlert(
      session,
      title = "Success!",
      text = paste0(
        "Unannotated features have been successfully removed.\n\n",
        "Features before: ", num_feature_before, "\n",
        "Features after: ", num_feature_after
      ),
      type = "success",
      btn_labels = "OK"
    )
    message(sample(quotes, 1))
  })
  
  observeEvent(input$editColumns, {
    showModal(
      modalDialog(
        title = "Edit columns", size = "s", easyClose = TRUE,
        footer = list(actionButton("edit_cols_confirm", "Confirm"), modalButton("Dismiss")),
        fluidRow(
          column(width = 9, h4("Column name")),
          column(width = 3, style = "text-align: left;", h4("Keep"))
        ),
        lapply(seq(ncol(rv$data[[rv$activeFile]])), function(x) {
          fluidRow(
            column(
              width = 9,
              textInput(paste0("column_edit_name", x), NULL, value = colnames(rv$data[[rv$activeFile]])[x])
            ),
            column(
              width = 3, style = "text-align: center;",
              prettyCheckbox(paste0("columns_to_keep", x), NULL, status = "info", value = T)
            ),
          )
        })
      )
    )
  })
  
  observeEvent(input$edit_cols_confirm, {
    ncol <- ncol(rv$data[[rv$activeFile]])
    column_names <- character()
    column_names <- sapply(seq(ncol), function(x) {
      input[[paste0("column_edit_name", x)]]
    })
    if(!checkDuplicates(column_names)) {
      isolate(colnames(rv$data[[rv$activeFile]]) <- column_names)
      isolate(row.names(rv$sequence[[rv$activeFile]]) <- column_names)
      keep <- sapply(seq(ncol), function(x) input[[paste0("columns_to_keep", x)]])
      rv$data[[rv$activeFile]] <- rv$data[[rv$activeFile]][, keep]
      rv$sequence[[rv$activeFile]] <- rv$sequence[[rv$activeFile]][keep, ]
    }
    removeModal()
  })
  
  observeEvent(input$editGroups, {
    sequence <- rv$sequence[[rv$activeFile]]
    unique_groups <- unique(na.omit(sequence[, 4]))
    # Generate UI elements for each group
    group_ui_elements <- lapply(seq(unique_groups), function(x) {
      group <- unique_groups[x]
      fluidRow(
        column(3, h5(group)),
        column(9,
               textInput(paste0("edit_nickname", group), NULL, value = NULL)
        ),
      )
    })
    showModal(
      modalDialog(
        title = "Edit Group Nicknames", size = "s", easyClose = TRUE,
        footer = list(actionButton("group_edit_confirm", "Confirm"), modalButton("Dismiss")),
        fluidRow(
          column(3, h4("Group")),
          column(9, h4("Nickname"))
        ), 
        do.call(tagList, group_ui_elements)
      )
    )
  })
  
  observeEvent(input$group_edit_confirm, {
    sequence <- rv$sequence[[rv$activeFile]]
    groups <- sequence[, 4]
    for (x in seq_along(groups)) {
      if (!is.na(groups[x])) {
        input_x <- input[[paste0("edit_nickname", groups[x])]]
        if (nchar(input_x) != 0 & isValidName(input_x)) {
          sequence[x, 4] <- input_x
        }
      }
    }
    rv$sequence[[rv$activeFile]] <- sequence
    removeModal()
  })
  
  observeEvent(input$example, {
    # Load example files
    # Negative ion mode
    data <- read.csv("./example_files/Liverfetus_lipid_neg1.csv", stringsAsFactors = FALSE)
    sequence <- read.csv("./example_files/fetus seq neg.csv", stringsAsFactors = FALSE)
    # look for column "Name" or "name" and make it the first column 
    if("Name" %in% colnames(data)) {
      data <- data[, c("Name", setdiff(colnames(data), "Name"))]
    } else if("name" %in% colnames(data)) {
      data <- data[, c("name", setdiff(colnames(data), "name"))]
    }
    
    row.names(sequence) <- sequence[, 1]
    # look for row "Name" or "name" and make it the first row
    if("Name" %in% rownames(sequence)) {
      sequence <- rbind(sequence["Name", ], sequence[-which(rownames(sequence) == "Name"), ])
    } else if("name" %in% rownames(sequence)) {
      sequence <- rbind(sequence["name", ], sequence[-which(rownames(sequence) == "name"), ])
    }
    sequence <- sequence[, -1]
    rv$sequence[[length(rv$sequence) + 1]] <- sequence
    rv$data[[length(rv$data) + 1]] <- data
    names(rv$data)[length(rv$data)] <- "Liverfetus_negative"
    initializeVariables()
    
    # Positive ion mode
    data <- read.csv("./example_files/Liverfetus_lipid_pos1.csv", stringsAsFactors = FALSE)
    sequence <- read.csv("./example_files/fetus seq pos.csv", stringsAsFactors = FALSE)
    # look for column "Name" or "name" and make it the first column 
    if("Name" %in% colnames(data)) {
      data <- data[, c("Name", setdiff(colnames(data), "Name"))]
    } else if("name" %in% colnames(data)) {
      data <- data[, c("name", setdiff(colnames(data), "name"))]
    }
    row.names(sequence) <- sequence[, 1]
    # look for row "Name" or "name" and make it the first row
    if("Name" %in% rownames(sequence)) {
      sequence <- rbind(sequence["Name", ], sequence[-which(rownames(sequence) == "Name"), ])
    } else if("name" %in% rownames(sequence)) {
      sequence <- rbind(sequence["name", ], sequence[-which(rownames(sequence) == "name"), ])
    }
    sequence <- sequence[, -1]
    rv$sequence[[length(rv$sequence) + 1]] <- sequence
    rv$data[[length(rv$data) + 1]] <- data
    names(rv$data)[length(rv$data)] <- "Liverfetus_positive"
    initializeVariables()
    rv$choices <- paste(seq_along(rv$data), ": ", names(rv$data))
    
    updateTabItems(session, "tabs", selected = "Datainput")
    show("buttons")
    updateCollapse(session, "menu", close = "Data input")
    disable("example")
  })
  
  # Update selected data
  observeEvent(input$selectDataset, ignoreInit = TRUE, {
    rv$activeFile <- which(rv$choices %in% input$selectDataset)
    
    output$seq_table <- renderDT(rv$sequence[[rv$activeFile]], extensions = 'Responsive', server = F, 
                                 editable = T, selection = 'none', options = list(pageLength = nrow(rv$sequence[[rv$activeFile]]), 
                                                                                  scrollX = TRUE))
    
    # Make a debugging statement for the active file
    print(paste0("Active file is ", rv$activeFile))
    # Make a debugging statement for the choices
    print(paste0("Choices are ", rv$choices))
    # Make a debugging statement for the selectDataset
    print(paste0("Select Dataset is ", input$selectDataset))
    
    # TODO: Maybe use in future  
    # Make a debugging statement for the datatype
    # datatype <- input$dataType
    # print(paste0(datatype))
    
    output$seq_table <- renderDT(rv$sequence[[rv$activeFile]],
                                 extensions = 'Responsive',
                                 server = F,
                                 editable = T,
                                 selection = 'none',
                                 options = list(pageLength = nrow(rv$sequence[[rv$activeFile]]),
                                                scrollX = TRUE))
    output$diboxtitle <- renderText(names(rv$data[rv$activeFile]))
    
    output$dttable <- renderDT(rv$data[[rv$activeFile]], rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px"))
    
    output$dt_drift_panel <- renderDT(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"], rownames = FALSE, 
                                      options = list(autoWidth = TRUE, scrollY = "700px", pageLength = 20))
    
    output$dt_boxplot_panel <- renderDT(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"],
                                        rownames = FALSE, 
                                        options = list(autoWidth = TRUE,
                                                       scrollY = "700px",
                                                       pageLength = 20))
    
    data <- rv$data[[rv$activeFile]]
    sequence <- rv$sequence[[rv$activeFile]]
    
    output$histogram <- renderPlotly({
      samples <- data[, sequence[ , 'labels'] %in% "Sample"]
      medians <- apply(samples, 2, median, na.rm = TRUE)
      median_data <- data.frame(
        Sample = names(medians),
        Median = medians
      )
      ggplot(median_data, aes(x = Sample, y = Median)) +
        geom_col(fill = "skyblue", color = "black", width = 0.7) +
        labs(x = "Samples", y = "Median") +
        theme_minimal() +
        theme(axis.text.x = element_blank())
    })
    
    output$histogram_qc <- renderUI({
      QCs <- data[, sequence[ , 'labels'] %in% "QC"]
      if(ncol(QCs) > 0) {
        medians <- apply(QCs, 2, median, na.rm = TRUE)
        median_QC <- data.frame(
          QC = names(medians),
          Median = medians
        )
        plotlyOutput("qc_distribution")
        output$qc_distribution <- renderPlotly({
          ggplot(median_QC, aes(x = QC, y = Median)) +
            geom_col(fill = "skyblue", color = "black") +
            labs(x = "Sample", y = "Median") +
            theme_minimal()
        })
      }
      else {
        textOutput("No columns labeled QC.")
      } 
    })
    output$dt_boxplot_panel <- renderDT(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"],
                                        rownames = FALSE, 
                                        options = list(autoWidth = TRUE,
                                                       scrollY = "700px",
                                                       pageLength = 20))
    
    
    # Plot of classes in the identifer table
    output$class_plot <- renderPlotly({
      req(input$selected_column_class_plot)  # Ensure a column is selected
      
      # Copy dataset
      data_sub <- data
      colname <- input$selected_column_class_plot
      
      # calculate how many invalid values are in the selected column
      invalid_values <- sum(is.na(data_sub[[colname]]) | data_sub[[colname]] == "" | 
                              data_sub[[colname]] == " " | data_sub[[colname]] == "NA" | 
                              data_sub[[colname]] == "N/A")
      
      # Remove rows where the selected column is NA, empty, or contains unwanted strings.
      data_sub <- data_sub[!is.na(data_sub[[colname]]) &
                             data_sub[[colname]] != "" &
                             data_sub[[colname]] != " " &
                             data_sub[[colname]] != "NA" &
                             data_sub[[colname]] != "N/A", ]
      
      # Check if the column exists (should always be true if choices are set correctly)
      if (!(colname %in% colnames(data_sub))) {
        showNotification(
          paste("No", colname, "column found in the dataset. Make sure a column named", colname, "is present by running 'Gather Identifiers'."),
          type = "message",
          duration = 10
        )
        return(NULL)
      }
      
      # Create the ggplot, sorting and converting the selected column to a factor
      p <- data_sub %>%
        arrange(.data[[colname]]) %>%
        mutate(!!colname := factor(.data[[colname]], levels = unique(.data[[colname]]))) %>%
        ggplot(aes(x = !!sym(colname))) +
        geom_bar(fill = "steelblue", color = "black", width = 0.4) +
        geom_text(stat = "count", aes(label = after_stat(count)), vjust = -15, size = 4.5, color = "red") +
        labs(
          title = paste0("Distribution of ", colname, " - number of features: ", nrow(data), 
                         "\n Invalid values: ", invalid_values),
          x = colname,
          y = "Count"
        ) +
        theme_bw(base_size = 11) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
      
      # Convert the ggplot object to an interactive Plotly plot
      ggplotly(p)
    })
    
    if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Name") == 1) { #TODO check if this check is needed
      internalStandards <- findInternalStandards(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"])
      updateCheckboxGroupInput(session, "isChoose", choices = internalStandards, selected = internalStandards)
      enable("normalizeIS"); enable("removeIS"); enable("saveIS")
      if(length(internalStandards) == 0) {
        disable("normalizeIS"); disable("removeIS"); disable("saveIS")
      } 
    }
  })
  
  observe({
    req(rv$activeFile,
        rv$data[[rv$activeFile]],
        rv$sequence[[rv$activeFile]])
    
    data <- rv$data[[rv$activeFile]]
    sequence <- rv$sequence[[rv$activeFile]]
    
    output$histogram <- renderPlotly({
      
      message("Inside group histogram")
      # Filter sample columns based on sequence labels
      samples <- data[, sequence[, "labels"] %in% "Sample"]
      
      # Compute medians for each sample
      medians <- apply(samples, 2, median, na.rm = TRUE)
      median_data <- data.frame(
        Sample = names(medians),
        Median = medians,
        stringsAsFactors = FALSE
      )
      
      # Add group information if present
      if ("group" %in% colnames(sequence) && !all(is.na(sequence[, "group"]))) {
        group_data <- data.frame(
          Sample = colnames(data),
          Group = sequence[, "group"],
          stringsAsFactors = FALSE
        )
        median_data <- merge(median_data, group_data, by = "Sample", all.x = TRUE)
        
        # Ensure the samples are ordered within each group
        median_data <- median_data %>%
          arrange(Group, mixedorder(Sample)) %>% # Use mixedorder for natural sorting
          mutate(
            Group = factor(Group, levels = unique(Group)), # Preserve group order
            Sample = factor(Sample, levels = unique(Sample)) # Preserve sample order
          )
        
        # Create a grouped bar chart with distinct group colors
        p <- ggplot(median_data,
                    aes(x = Sample,
                        y = Median,
                        fill = Group)) +
          geom_col(color = "black", width = 0.7) +
          labs(x = "Samples", y = "Median", fill = "Group") +
          facet_wrap(~Group, scales = "free_x", nrow = 1) + 
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right",
            strip.text = element_text(size = 12, face = "bold")
          )
      } else {
        # Ensure samples are sorted naturally (numeric first, then lexicographical)
        median_data <- median_data %>%
          arrange(mixedorder(Sample)) %>% # Use mixedorder for natural sorting
          mutate(Sample = factor(Sample, levels = unique(Sample)))
        
        p <- ggplot(median_data, aes(x = Sample, y = Median)) +
          geom_col(fill = "skyblue", color = "black", width = 0.7) +
          labs(x = "Samples", y = "Median") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      ggplotly(p)
    })
    
    output$histogram_qc <- renderUI({
      QCs <- data[, sequence[, "labels"] %in% "QC"]
      if (ncol(QCs) > 0) {
        medians <- apply(QCs, 2, median, na.rm = TRUE)
        median_QC <- data.frame(
          QC = names(medians),
          Median = medians
        )
        plotlyOutput("qc_distribution")
        output$qc_distribution <- renderPlotly({
          ggplot(median_QC, aes(x = QC, y = Median)) +
            geom_col(fill = "skyblue", color = "black") +
            labs(x = "QC", y = "Median") +
            theme_minimal()
        })
      } else {
        textOutput("No columns labeled QC.")
      }
    })
    
    output$violin_plot <- renderPlotly({
      message("Inside violin plot")
      
      seq_sub <- sequence[sequence$labels == "Sample", ]

      data_sub <- data[, rownames(seq_sub)] 
      
      seq_sub <- seq_sub %>% 
        rownames_to_column("sample")
      
      data_long <- data_sub %>%
        pivot_longer(cols = everything(), names_to = "sample", values_to = "value")
      
      data_long <- left_join(data_long, seq_sub, by = "sample")
      
      data_long <- data_long[!is.na(data_long$value), ]
      
      sample_size <- data_long %>%
        group_by(group) %>%
        summarize(num = n())
      
      data_long <- data_long %>%
        left_join(sample_size, by = "group") %>%
        mutate(myaxis = paste0(group, "\n", "n=", num))
      
      print(head(data_long))
      print(head(sample_size))
      
      # Create the base plot
      violin_layer <- ggplot(data_long, aes(x = myaxis, y = value, fill = group)) +
        geom_violin(width = 1.4) + 
        # scale_fill_viridis(discrete = TRUE) +
        theme_bw() +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 11)
        ) +
        ggtitle("Violin Plot with Boxplot and Sample Size") +
        xlab("")
      
      boxplot_layer <- geom_boxplot(width = 0.1, color = "red", alpha = 0.2)
      
      # Combine them into the final plot
      violin_plot <- violin_layer + boxplot_layer
      
      # For interactivity with plotly:
      ggplotly(violin_plot)
    })
    
  })
  
  # Observer for list of all the datasets 
  observeEvent(rv$choices, {
    choices <- rv$choices
    num_datasets <- length(choices)
    
    output$downloadSequence <- downloadHandler(
      filename <- function() {
        paste0(names(rv$data[rv$activeFile]), "_seq.csv")
      },
      content = function(file) {
        write.csv(cbind("sample" = rownames(rv$sequence[[rv$activeFile]]), rv$sequence[[rv$activeFile]]), file, row.names = FALSE) # TODO
      }
    )
    
    # Export panel
    output$export_ui <- renderDownloadUI("dwn_general", ".csv")
    output$export_metabo <- renderDownloadUI("dwn_metabo", "_metabo.csv")
    output$export_stats <- renderDownloadUI("dwn_stats", "_results.xlsx")
    output$export_settings <- renderDownloadUI("dwn_settings", ".txt")
    
    output$downloadLipids <- downloadHandler(
      filename = function() {
        paste0("cleaned_lipid_names_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(rv$multipleLipidNamesDf, file, row.names = FALSE)
      }
    )
    
    #    lapply(seq_len(num_datasets), createDownloadHandler("general", ".csv", rv$data[[rv$activeFile]]))
    #    lapply(seq_len(num_datasets), createDownloadHandler("stats", ".xlsx", rv$results[[rv$activeFile]]))
    #    lapply(seq_len(num_datasets), createDownloadHandler("settings", ".txt", rv$info[[rv$activeFile]]))
    #    lapply(seq_len(num_datasets), createDownloadHandler("metabo", ".csv", getMetaboData))
    
    lapply(1:length(rv$choices), function(x) {
      output[[paste0("dwn_stats", x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), "_results.xlsx")
        },
        content = function(file) {
          write_xlsx(rv$results[[x]], file)
        }
      )
    })
    lapply(1:length(rv$choices), function(x) {
      output[[paste0("dwn_general", x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), ".csv")
        },
        content = function(file) {
          write.csv(rv$data[[x]], file, row.names = FALSE)
        }
      )
    })
    lapply(1:length(rv$choices), function(x) {
      output[[paste0("dwn_settings", x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), ".txt")
        },
        content = function(file) {
          write.csv(rv$info[x], file, row.names = FALSE)
        }
      )
    })
    lapply(1:length(rv$choices), function(x) {
      dat <- rv$data[[x]]
      seq <- rv$sequence[[x]]
      seq[seq[, 1] %in% "QC", 4] <- "QC"
      group <- c("", seq[seq[, 1] %in% c("Sample", "QC"), 4])
      outdat <- data.frame(dat[seq[, 1] %in% "Name"], dat[seq[, 1] %in% c("Sample", "QC")])
      outdat <- rbind(group, outdat)
      output[[paste0("dwn_metabo", x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), "_metabo.csv")
        },
        content = function(file) {
          write.csv(outdat, file, row.names = FALSE)
        }
      )
    })
    
    updateCheckboxGroupInput(session, "export_xml_list", choices = choices, selected = NULL)
    updateCheckboxGroupInput(session, "filesToRemove", choices = names(rv$data), selected = NULL)
    updateSelectInput(session, "drift_select", choices = c("None", choices))
    
    inputs <- c("selectDataset", "mergeFile",
                "selectpca1", "selectpca2",
                "select_data_for_enrichment",
                "select_data_circular_barplot",
                "select_heatmap_data", "select_volcano_data",
                "select_OR_data") # Update select inputs
    
    selected <- ifelse(is.null(rv$activeFile), length(choices), rv$activeFile)
    for(input in inputs) {
      updateSelectInput(session, input, choices = choices, selected = choices[selected])
    }
    
  })
  
  observeEvent(input$removeFiles, {
    if (is.null(input$filesToRemove)) {
      showNotification("No files selected.", type = "error")
    } else if(length(input$filesToRemove) == length(rv$choices)) {
      showNotification("At least one file must be kept.", type = "error")
    } else {
      keep <- !names(rv$data) %in% input$filesToRemove      
      rv$data <- rv$data[keep]
      rv$sequence <- rv$sequence[keep]
      rv$info <- rv$info[keep]
      rv$results <- rv$results[keep]
      rv$activeFile <- length(rv$data)
      rv$choices <- paste(seq_along(rv$data), ": ", names(rv$data))
      showNotification("Files removed.", type = "message")
    }
  })
  
  observeEvent(input$export_xml_list, {
    output$export_xml <- downloadHandler(
      filename = function() {
        paste0(names(rv$data[rv$choices %in% input$export_xml_list])[1], ".xlsx")
      },
      content = function(file) {
        write_xlsx(rv$data[rv$choices %in% input$export_xml_list], file)
      }
    )
  })
  
  observeEvent(input$seq_table_cell_edit, {
    sequence <- rv$sequence[[rv$activeFile]]
    info <- input$seq_table_cell_edit
    str(info)
    i <- info$row
    j <- info$col
    v <- info$value
    if(j == 1) {
      sendSweetAlert(session, title = "Warning", text = "Column 'labels' cannot be edited", type = "warning")
    } else {
      sequence[i, j] <- v
      rv$sequence[[rv$activeFile]] <- sequence
    }
  })
  
  observeEvent(input$updateSequence, {
    if (!is.null(rv$activeFile) && !is.null(rv$tmpSequence)) {
      rv$sequence[[rv$activeFile]] <- rv$tmpSequence
      rv$tmpSequence <- NULL
    } else {
      showNotification("No changes to update", type = "message")
    }
  })
  
  
  ####################
  # Blank filtration #
  ####################
  
  observeEvent(input$blankFiltrate, {
    if(is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if (!"QC" %in% rv$sequence[[rv$activeFile]][, 1]) {
      showNotification("Data must have at least 1 QC", type = "error")
    } else if (!"Blank" %in% rv$sequence[[rv$activeFile]][, 1]) {
      showNotification("Data must have at least 1 Blank", type = "error")
    } else if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Name") != 1) {
      showNotification("Data must have exactly 1 \"Name\" column", type = "error")
    } else {
      sequence <- rv$sequence[[rv$activeFile]]
      data <- rv$data[[rv$activeFile]]
      
      filtered <- blankFiltration(data, sequence, input$signalStrength, input$keepIS)
      
      if(input$discardBlank) {
        filtered <- filtered[!sequence[, 1] %in% "Blank"]
        sequence <- sequence[!sequence[, 1] %in% "Blank", ]
      }
      
      rv$tmpData <- filtered
      rv$tmpSequence <- sequence
      
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(session, "Success", paste0(nrow(rv$data[[rv$activeFile]]) - nrow(rv$tmpData), " features removed"), type = "success")
    }
  })
  
  observeEvent(input$saveBF, {
    additionalInfo <- paste("Blank filtrated with signal strength above blank =", input$signalStrength)
    updateDataAndSequence("Blank filtrate first", input$newFileBF, paste("_", input$signalStrength, "xb"), additionalInfo)
  })
  
  
  ####################
  # IS normalization #
  ####################
  
  observeEvent(input$normalizeIS, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Name") != 1) {
      showNotification("Data must have exactly 1 \"Name\" column", type = "error")
    } else if (is.null(input$isChoose)) {
      showNotification("No internal standards selected", type = "error")
    } else {
      sequence <- rv$sequence[[rv$activeFile]]
      data <- rv$data[[rv$activeFile]]
      
      normalized <- normalizationIS(data, sequence, input$isChoose, input$isMethod, input$normalizeQC)
      
      if(is.null(normalized)) {
        sendSweetAlert(session, "Error", "Internal standard normalization failed due to missing values in IS.", type = "error")
      }
      else {
        # Add isnorm column to sequence
        isColumn <- c("-", rep(NA, ncol(sequence) - 1))
        sequence <- rbind(sequence, isColumn)
        rownames(sequence)[nrow(sequence)] <- "isnorm"
        
        rv$tmpData <- normalized
        rv$tmpSequence <- sequence
        
        sendSweetAlert(session, title = "Success", text = paste0("Internal standards normalized with ", input$isMethod, " method"), type = "success")
        updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
        output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      }
    }
  })
  
  observeEvent(input$saveIS, {
    additionalInfo <- paste("Internal standards normalized with", input$isMethod, "method")
    updateDataAndSequence("IS normalize first", input$newFileIS, "_is", additionalInfo)
  })
  
  observeEvent(input$removeIS, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]
      toRemove <- data[sequence[, 1] %in% "Name"]
      data <- data[!grepl("\\(IS\\)", toupper(toRemove[ , 1])), ]
      rv$data[[rv$activeFile]] <- data
      updateCheckboxGroupInput(session, "isChoose", choices = character(0), selected = NULL)
    }
  })
  
  
  ############################
  # Missing value filtration #
  ############################
  
  observeEvent(input$runFilterNA, {
    if(is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else {
      sequence <- rv$sequence[[rv$activeFile]]
      method <- input$filterNAmethod
      if(("in group" %in% method) & !any(complete.cases(sequence[, 4]))) {
        sendSweetAlert(session, "Error!", "Group information needed.", type = "error")
      }
      else if(is.null(method)) {
        sendSweetAlert(session, "Error!", "No method selected.", type = "error")
      }
      else {
        mvf_dat <- cutoffrm(rv$data[[rv$activeFile]], sequence, input$cutoffNAs, method)
        rv$tmpData <- mvf_dat
        rv$tmpSequence <- sequence
        updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
        output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
        sendSweetAlert(
          title = "Success",
          text = paste0(nrow(rv$data[[rv$activeFile]]) - nrow(rv$tmpData), " feature(s) removed"),
          type = "success"
        )
      }
    }
  })
  
  observeEvent(input$saveFilterNA, {
    additionalInfo <- paste(
      "Missing value filtration using",
      input$cutoffNAs,
      "% as threshold and method -",
      paste(input$filterNAmethod, collapse=", ")
    )
    updateDataAndSequence("Filtrate first", input$mvf_newsave, "_mvr", additionalInfo)
  })
  
  
  ##############
  # Imputation #
  ##############
  
  observeEvent(input$runImputation, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Sample") < 1) {
      showNotification("Data must have at least one Sample", type = "error")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]
      
      # check that in sequence contain QC samples in the labels column 
      if (sum(sequence[, "labels"] %in% "QC") < 1) {
        showNotification("Data must have at least one QC sample", type = "error")
        return(NULL)
      }
      
      imputed <- imputation(data, sequence, input$imputationMethod, input$imputationMinX, input$imp_onlyQC, input$remainingNAs)
      
      rv$tmpData <- imputed
      rv$tmpSequence <- sequence
      
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(
        title = "Success",
        text = paste0(sum(is.na(rv$data[[rv$activeFile]]) | rv$data[[rv$activeFile]] == 0) - sum(is.na(rv$tmpData) | rv$tmpData == 0), " missing values were imputed."),
        type = "success"
      )
    }
  })
  
  observeEvent(list(input$imputationMethod, input$remainingNAs), {
    if (input$imputationMethod == "KNN") {
      hide("imp_minx_hide")
      hide("imp_remaining_hide")
    } else {
      show("imp_remaining_hide")
    }
    
    if (input$imputationMethod == "Min/X" || input$remainingNAs == "Min/X") {
      show("imp_minx_hide")
    } else {
      hide("imp_minx_hide")
    }
  })
  
  observeEvent(input$saveImputation, {
    additionalInfo <- paste("Missing values imputation with", input$imputationMethod)
    updateDataAndSequence("Impute first", input$newFileImp, "_imp", additionalInfo)
  })
  
  ####################
  # Drift correction #
  ####################
  
  # observeEvent(input$driftMethod, {
  #   if (input$driftMethod == "QC-RFSC (random forrest)") {
  #     hide("dc_qcspan_hide")
  #     hide("dc_degree_hide")
  #     show("dc_ntree_hide")
  #   } else {
  #     hide("dc_ntree_hide")
  #     show("dc_qcspan_hide")
  #     show("dc_degree_hide")
  #   }
  # })
  
  observeEvent(input$runDrift, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if (is.null(rv$sequence[[rv$activeFile]])) { #TODO remove? sequence is never NULL
      showNotification("No sequence file", type = "error")
    } else if (all(is.na(rv$sequence[[rv$activeFile]][, 'order']))) {
      showNotification("No order information, upload sequence", type = "error")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]  
      dat_qc <- data[, sequence[, 1] %in% "QC"]
      
      if(any(colSums(!is.na(dat_qc)) != nrow(dat_qc))) {
        sendSweetAlert(session = session, title = "Error", text = "QCs cannot have missing values.", type = "error")
      }
      else {
        corrected <- driftCorrection(data, sequence, input$driftMethod, input$driftTrees, input$driftDegree, input$driftQCspan)
        
        rv$tmpData <- corrected
        rv$tmpSequence <- sequence
        
        updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))

        output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      }
    }
  })
  
  observeEvent(input$saveDrift, {
    additionalInfo <- paste(
      "Drift correction applied using method: ", input$driftMethod,
      "with ", input$driftTrees, " trees."
    )
    updateDataAndSequence("Drift correct first", input$newFileDrift, "_dc", additionalInfo)
  })
  
  
  ##################
  # Merge datasets #
  ##################
  
  observeEvent(input$editRankings, {
    showModal(
      modalDialog(
        title = "Change the priority of annotations", size = "s", easyClose = TRUE,
        footer = list(actionButton("md_edit_rankings", "Save edits"), modalButton("Dismiss")),
        lapply(1:10, function(x) {
          fluidRow(
            column(
              width = 8,
              textInput(paste0("md_rankings_text", x), NULL, value = rankings_merge[x, 1], placeholder = "Empty")
            ),
            column(
              width = 4,
              numericInput(paste0("md_rankings_prio", x), NULL, value = rankings_merge[x, 2], min = 0, max = 10)
            ),
          )
        })
      )
    )
  })
  
  observeEvent(input$md_edit_rankings, {
    sapply(1:10, function(x) {
      rankings_merge[x, 1] <<- toupper(input[[paste0("md_rankings_text", x)]])
      rankings_merge[x, 2] <<- input[[paste0("md_rankings_prio", x)]]
    })
    removeModal()
  })
  
  observeEvent(input$mergeDatasets, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else {
      activeSequence <- rv$sequence[[rv$activeFile]]
      activeDataset <- rv$data[[rv$activeFile]]
      selected <- which(rv$choices %in% input$mergeFile)
      if(is.null(selected)) {
        showNotification("No file selected", type = "error")
      } else {
        sequenceToMerge <- rv$sequence[[selected]]
        datasetToMerge <- rv$data[[selected]]
        
        if(names(rv$data)[rv$activeFile] == names(rv$data)[selected]) {
          showModal(
            modalDialog(
              title = "Do you want to merge a dataset with itself?", size = "m",
              footer = list(actionButton("mergeSameFile", "Yes"), modalButton("Cancel"))
            )
          )
        } else {
          userConfirmation(TRUE)
        }
      }
    }
  })
  
  observeEvent(input$mergeSameFile, {
    userConfirmation(TRUE)
    removeModal()
  })
  
  observeEvent(userConfirmation(), {
    if(userConfirmation()) {
      activeSequence <- rv$sequence[[rv$activeFile]]
      activeDataset <- rv$data[[rv$activeFile]]
      selected <- which(rv$choices %in% input$mergeFile)
      sequenceToMerge <- rv$sequence[[selected]]
      datasetToMerge <- rv$data[[selected]]
      
      print(input$annotation_column_merge)
      
      criteria_column <- input$annotation_column_merge
      
      
      if (sum(activeSequence[, 1] %in% c("Adduct_pos", "Adduct_neg")) != 1 || sum(sequenceToMerge[, 1] %in% c("Adduct_pos", "Adduct_neg")) != 1) {
        sendSweetAlert(session = session, title = "Error", text = "Each dataset must contain exactly one adduct column labeled in the sequence file.", type = "error")
      } else if (ncol(activeDataset) != ncol(datasetToMerge)) {
        sendSweetAlert(session = session, title = "Error", text = "Datasets must have the same number of columns", type = "error")
      } else {
        mergedDatasets <<- mergeDatasets(activeDataset, activeSequence,
                                         datasetToMerge, sequenceToMerge, input$merge_ppm, input$merge_rt)
        clustn <- data.frame(table(mergedDatasets$mergeID))
        dub_clust <- clustn[clustn$Freq > 1, ]
        dub_dat <- mergedDatasets[mergedDatasets$mergeID %in% dub_clust[, 1], ]
        dub_qc <- dub_dat[, activeSequence[, 1] %in% "QC"]
        cov <- cv(dub_qc)
        nclust <- sapply(dub_dat$mergeID, function(x) {
          table(dub_dat$mergeID)[names(table(dub_dat$mergeID)) == x]
        })
        colnames(dub_dat)[activeSequence[, 1] %in% c("Adduct_pos", "Adduct_neg")] <- "adduct"
        
        print(head(clustn)) 
        print(head(dub_dat$mergeID)) # TODO sometimes all NA
        print(head(dub_dat$ionmode)) # TODO sometimes all NA
        print(head(dub_dat$adduct))
        print(head(dub_dat[, which(activeSequence[, 1] %in% "Name")]))
        print(head(dub_dat[, which(activeSequence[, 1] %in% "RT")]))
        print(head(dub_dat[, which(activeSequence[, 1] %in% "Mass")]))
        print(head(cov))
        print(head(dub_dat[, which(rownames(activeSequence) %in% criteria_column)]))
        
        out_dub <- data.frame(
          nClust = nclust,
          Cluster_ID = dub_dat$mergeID,
          Ion_mode = dub_dat$ionmode,
          Adductor = dub_dat$adduct,
          Name = dub_dat[, which(activeSequence[, 1] %in% "Name")],
          RT = dub_dat[, which(activeSequence[, 1] %in% "RT")],
          Mass = dub_dat[, which(activeSequence[, 1] %in% "Mass")],
          CV = cov,
          Criteria = dub_dat[, which(rownames(activeSequence) %in% criteria_column)]
        )
        out_dub <- out_dub[order(out_dub[, 1], out_dub[, 2], decreasing = T), ]
        print(head(out_dub))
        
        md_dup <<- out_dub
        cluster_ends <- which(!duplicated(out_dub[, 2]))
        output$md_modal_dt <- renderDataTable({
          datatable(out_dub,
                    rownames = F,
                    options = list(dom = "t",
                                   autowidth = T,
                                   paging = F),
                    selection = list(selected = finddup(out_dub, rankings_merge))
          ) %>% formatStyle(1:9,
                            `border-top` = styleRow(cluster_ends, "solid 4px"))
        },
        server = T
        )
        userConfirmation(FALSE)
        showModal(
          modalDialog(
            title = "Select features to keep", size = "l",
            p(paste0(length(unique(dub_dat$mergeID))), " duplicate clusters found, of those ", paste0(length(unique(out_dub[out_dub[, 1] > 2, ][, 2]))), " consists of more than 2 features."),
            DTOutput("md_modal_dt"),
            footer = list(actionButton("confirmMerging", "Remove duplicates"), modalButton("Dismiss"))
          )
        )
      }
    }
  })
  
  observeEvent(input$confirmMerging, {
    duplicates <- as.numeric(rownames(md_dup[-input$md_modal_dt_rows_selected, ]))
    merged <<- mergedDatasets[-duplicates, ]
    output$dttable <- renderDataTable(merged, rownames = F, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
    removeModal()
    confirmSweetAlert(session, inputId = "newFileMerge", title = "Merge complete", text = "Save as new file?", btn_labels = c("No", "Yes"), type = "success")
  })
  
  observeEvent(input$newFileMerge, {
    if (isTRUE(input$newFileMerge)) {
      rv$data[[length(rv$data) + 1]] <- merged[, seq(ncol(merged) - 2)]
      rv$sequence[[length(rv$sequence) + 1]] <- rv$sequence[[rv$activeFile]]
      names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_merged")
      initializeVariables()
    } else if (isFALSE(input$newFileMerge)) {
      rv$data[[rv$activeFile]] <- merged[, seq(ncol(merged) - 2)]
      names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_merged")
    }
    rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Positive and negative mode merged: M/z tolerance ppm", input$merge_ppm, "and RT tolerance", input$merge_rt, "\n")
    rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
  })
  
  
  ################################
  # Principal Component Analysis #
  ################################
  #TODO
  observeEvent(input$run_pca1, {
    if (!is.null(rv$activeFile)) { 
      if (input$selectpca1 == "Unsaved data") {
        data <- rv$tmpData       # Set data to the temporary data
        seq <- rv$tmpSequence    # Set sequence to the temporary sequence
      } else { 
        selectchoices <- paste(seq_along(rv$data), ": ", names(rv$data)) # Get the selected dataset
        sd <- which(rv$choices %in% input$selectpca1) # Get the index of the selected dataset
        data <- rv$data[[sd]]    # Set data to the selected dataset
        seq <- rv$sequence[[sd]] # Set sequence to the selected sequence
      }
      
      if ("Sample"  %in% seq[, "labels"]) { # Check if the sequence file contains a "Sample" column
        if (any(seq[, "labels"] %in% "QC")) { # Check if the sequence file contains a "QC" column
          seq[seq[, "labels"] %in% "QC", "group"] <- "QC" # Set the "QC" column to "QC"
        } else {
          cat("No 'QC' labels found in the sequence.\n")
        }
        
        data_subset <- data[seq[, "labels"] %in% c("Sample", "QC")] # Get the data for the samples and QC
        
        if (any(is.na(data[, "Name"]) | data[, "Name"] == "")) {
          sendSweetAlert(session, "Error",
                         "No names in Name column.
                         Make sure features has names ;)",
                         type = "error")
          return()
        }
        
        rownames(data_subset) <- make.unique(as.character(data[, "Name"])) # Make the rownames unique
        
        seq_subset <- seq[seq[, "labels"] %in% c("Sample", "QC"), ] # Get the sequence for the samples and QC
        
        # Perform PCA once and save the results to pca_result
        pca_result <- pcaplot(data_subset, seq_subset, input$pca1_islog)
        
        message("PCA results saved.")
        # Generate a unique name for the PCA result based on the dataset name
        if (input$selectpca1 == "Unsaved data") {
          dataset_name <- "UnsavedData"  # or any other name you prefer for unsaved data
        } else {
          dataset_name <- names(rv$data)[sd]
        }
        pca_name <- paste0(dataset_name, "_pca")
        pc_name <- paste0(dataset_name, "_PC")
        
        # Check if the PCA name already exists in rv$pca_results
        if (!(pca_name %in% names(rv$pca_results))) {
          # If the name does not exist, save the PCA and PC results
          rv$pca_results[[pca_name]] <- list(pca_df = pca_result$pca_df,
                                             PC_df = pca_result$PC_df)
        }
        
        output$plotpca1 <- renderPlotly({
          pca_result$pca_plotly
        })
        
        output$plotscree1 <- renderPlotly({
          pca_result$scree_plotly
        })
        
        message(sample(quotes, 1))
        
        if (sum(seq[, 1] %in% "QC") > 0) {
          qccv <- paste0("CV in QC samples: ", round(cvmean(data[seq[, 1] %in% "QC"]), 2), "</br>")
        } else {
          qccv <- "No QC in dataset </br>"
        }
        sclass <- seq[seq[, "labels"] %in% c("Sample", "QC"), ][, "group"] # Get the class of the samples and QC
        sclass <- sclass[sclass != "QC"]
        if (sum(!is.na(sclass)) > 0) {
          classcv <- sapply(sort(unique(sclass)), function(x) {
            round(cvmean(data_subset[, sclass %in% x]), 2)
          })
          classcv <- sapply(seq_along(classcv), function(x) {
            paste0("CV in group ", sort(unique(sclass))[x], ": ", classcv[x], "</br>")
          })
        } else {
          classcv <- NULL
        }
        text <- c(qccv, classcv)
        output$pca1Details <- renderUI({
          HTML(text)
        })
      }
    }
  })
  observeEvent(input$run_pca2, {
    selectchoices <- paste(seq_along(rv$data), ": ", names(rv$data))
    sd <- which(rv$choices %in% input$selectpca2)
    if ("Sample" %in% rv$sequence[[sd]][, 1]) {
      data <- rv$data[[sd]]
      seq <- rv$sequence[[sd]]
      shinyCatch(
        seq[seq[, 1] %in% "QC", ][, 4] <- "QC",
        blocking_level = 'message',
        shiny = FALSE
      )
      
      data_subset <- data[seq[, "labels"] %in% c("Sample", "QC")] # Get the data for the samples and QC
      rownames(data_subset) <- make.unique(as.character(data[, "Name"]))
      
      seq_subset <- seq[seq[, "labels"] %in% c("Sample", "QC"), ] # Get the sequence for the samples and QC
      
      # Save the PCA results to pca_results
      pca_result <- pcaplot(data_subset, seq_subset, input$pca2_islog)
      
      message("PCA results saved.")
      # Generate a unique name for the PCA result based on the dataset name
      if (input$selectpca1 == "Unsaved data") {
        dataset_name <- "UnsavedData"  # or any other name you prefer for unsaved data
      } else {
        dataset_name <- names(rv$data)[sd]
      }
      pca_name <- paste0(dataset_name, "_pca")
      pc_name <- paste0(dataset_name, "_PC")
      
      # Check if the PCA name already exists in rv$pca_results
      if (!(pca_name %in% names(rv$pca_results))) {
        # If the name does not exist, save the PCA and PC results
        pca_result <- pcaplot(data_subset, seq_subset, input$pca2_islog)  # Perform PCA
        
        # Save the PCA and PC results as a named list for each PCA result
        rv$pca_results[[pca_name]] <- list(pca_df = pca_result$pca_df,
                                           PC_df = pca_result$PC_df)
      }
      
      # Debugging to show that rv$results is updated
      # cat("PCA results saved as:", pca_name, "\n")
      # cat("PCA results dimensions:", dim(rv$pca_results[[pca_name]]), "\n")
      # print(str(rv$pca_results[[pca_name]]))
      
      output$plotpca2 <- renderPlotly({
        pca_result$pca_plotly
      })
      
      output$plotscree2 <- renderPlotly({
        pca_result$scree_plotly
      })
      
      if (sum(seq$labels %in% "QC") > 0) {
        qccv <- paste0("CV in QC samples: ", round(cvmean(data[seq[, 1] %in% "QC"]), 2), "</br>")
      } else {
        qccv <- "No QC in dataset </br>"
      }
      sclass <- seq[seq[, 1] %in% c("Sample", "QC"), ][, 4]
      sclass <- sclass[sclass != "QC"]
      if (sum(!is.na(sclass)) > 0) {
        classcv <- sapply(sort(unique(sclass)), function(x) {
          round(cvmean(data_subset[sclass %in% x]), 2)
        })
        classcv <- sapply(seq_along(classcv), function(x) {
          paste0("CV in group ", sort(unique(sclass))[x], ": ", classcv[x], "</br>")
        })
      } else {
        classcv <- NULL
      }
      text <- c(qccv, classcv)
      output$pca2Details <- renderUI({
        HTML(text)
      })
    }
  })
  
  observeEvent(input$select_boxplot_1, { #TODO which rv choices -> function
    data <- rv$data[[which(rv$choices %in% input$select_boxplot_1)]]
    sequence <- rv$sequence[[which(rv$choices %in% input$select_boxplot_1)]]
    group <- input$select_boxplot_1_group
    data <- data[sequence[, 1] %in% "Sample" & sequence[, 4] %in% group]
    output$boxplot_1 <- renderPlot({
      boxplot(log2(data), main = input$select_boxplot_1, xlab = "Analyte", ylab = "Intensity")
    })
  }, ignoreInit = TRUE)
  
  observeEvent(input$drift_1, {
    rv$drift_plot_select <- 1
  })
  observeEvent(input$drift_2, {
    rv$drift_plot_select <- 2
  })
  observeEvent(input$drift_3, {
    rv$drift_plot_select <- 3
  })
  
  output$drift_ui <- renderUI({
    box(
      width = NULL,
      if (is.null(rv$activeFile)) {
        p("No data")
      } else if (input$drift_select != "None" && nrow(rv$data[[rv$activeFile]]) != nrow(rv$data[[which(rv$choices %in% input$drift_select)]])) {
        p("Not able to compare the selected datasets")
      } else if (input$drift_select == "None" && rv$drift_plot_select == 2) {
        p("Need dataset to compare with")
      } else if (is.null(input$dt_drift_panel_rows_selected) && rv$drift_plot_select == 1) {
        p("Select feature to plot")
      } else if (rv$drift_plot_select == 1) {
        lapply(seq_along(input$dt_drift_panel_rows_selected), function(i) {
          fluidRow(
            column(6, plotOutput(paste0("driftplotoutput", i), height = 280, width = "100%")),
            column(6, plotOutput(paste0("driftplotoutput2", i), height = 280, width = "100%"))
          )
        })
      } else if (rv$drift_plot_select == 2) {
        fluidRow(column(12, plotOutput("cvscatterplot", height = 600, width = "100%")))
      } else {
        p("Nothing here yet")
      }
    )
  })
  output$boxplot_ui <- renderUI({
    box(
      width = NULL,
      if (is.null(rv$activeFile)) {
        p("No data")
      } else if (is.null(input$dt_boxplot_panel_rows_selected)) {
        p("Select feature to plot")
      } else {
        lapply(seq_along(input$dt_boxplot_panel_rows_selected), function(i) {
          fluidRow(column(12, plotOutput(paste0("boxplotoutput", i), height = 280, width = "100%")))
        })
      }
    )
  })
  
  observe({
    if (length(input$dt_drift_panel_rows_selected) == 0 && rv$drift_plot_select == 1) {
      output$driftplotoutput1 <- renderPlot({
        NULL
      })
      output$driftplotoutput21 <- renderPlot({
        NULL
      })
    } else if (rv$drift_plot_select == 1) {
      for (i in 1:(length(input$dt_drift_panel_rows_selected) + 1)) {
        output[[paste0("driftplotoutput", i)]] <- renderPlot({
          NULL
        })
        output[[paste0("driftplotoutput2", i)]] <- renderPlot({
          NULL
        })
      }
      for (i in seq_along(input$dt_drift_panel_rows_selected)) {
        local({
          my_i <- i
          output[[paste0("driftplotoutput", my_i)]] <- renderPlot({
            driftplot(
              data = rv$data[[rv$activeFile]][input$dt_drift_panel_rows_selected[my_i], ],
              seq = rv$sequence[[rv$activeFile]]
            )
          })
        })
        if (input$drift_select != "None") {
          local({
            my_i <- i
            output[[paste0("driftplotoutput2", my_i)]] <- renderPlot({
              driftplot(
                data = rv$data[[which(rv$choices %in% input$drift_select)]][input$dt_drift_panel_rows_selected[my_i], ],
                seq = rv$sequence[[rv$activeFile]]
              )
            })
          })
        }
      }
    } else if (rv$drift_plot_select == 2) {
      output$cvscatterplot <- renderPlot({
        cvscatterplot(
          data = rv$data[[rv$activeFile]],
          data2 = rv$data[[which(rv$choices %in% input$drift_select)]],
          seq = rv$sequence[[rv$activeFile]],
          name1 = names(rv$data)[rv$activeFile],
          name2 = names(rv$data)[which(rv$choices %in% input$drift_select)]
        )
      })
    }
  })
  observe({
    if (length(input$dt_boxplot_panel_rows_selected > 0)) {
      for (i in seq_along(input$dt_boxplot_panel_rows_selected)) {
        local({
          my_i <- i
          output[[paste0("boxplotoutput", my_i)]] <- renderPlot({
            myboxplot(
              data = rv$data[[rv$activeFile]][input$dt_boxplot_panel_rows_selected[my_i], ],
              seq = rv$sequence[[rv$activeFile]],
              log = input$bloxplot_log,
              ylog = input$bloxplot_ylog
            )
          })
        })
      }
    }
  })
  
  #####################
  # Outlier Detection #
  #####################
  
  # PCA results update 
  # Whenever pca_results are updated, update the selectInput choices for outlier detection
  observe({
    req(rv$pca_results)  # Ensure rv$pca_results is not NULL
    
    input_ids <- c("kmeans_pca", "hierarchical_pca", "dbscan_pca",
                   "hdbscan_pca", "optics_pca", "lof_pca")
    
    choices <- names(rv$pca_results)
    
    # If choices are NULL or empty, provide a default message
    if (is.null(choices) || length(choices) == 0) {
      choices <- list("No PCA results available" = "")
    }
    
    lapply(input_ids, function(input_id) {
      updateSelectInput(session, input_id, choices = choices)
    })
  })
  
  # Function to generate selectInput UI for a given input ID
  create_group_selection_ui <- function(input_id) {
    renderUI({
      if (is.null(rv$activeFile)) {
        showNotification("No data", type = "error")
        return(NULL)
      }
      
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]
      seq <- sequence[sequence$labels %in% c("Sample", "QC"), ]
      data <- data[, rownames(seq), drop = FALSE]
      
      if (input[[paste0("select_groups_", input_id)]]) {  # Check if the checkbox is selected
        selectInput(
          inputId = paste0("selected_groups_", input_id),  # Unique inputId per method
          label = paste("Select Groups for", input_id),
          choices = seq$group, 
          selected = seq$group[1],  # Default to first group
          multiple = TRUE, 
          width = "100%"
        )
      }
    })
  }
  
  # Create separate UI outputs for each tab
  output$group_selection_ui_kmeans <- create_group_selection_ui("kmeans")
  output$group_selection_ui_hierarchical <- create_group_selection_ui("hierarchical")
  output$group_selection_ui_dbscan <- create_group_selection_ui("dbscan")
  output$group_selection_ui_hdbscan <- create_group_selection_ui("hdbscan")
  output$group_selection_ui_optics <- create_group_selection_ui("optics")
  output$group_selection_ui_lof <- create_group_selection_ui("lof")
  
  # kmeans
  observeEvent(input$compute_kmeans_eval, {
    # Fetch the PCA data from the reactive values
    pca_data <- rv$pca_results[[input$kmeans_pca]]  # Fetch the selected PCA result (list)
    
    # Check if PCA data is available and extract it
    if (!is.null(pca_data)) {
      pca_df <- pca_data[[1]]  # Extract the pca_df directly from the list
      
      # Get the evaluation method selected by the user
      method <- input$kmeans_eval_method
      
      # Check that the method is valid
      if (!is.null(method)) {
        # Use the selected evaluation method to create the plot
        eval_plot <- switch(method,
                            "silhouette" = fviz_nbclust(pca_df[, c("PC1", "PC2")], kmeans, method = "silhouette", k.max = nrow(pca_df) - 1) + 
                              scale_x_discrete(breaks = seq(1, nrow(pca_df) - 1, by = 5)) +  # Reduce number of x-axis ticks
                              labs(title = "Optimal number of clusters (Silhouette Method)") +
                              theme_bw(),
                            "wss" = fviz_nbclust(pca_df[, c("PC1", "PC2")], kmeans, method = "wss", k.max = nrow(pca_df) - 1) +
                              scale_x_discrete(breaks = seq(1, nrow(pca_df) - 1, by = 5)) +  # Reduce number of x-axis ticks
                              labs(title = "Optimal number of clusters (Elbow Method)") +
                              theme_bw(),
                            "gap_stat" = fviz_nbclust(pca_df[, c("PC1", "PC2")], kmeans, method = "gap_stat", k.max = nrow(pca_df) - 1) +
                              scale_x_discrete(breaks = seq(1, nrow(pca_df) - 1, by = 5)) +  # Reduce number of x-axis ticks
                              labs(title = "Optimal number of clusters (Gap Statistic Method)") +
                              theme_bw())
        
        # Render the evaluation plot in plotly
        output$kmeans_eval_plot <- renderPlotly(ggplotly(eval_plot))
      }
    }
  })
  observeEvent(input$run_kmeans, {
    # Fetch the selected PCA data
    pca_data <- rv$pca_results[[input$kmeans_pca]]  # Fetch the selected PCA result (list)
    
    if (!is.null(pca_data)) {
      pca_df <- pca_data[[1]]   # Extract pca_df from the list
      PC_df <- pca_data[[2]]    # Extract PC_df from the list
      k <- input$num_clusters   # Get the number of clusters (k)
      percentile_threshold <- input$percentile_threshold  # Get the percentile threshold
      
      # in pca_df remove rows with QC in column group 
      pca_df <- pca_df[pca_df$group != "QC", ]
      
      print(pca_df)
      print(PC_df)
      pca_df$group <- as.character(pca_df$group)
      
      if (input$select_groups_kmeans) {
        message(paste0("Enable grouping for groups: ", input$select_groups_kmeans))
        if (is.null(input$selected_groups_kmeans) || length(input$selected_groups_kmeans) < 1) {
          showNotification("Select at least one groups for the heatmap.", type = "error")
          return()  # Stop execution
        }
        # Filter seq_subset and data_subset by selected groups
        selected_groups_kmeans <- input$selected_groups_kmeans
        message(paste0("Selected groups: ", paste(selected_groups_kmeans, collapse = ", ")))
        pca_df <- pca_df[pca_df$group %in% selected_groups_kmeans, ]
      }
      
      pca_df$group <- as.factor(pca_df$group)
      
      print(head(pca_df))
      print(head(PC_df))

      # Check if pca_df is a valid data frame
      if (!is.null(pca_df) && is.data.frame(pca_df)) {
        # Call the kmeans_clustering function to get the results
        kmeans_results <- kmeans_clustering(pca_df, k, percentile_threshold, PC_df)
        
        # Generate a unique name for the K-means result from the selected PCA result name
        pca_result_name <- input$kmeans_pca   # This gives the selected name from the dropdown
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # Timestamp for uniqueness (date + time)
        kmeans_name <- paste0(pca_result_name, "_Kmeans_", timestamp)
        
        # Save the K-means result (including outlier information) into the reactive value
        rv$outlier_df[[kmeans_name]] <- list(kmeans_df = kmeans_results)
        
        # Render the K-means plot in the UI
        output$kmeans_plot <- renderPlotly({
          kmeans_results$kmeans_plotly  # Pass the interactive plotly object to renderPlotly
        })
        
        # Render the K-means outlier table in the UI
        output$kmeans_outliers <- renderDT({
          datatable(kmeans_results$kmeans_df, options = list(pageLength = 20, autoWidth = TRUE))
        })
        
      } else {
        cat("Selected PCA result is not a valid data frame.\n")
      }
    } else {
      cat("No PCA results selected or available.\n")
    }
  })
  # Hierarchical Clustering 
  observeEvent(input$run_hierarchical, {
    # Fetch the selected PCA data from reactive values
    pca_data <- rv$pca_results[[input$hierarchical_pca]]  # Fetch the selected PCA result (list)
    sequence <- rv$sequence[[rv$activeFile]]  # Fetch the sequence data from the active file
    
    seq_subset <- sequence[sequence[, "labels"] %in% c("Sample", "QC"), ]  # Get the sequence for the samples and QC
    
    # Debugging to show the sequence data
    if (!"labels" %in% colnames(sequence)) {
      cat("Error: 'labels' column not found in sequence data.\n")
      return()
    }
    str(seq_subset)
    
    if (!is.null(pca_data)) {
      pca_df <- pca_data[[1]]   # Extract pca_df from the list
      PC_df <- pca_data[[2]]    # Extract PC_df from the list
      
      # in pca_df remove rows with QC in column group 
      pca_df <- pca_df[pca_df$group != "QC", ]
      
      # Retrieve parameters from the UI
      method <- input$clustering_method   # Clustering method selected
      k <- input$num_clusters_hierarchical  # Number of clusters
      threshold <- input$threshold  # Dendrogram threshold
      
      pca_df$group <- as.character(pca_df$group)
      
      if (input$select_groups_hierarchical) {
        message(paste0("Enable grouping for groups: ", input$select_groups_hierarchical))
        if (is.null(input$selected_groups_hierarchical) || length(input$selected_groups_hierarchical) < 1) {
          showNotification("Select at least one groups for the hierarchical.", type = "error")
          return()  # Stop execution
        }
        # Filter seq_subset and data_subset by selected groups
        selected_groups_hierarchical <- input$selected_groups_hierarchical
        message(paste0("Selected groups: ", paste(selected_groups_hierarchical, collapse = ", ")))
        pca_df <- pca_df[pca_df$group %in% selected_groups_hierarchical, ]
      }
      
      pca_df$group <- as.factor(pca_df$group)
      
      # Debugging to show the parameters 
      # cat("Clustering method: ", method, "\n")
      # cat("Number of clusters: ", k, "\n")
      # cat("Dendrogram threshold: ", threshold, "\n")
      # print(head(seq_subset))
      
      # Perform the hierarchical clustering
      hierarchical_results <- perform_hierarchical_clustering(pca_df, seq_subset, method, k, threshold)
      
      # Render the hierarchical clustering plot
      output$hclust_plot <- renderPlotly({
        hierarchical_results$hclust_plot  # Pass the plotly object to renderPlotly
      })
      
      # conf_matrix_plot <- perform_confusion_matrix(pca_df, seq_subset, method, k)
      
      # Render the confusion matrix plot
      # output$conf_matrix_plot <- renderPlotly({
      #   conf_matrix_plot  # Pass the plotly object to renderPlotly
      # })
      
      # Render the dendrogram plot
      output$dendrogram_plot <- renderPlotly({
        hierarchical_results$dendrogram_plot  # Pass the plotly dendrogram to renderPlotly
      })
      
      # Render the outliers table
      output$hierarchical_outliers <- renderDT({
        datatable(hierarchical_results$hierarchical_outliers, options = list(pageLength = 20, autoWidth = TRUE))
      })
      
    } else {
      cat("No PCA results selected or available.\n")
    }
  })
  # Run DBSCAN
  # DBSCAN Clustering
  observeEvent(input$compute_knn, {
    req(rv$pca_results[[input$dbscan_pca]])  # Ensure PCA data is loaded
    pca_data <- rv$pca_results[[input$dbscan_pca]]  # Fetch the selected PCA result (list)
    pca_df <- pca_data[[1]]  # Extract pca_df from the list
    k <- input$knn
    
    # in pca_df remove rows with QC in column group 
    pca_df <- pca_df[pca_df$group != "QC", ]
    
    
    # Debug print
    # print(paste("Computing kNN distance plot with k =", k))
    
    output$knn_plot <- renderPlotly({
      perform_kNN_dist_plot(pca_df, k)
    })
  })
  observeEvent(input$run_dbscan, {
    req(rv$pca_results[[input$dbscan_pca]])  # Ensure PCA data is loaded
    pca_data <- rv$pca_results[[input$dbscan_pca]]  # Fetch the selected PCA result (list)
    pca_df <- pca_data[[1]]  # Extract pca_df from the list
    
    # in pca_df remove rows with QC in column group 
    pca_df <- pca_df[pca_df$group != "QC", ]
    
    pca_df$group <- as.character(pca_df$group)
    
    if (input$select_groups_hdbscan) {
      message(paste0("Enable grouping for groups: ", input$select_groups_hdbscan))
      if (is.null(input$selected_groups_hdbscan) || length(input$selected_groups_hdbscan) < 1) {
        showNotification("Select at least one groups for the hdbscan.", type = "error")
        return()  # Stop execution
      }
      # Filter seq_subset and data_subset by selected groups
      selected_groups_hdbscan <- input$selected_groups_hdbscan
      message(paste0("Selected groups: ", paste(selected_groups_hdbscan, collapse = ", ")))
      pca_df <- pca_df[pca_df$group %in% selected_groups_hdbscan, ]
    }
    
    pca_df$group <- as.factor(pca_df$group)

    eps <- input$eps
    min_pts <- input$min_pts_dbscan
    
    # Debugging print
    # print(paste("Running DBSCAN with eps =", eps, "and minPts =", min_pts))
    
    dbscan_res <- perform_dbscan_clustering(pca_df, eps, min_pts)
    
    dbscan_res$dbscan_outliers <- dbscan_res$dbscan_outliers %>%
      rename(Category = Outlier)
    
    output$dbscan_plot <- renderPlotly({
      dbscan_res$dbscan_plot
    })
    # Debugging 
    # print(head(dbscan_res$dbscan_outliers))
    # print(names(dbscan_res$dbscan_outliers))
    
    output$dbscan_outliers <- renderDT({
      datatable(dbscan_res$dbscan_outliers %>%
                  select(Sample, PC1, PC2, Cluster, Category), options = list(pageLength = 10))
    })
  })
  # HDBSCAN Clustering
  observeEvent(input$run_hdbscan, {
    # Ensure PCA data is loaded
    req(rv$pca_results[[input$hdbscan_pca]])
    
    # Fetch the selected PCA result
    pca_data <- rv$pca_results[[input$hdbscan_pca]]
    pca_df <- pca_data[[1]]  # Extract pca_df from the list
    
    # in pca_df remove rows with QC in column group 
    pca_df <- pca_df[pca_df$group != "QC", ]
    
    min_pts <- input$min_pts_hdbscan  # Minimum number of points for HDBSCAN
    threshold <- input$threshold_hdbscan  # Outlier threshold
    
    pca_df$group <- as.character(pca_df$group)
    
    if (input$select_groups_hdbscan) {
      message(paste0("Enable grouping for groups: ", input$select_groups_hdbscan))
      if (is.null(input$selected_groups_hdbscan) || length(input$selected_groups_hdbscan) < 1) {
        showNotification("Select at least one groups for the hdbscan.", type = "error")
        return()  # Stop execution
      }
      # Filter seq_subset and data_subset by selected groups
      selected_groups_hdbscan <- input$selected_groups_hdbscan
      message(paste0("Selected groups: ", paste(selected_groups_hdbscan, collapse = ", ")))
      pca_df <- pca_df[pca_df$group %in% selected_groups_hdbscan, ]
    }
    
    pca_df$group <- as.factor(pca_df$group)
    
    # Debug print
    # print(paste("Running HDBSCAN with minPts =", min_pts))
    
    # Perform HDBSCAN clustering
    hdbscan_res <- perform_hdbscan_clustering(pca_df, min_pts)
    
    # Process outliers from the HDBSCAN results
    hdbscan_outliers <- hdbscan_res$hdbscan_outliers
    
    # Add categorization based on OutlierScore and threshold
    if (nrow(hdbscan_outliers) > 0 && "OutlierScore" %in% names(hdbscan_outliers)) {
      hdbscan_outliers <- hdbscan_outliers %>%
        mutate(Category = ifelse(OutlierScore > threshold, "Outlier", "Inlier"))
    } else {
      showNotification("HDBSCAN results are empty or do not contain 'OutlierScore' column.", type = "error")
    }
    
    # Render HDBSCAN plot
    output$hdbscan_plot <- renderPlotly({
      hdbscan_res$hdbscan_plot
    })
    
    # Render outlier table
    output$hdbscan_outliers <- renderDT({
      datatable(hdbscan_outliers %>%
                  select(Sample, PC1, PC2, Cluster, Category), options = list(pageLength = 10))
    })
  })
  # OPTICS Clustering
  observeEvent(input$run_optics, {
    req(rv$pca_results[[input$optics_pca]])
    pca_data <- rv$pca_results[[input$optics_pca]]
    pca_df <- pca_data[[1]]  # Extract pca_df from the list
    
    # in pca_df remove rows with QC in column group 
    pca_df <- pca_df[pca_df$group != "QC", ]
    
    # Debugging print
    min_pts <- input$min_pts_optics
    eps <- if (is.na(input$eps_optics)) NULL else input$eps_optics
    eps_cl <- input$eps_cl_optics
    
    pca_df$group <- as.character(pca_df$group)
    
    if (input$select_groups_optics) {
      message(paste0("Enable grouping for groups: ", input$select_groups_optics))
      if (is.null(input$selected_groups_optics) || length(input$selected_groups_optics) < 1) {
        showNotification("Select at least one groups for the optics.", type = "error")
        return()  # Stop execution
      }
      # Filter seq_subset and data_subset by selected groups
      selected_groups_optics <- input$selected_groups_optics
      message(paste0("Selected groups: ", paste(selected_groups_optics, collapse = ", ")))
      pca_df <- pca_df[pca_df$group %in% selected_groups_optics, ]
    }
    
    pca_df$group <- as.factor(pca_df$group)
    
    # print(paste("Running OPTICS with minPts =", min_pts, ", eps =", eps, ", and eps_cl =", eps_cl))
    
    optics_res <- perform_optics_analysis(pca_df, eps, min_pts, eps_cl)
    
    optics_outliers <- optics$optics_outliers
    
    output$optics_reachability_plot <- renderPlot({
      optics_res$reachability_plot()
    })
    
    output$reachability_plot_threshold <- renderPlot({
      optics_res$reachability_plot_threshold()
    })
    
    output$cluster_plot <- renderPlot({
      optics_res$cluster_plot()
    })
    
    output$optics_outliers <- renderTable({
      optics_res$optics_outliers
    })
  })
  # LOF Clustering
  observeEvent(input$run_lof, {
    req(rv$pca_results[[input$optics_pca]])
    pca_data <- rv$pca_results[[input$optics_pca]]
    pca_df <- pca_data[[1]]  # Extract pca_df from the list
    
    # in pca_df remove rows with QC in column group 
    pca_df <- pca_df[pca_df$group != "QC", ]
    
    threshold <- input$lof_threshold
    min_pts <- input$lof_k
    
    pca_df$group <- as.character(pca_df$group)
    
    if (input$select_groups_lof) {
      message(paste0("Enable grouping for groups: ", input$select_groups_lof))
      if (is.null(input$selected_groups_lof) || length(input$selected_groups_lof) < 1) {
        showNotification("Select at least one groups for the lof. ", type = "error")
        return()  # Stop execution
      }
      # Filter seq_subset and data_subset by selected groups
      selected_groups_lof <- input$selected_groups_lof
      message(paste0("Selected groups: ", paste(selected_groups_lof, collapse = ", ")))
      pca_df <- pca_df[pca_df$group %in% selected_groups_lof, ]
    }
    
    pca_df$group <- as.factor(pca_df$group)
    
    lof_res <- calculate_and_plot_lof(pca_df, threshold = threshold, minPts = min_pts)
    
    lof_plot <- lof_res$lof_plotly
    lof_od_plot <- lof_res$lof_od_plotly
    lof_outliers <- lof_res$lof_outliers
    
    output$lof_plot <- renderPlotly({
      lof_res$lof_plotly
    })
    
    output$lof_od_plot <- renderPlotly({
      lof_res$lof_od_plotly
    })
    
    output$lof_outliers <- renderTable({
      lof_res$lof_outliers
    })
  })
  
  ###########
  # Heatmap #
  ###########
  # Generate Heatmap
  output$group_selection_ui_heatmap <- renderUI({
    
    if (!is.null(rv$activeFile)) {
      if (input$select_heatmap_data == "Unsaved data") {
        data <- rv$tmpData  # Use the temporary data
        seq <- rv$tmpSequence  # Use the temporary sequence
      } else {
        # Get the index of the selected dataset
        sd <- which(rv$choices %in% input$select_heatmap_data)
        data <- rv$data[[sd]]  # Retrieve the selected dataset
        seq <- rv$sequence[[sd]]  # Retrieve the selected sequence
      }
      
      if (input$select_groups_heatmap) {  # Only render if the checkbox is checked
        selectInput(
          "selected_groups_heatmap", 
          "Select Groups:", 
          choices = seq$group, 
          selected = seq$group[1],  # Default to the first group
          multiple = TRUE,          # Allow multiple selections
          width = "100%"
        )
      }
    }
  })
  output$grouping_column_ui <- renderUI({
    if (!is.null(rv$activeFile)) {
      if (input$select_heatmap_data == "Unsaved data") {
        data <- rv$tmpData  # Use the temporary data
        seq <- rv$tmpSequence  # Use the temporary sequence
      } else {
        # Get the index of the selected dataset
        sd <- which(rv$choices %in% input$select_heatmap_data)
        data <- rv$data[[sd]]  # Retrieve the selected dataset
        seq <- rv$sequence[[sd]]  # Retrieve the selected sequence
      }
      
      # seq <- seq[!seq[, "labels"] %in% c("Sample", "QC"), ]  # Restrict rows to "Sample" and "QC"
      # data <- data[, rownames(seq), drop = FALSE]  # Use row names of seq to filter columns
      
      columns <- colnames(data)
      
      if ("super_class" %in% columns) {
        default_column <- "super_class"
      } else {
        default_column <- columns[1]
      }
      
      if (input$enable_grouping_heatmap) {  # Only render if the checkbox is checked
        selectInput(
          inputId = "group_column_heatmap",
          label = "Select grouping column",
          choices = columns,
          selected = default_column, 
          width = "100%"
        )
      }
    }
  })
  
  # Define the reactive value at the top of the server so it persists
  savedDatasetNameHeatmap <- reactiveVal("My Heatmap")
  # Observe the input for the heatmap title and update the reactive value
  observe({
    savedDatasetNameHeatmap(input$heatmap_title)
    
    output$displayName <- renderText({
      paste("Current Heatmap Title:", savedDatasetNameHeatmap())
    })
    
  })
  
  observeEvent({input$select_heatmap_data}, {
    if (!is.null(rv$activeFile)) {
      if (input$select_heatmap_data == "Unsaved data") {
        data <- rv$tmpData  # Use the temporary data
        seq <- rv$tmpSequence  # Use the temporary sequence
      } else {
        # Get the index of the selected dataset
        sd <- which(rv$choices %in% input$select_heatmap_data)
        data <- rv$data[[sd]]  # Retrieve the selected dataset
        seq <- rv$sequence[[sd]]  # Retrieve the selected sequence
      }
      
      # TODO crazes the example file
      # seq <- seq[!seq[, "labels"] %in% c("Sample", "QC"), ] 
      # data_sub <- data[, rownames(seq), drop = FALSE]  # Use row names of seq to filter columns
      
      # # Extract column names from the selected dataset
      data_colnames <- colnames(data) # Substitude with data_sub
      columns <- c("heatmap_labels")
      for (column in columns) {
        # Update the 'identifier_column' select input with the new choices
        updateSelectInput(session, column, choices = data_colnames)
      }
    }
  })
  
  observeEvent(input$run_heatmap, {
    # Ensure a dataset is selected
    req(input$select_heatmap_data,
        input$heatmap_labels)
    
    if (!is.null(rv$activeFile)) {
      if (input$select_heatmap_data == "Unsaved data") {
        data <- rv$tmpData  # Use the temporary data
        seq <- rv$tmpSequence  # Use the temporary sequence
      } else {
        # Get the index of the selected dataset
        sd <- which(rv$choices %in% input$select_heatmap_data)
        data <- rv$data[[sd]]  # Retrieve the selected dataset
        seq <- rv$sequence[[sd]]  # Retrieve the selected sequence
        # dataset_name <- names(rv$data)[sd]  # Retrieve dataset name
      }
      
      # Subset data for "Sample" labels
      seq_subset <- seq[seq[, "labels"] %in% c("Sample", 2), ]  # Restrict to "Sample" rows
      data_subset <- data[, rownames(seq_subset), drop = FALSE]  # Use row names of seq_subset to filter columns
      
      # Check group selection
      if (input$select_groups_heatmap) {
        if (is.null(input$selected_groups_heatmap) || length(input$selected_groups_heatmap) < 2) {
          showNotification("Please select at least two groups for the heatmap.", type = "error")
          return()  # Stop execution
        }
        # Filter seq_subset and data_subset by selected groups
        selected_groups_heatmap <- input$selected_groups_heatmap
        seq_subset <- seq_subset[seq_subset$group %in% selected_groups_heatmap, ]
        data_subset <- data[, rownames(seq_subset), drop = FALSE]  # Subset columns by rownames of seq_subset
      }
      
      
      enable_groups <- input$enable_grouping_heatmap
      groups <- input$group_column_heatmap
      show_column_names <- input$show_column_names
      show_row_names <- input$show_row_names
      cluster_rows <- input$cluster_rows
      show_row_dend <- input$show_row_dend
      labels <- input$heatmap_labels
      clustering_distance_rows <- input$clustering_distance_rows
      clustering_method_rows <- input$clustering_method_rows
      islog <- input$heatmap_islog
      
      message(paste0("Heatmap labels column: ", labels))
      
      message(paste0("Enable grouping: ", enable_groups))
      if (enable_groups) {
        message(paste0("Grouping column selected: ", groups))
      }
      
      
      selected_labels <- as.character(data[[labels]])
      fallback <- if ("Name" %in% colnames(data)) {
        as.character(data[["Name"]])
      } else if ("name" %in% colnames(data)) {
        as.character(data[["name"]])
      } else {
        NULL
      }
      if (is.null(fallback)) {
        showNotification("No fallback column ('Name' or 'name') available.", type = "error")
        return()
      }
      missing <- is.na(selected_labels) | selected_labels == ""
      selected_labels[missing] <- fallback[missing]
      rownames(data_subset) <- make.unique(selected_labels)
      rownames(data) <- make.unique(selected_labels) 
      
      TOP_X <- as.numeric(input$top_x)
      if (is.na(TOP_X) || TOP_X < 1) {
        showNotification("'Number of Top Features' must be a positive integer", type = "error")
        return()
      }
      
      if (TOP_X > nrow(data) ) {
        showNotification(paste0("'Number of Top Features' must be less than or equal to ", nrow(data)), type = "error")
        return()
      }
      
      # Generate the heatmap
      result <- plot_heatmap(data_subset, data, seq_subset, TOP_X, savedDatasetNameHeatmap(),
                             clustering_distance_rows, clustering_method_rows, 
                             show_column_names, show_row_names, cluster_rows,
                             show_row_dend, labels, enable_groups, groups, islog)
      
      heatmap_plot <- result$heatmap
      top_stats <- result$top_stats

      # Render the heatmap
      #output$heatmap_plot <- renderPlot({
      #  if (!is.null(heatmap_plot)) {
      #    draw(heatmap_plot)
      #  }
      #}, height = heatmap_height)

      ht1 <- draw(heatmap_plot)
      makeInteractiveComplexHeatmap(input, output, session, ht1, "heatmap_interactive")
      
      # Render the table of top features
      output$heatmap_table <- DT::renderDataTable({
        DT::datatable(top_stats, options = list(pageLength = 20))
      })
      
      
      message(sample(quotes, 1))
      
    }
  })
  
  ###########
  # Volcano #
  ###########
  # Generate Volcano Plot
  
  observeEvent(input$select_volcano_data, {
    req(input$select_volcano_data) # Ensure a dataset is selected
    
    if (!is.null(rv$activeFile)) {
      if (input$select_volcano_data == "Unsaved data") {
        data <- rv$tmpData  # Use the temporary data
      } else {
        # Get the index of the selected dataset
        sd <- which(rv$choices %in% input$select_volcano_data)
        data <- rv$data[[sd]]  # Retrieve the selected dataset
      }
      
      # Extract column names from the selected dataset
      data_colnames <- colnames(data)
      
      columns <- c("volcano_labels")
      
      for (column in columns) {
        # Update the 'identifier_column' select input with the new choices
        updateSelectInput(session, column, choices = data_colnames)
      }
    }
  })
  # Define the reactive value at the top of the server 
  savedDatasetNameVolcano <- reactiveVal("Volcano Plot: ")
  # Observe the input for the volcano title and update the reactive value
  observe({
    savedDatasetNameVolcano(input$volcano_title)
    
    output$displayName <- renderText({
      paste("Current Heatmap Title:", savedDatasetNameVolcano())
    })
  })
  
  output$feature_selection_ui_volcano <- renderUI({
    if (!input$enable_feature_selection) return(NULL)  # Only show if checkbox is checked
    req(input$select_volcano_data)  # Ensure a dataset is selected
    
    if (!is.null(rv$activeFile)) {
      if (input$select_volcano_data == "Unsaved data") {
        data <- rv$tmpData  # Use the temporary data
        seq <- rv$tmpSequence  # Use the temporary sequence
      } else {
        # Get the index of the selected dataset
        sd <- which(rv$choices %in% input$select_volcano_data)
        data <- rv$data[[sd]]  # Retrieve the selected dataset
        seq <- rv$sequence[[sd]]  # Retrieve the selected sequence
      }
      
      # Filter sequence to include only "Sample" rows
      seq <- seq[!seq[, "labels"] %in% c("Sample", "QC"), ]
      data <- data[, rownames(seq), drop = FALSE]  # Filter data based on seq row names
      
      columns <- colnames(data)  # Extract column names
      
      default_val <- if ("refmet_name" %in% columns) {
        "refmet_name"
      } else {
        columns[1]  # Default to first column if "refmet_name" is not found
      }
      
      # **Fix: Wrap in `tagList()` so both UI elements render correctly**
      tagList(
        selectInput(
          "volcano_feature_column",  # Input ID for selecting feature column
          "Select Feature Column:",
          choices = columns,
          selected = default_val,
          width = "100%"
        ),
        
        selectizeInput(
          "selected_features_volcano",
          "Select Features:",
          choices = NULL,  # Choices will be updated dynamically
          multiple = TRUE,
          options = list(
            placeholder = "Search & Select Features",
            maxOptions = 100  # Show only 100 at a time
          )
        )
      )
    }
  })
  observeEvent(input$volcano_feature_column, {
    req(input$volcano_feature_column, input$select_volcano_data)  # Ensure valid inputs
    
    if (!is.null(rv$activeFile)) {
      if (input$select_volcano_data == "Unsaved data") {
        data <- rv$tmpData
      } else {
        sd <- which(rv$choices %in% input$select_volcano_data)
        data <- rv$data[[sd]]
      }
      
      # Get the unique feature names from the selected column
      feature_choices <- unique(data[[input$volcano_feature_column]])
      
      updateSelectizeInput(
        session,
        "selected_features_volcano",
        choices = feature_choices,
        selected = NULL,  # Reset selection
        server = TRUE  # **Enable Server-Side Processing**
      )
    }
  })
  
  output$group_selection_ui_volcano <- renderUI({
    if (!input$enable_group_selection) return(NULL)
    req(input$select_volcano_data)
    
    # Retrieve data (this example is based on your existing code)
    if (!is.null(rv$activeFile)) {
      if (input$select_volcano_data == "Unsaved data") {
        data <- rv$tmpData  
        seq <- rv$tmpSequence  
      } else {
        sd <- which(rv$choices %in% input$select_volcano_data)
        data <- rv$data[[sd]]
        seq <- rv$sequence[[sd]]
      }
      
      seq <- seq[!seq[, "labels"] %in% c("Sample", "QC"), ]
      data <- data[, rownames(seq), drop = FALSE]
      columns <- colnames(data)
      
      default_val <- if ("sub_class" %in% columns) {
        "sub_class"
      } else if ("Lipid.Abbreviation" %in% columns) {
        "Lipid.Abbreviation"
      } else {
        columns[1]
      }
    }
    
    tagList(
      selectInput(
        "volcano_group_column",
        "Select Group Column:",
        choices = columns,
        selected = default_val,
        width = "100%"
      ),
      selectizeInput(
        "selected_group_volcano",
        "Select Group:",
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "Search & Select Groups")
      )
    )
  })
  
  darken_color <- function(color, factor = 0.7) {
    rgb_val <- grDevices::col2rgb(color)
    dark_rgb <- pmax(rgb_val * factor, 0)
    rgb(t(dark_rgb), maxColorValue = 255)
  }
  
  output$group_color_ui <- renderUI({
    # Only show if group selection is enabled and at least one group is selected
    if (!input$enable_group_selection) return(NULL)
    req(input$selected_group_volcano)
    
    selected_groups <- input$selected_group_volcano
    n_groups <- length(selected_groups)
    default_colors <- hue_pal()(n_groups)

    # For each selected group, create two colourInput widgets:
    ui_list <- lapply(seq_along(selected_groups), function(i) {
      grp <- selected_groups[i]
      grp_id <- make.names(grp)
      
      fill_default <- default_colors[i]
      outline_default <- darken_color(fill_default)
      
      tagList(
        h4(paste("Group:", grp)),
        fluidRow(
          column(
            width = 6,
            colourInput(
              inputId = paste0("color_", grp_id, "_fill"),
              label = "Fill:",
              value = fill_default  # Default fill color
            )
          ),
          column(
            width = 6,
            colourInput(
              inputId = paste0("color_", grp_id, "_outline"),
              label = "Outline:",
              value = outline_default  # Default outline color
            )
          )
        )
      )
    })
    
    tagList(ui_list)
  })
  observeEvent(input$volcano_group_column, {
    req(input$volcano_group_column,
        input$select_volcano_data)  # Ensure valid inputs
    
    if (!is.null(rv$activeFile)) {
      if (input$select_volcano_data == "Unsaved data") {
        data <- rv$tmpData
      } else {
        sd <- which(rv$choices %in% input$select_volcano_data)
        data <- rv$data[[sd]]
      }
      
      # Ensure the selected column exists in data before accessing it
      if (!input$volcano_group_column %in% colnames(data)) return()
      
      unique_groups <- unique(data[[input$volcano_group_column]])
      
      updateSelectizeInput(
        session,
        "selected_group_volcano",
        choices = unique_groups,
        selected = unique_groups[2],  # Reset selection
        server = TRUE  # Enable server-side processing for large lists
      )
      
    }
  })
  
  output$parameter_selection_ui_volcano <- renderUI({
    if (isTRUE(input$select_parameter_volcano)) {
      fluidRow(
        column(
          width = 6,
          numericInput("x_param", "X axis limit", value = 5)
        ),
        column(
          width = 6,
          numericInput("y_param", "Y axis limit", value = 5)
        )
      )
    }
  })
  
  observeEvent(input$run_volcano_plot, {
    req(
      input$select_volcano_data,
      input$volcano_labels,
      input$group1_vol, 
      input$group2_vol,
      input$log2fc_threshold, 
      input$pval_threshold,
      input$color_up_fill,
      input$color_up_outline,
      input$color_down_fill,
      input$color_down_outline,
      input$color_ns_fill,
      input$color_ns_outline
    )

    if (!is.null(rv$activeFile)) {
      if (input$select_volcano_data == "Unsaved data") {
        data <- rv$tmpData  # Use the temporary data
        seq <- rv$tmpSequence  # Use the temporary sequence
        dataset_name <- "Unsaved data"
      } else {
        # Get the index of the selected dataset
        sd <- which(rv$choices %in% input$select_volcano_data)
        data <- rv$data[[sd]]  # Retrieve the selected dataset
        seq <- rv$sequence[[sd]]  # Retrieve the selected sequence
        dataset_name <- names(rv$data)[sd]  # Retrieve dataset name
      }
      
      label_column <- input$volcano_labels
      numerator <- input$group1_vol
      denominator <- input$group2_vol
      
      # make an error check that numerator and denominator are not the same
      if (numerator == denominator) {
        showNotification(
          "The numerator and denominator groups must be different!",
          type = "warning"
        )
        return(NULL) # return from the function/observe and don't proceed
      }
      
      log2FC_tresh <- log2(input$log2fc_threshold)
      pAdjustMethod <- input$pAdjustMethod_volcano
      pval_tresh <- input$pval_threshold
      pval_col <- input$pval_col_volcano
      
      fill_up <- input$color_up_fill
      outline_up <- input$color_up_outline
      fill_down <- input$color_down_fill
      outline_down <- input$color_down_outline
      fill_ns <- input$color_ns_fill
      outline_ns <- input$color_ns_outline
      
      show_legend <- input$show_legend_volcano
      
      x_param <- input$x_param
      y_param <- input$y_param
      apply_axis_limits <- input$select_parameter_volcano
      
      message(paste0("dataset_name: ", dataset_name))
      message(paste0("label column: ", label_column))
      message(paste0("numerator: ", numerator))
      message(paste0("denominator: ", denominator))
      message(paste0("log2FC input: ", 2^(log2FC_tresh)))
      message(paste0("log2FC tresh: ", log2FC_tresh))
      message(paste0("pval tresh: ", pval_tresh))
      message(paste0("show legend: ", show_legend))
      
      message(paste0("Axis limits: ", apply_axis_limits))
      
      if (input$select_parameter_volcano) {
        message(paste0("X axis limit: ", x_param))
        message(paste0("Y axis limit: ", y_param))
      }
    
      enable_feature_selection <- input$enable_feature_selection
      message(paste0("Feature Selection Enabled: ", enable_feature_selection))
      if (enable_feature_selection) {
        available_features <- input$selected_features_volcano
        message("Available Features:")
        print(available_features)
      }
      
      enable_group_selection <- input$enable_group_selection
      message(paste0("Group Selection Enabled: ", enable_group_selection))
      if (enable_group_selection) {
        available_groups <- input$selected_group_volcano
        message("Available Groups:")
        print(available_groups)
      }
      
      
      if (input$enable_group_selection && !is.null(input$selected_group_volcano)) {
        selected_groups <- input$selected_group_volcano
        group_color_df <- do.call(rbind, lapply(selected_groups, function(grp) {
          grp_id <- make.names(grp)
          data.frame(
            Group   = grp,
            Fill    = input[[paste0("color_", grp_id, "_fill")]],
            Outline = input[[paste0("color_", grp_id, "_outline")]],
            stringsAsFactors = FALSE
          )
        }))
        
        # Mapping each group to its fill and outline colors.
        print(group_color_df)
      }
      
      if (enable_feature_selection) {
        if (length(available_features) == 0) {
          showNotification(
            "Select at least one feature for the volcano plot.",
            type = "warning"
          )
          return(NULL)  # Stop execution if no features are selected
        }
        
        showNotification("This feature is of limited use and may not work as expected. ",
                 "As of 5/3-2025 this feature is still under development.", type = "message")
        
        # Check if feature column exists
        if (input$volcano_feature_column %in% colnames(data)) {
          # Filter data to include only selected features
          feature_sub <- data[data[[input$volcano_feature_column]] %in% available_features, , drop = FALSE]
          
          # Print only the first few rows of the selected feature subset
          print("Subset of Selected Features:")
          print(head(feature_sub,2))
        } else {
          showNotification("Feature column not found in dataset.", type = "error")
        }
      }
      
      if (enable_group_selection) {
        if (length(available_groups) == 0) {
          showNotification(
            "Select at least one group for the volcano plot.",
            type = "warning"
          )
          return(NULL)  # Stop execution if no groups are selected
        }
        
        # Check if group column exists
        if (input$volcano_group_column %in% colnames(data)) {
          # Filter data to include only selected groups
          group_sub <- data[data[[input$volcano_group_column]] %in% available_groups, , drop = FALSE]
          
          # Print only the first few rows of the selected group subset
          print("Subset of Selected Groups:")
          print(head(group_sub,2))
        } else {
          showNotification("Group column not found in dataset.", type = "error")
        }
      }
      
      seq_subset <- seq[seq[, "labels"] %in% c("Sample"), ]  # Restrict to "Sample" rows
      data_subset <- data[, c(rownames(seq_subset)), drop = FALSE]  # Use row names of seq_subset to filter columns
      
      selected_labels <- as.character(data[[label_column]])
      fallback <- if ("Name" %in% colnames(data)) {
        as.character(data[["Name"]])
      } else if ("name" %in% colnames(data)) {
        as.character(data[["name"]])
      } else {
        NULL
      }
      if (is.null(fallback)) {
        showNotification("No fallback column ('Name' or 'name') available.", type = "error")
        return()
      }
      missing <- is.na(selected_labels) | selected_labels == ""
      selected_labels[missing] <- fallback[missing]
      rownames(data_subset) <- make.unique(selected_labels)
      rownames(data) <- make.unique(selected_labels)
      
      stat_results <- calculate_stats(data_subset, seq_subset, adjust.method = pAdjustMethod)
      
      target_contrast   <- paste0(numerator, "_vs_", denominator)
      reversed_contrast <- paste0(denominator, "_vs_", numerator)
      
      # If  contrast is present in stat_results:
      if (target_contrast %in% stat_results$Contrast) {
        # Just subset
        sub_df <- subset(stat_results, Contrast == target_contrast)
      } else if (reversed_contrast %in% stat_results$Contrast) {
        # Subset, then flip the log2FC & FC
        sub_df <- subset(stat_results, Contrast == reversed_contrast)
        sub_df$log2FC <- -sub_df$log2FC
        sub_df$FC     <- 1 / sub_df$FC
        
        # Rename the contrast column to reflect the new direction
        sub_df$Contrast <- target_contrast
      } else {
        # No matching contrast found; handle how you like (warn user, or return empty)
        warning(
          paste0("No matching contrast found for '", numerator, " vs ", denominator, "'. ",
                 "Available contrasts are: ", paste(unique(stat_results$Contrast), collapse=", "))
        )
        sub_df <- data.frame()
      }
      
      data <- data %>%
        rownames_to_column("Feature_ID")
      
      sub_df <- sub_df %>%
        rownames_to_column("Feature_ID") %>%
        relocate(Feature_ID, .before = "Contrast")
      
      # make the rownames as the rownames of the sub_df
      rownames(sub_df) <- sub_df$Feature_ID
      sub_df$Feature_ID <- gsub("^[^.]+\\.", "", sub_df$Feature_ID)
      
      if (input$enable_group_selection) {
        sub_df <- sub_df %>%
          left_join(
            data %>% select(Feature_ID, !!sym(input$volcano_group_column)),
            by = "Feature_ID"
          ) %>%
          rename(Group = !!sym(input$volcano_group_column)) %>%
          relocate(Group, .before = "Contrast")
      }
      
      # assign the sub_df a name for debugging
      volcano_df_name <- paste0(dataset_name, "_volcano_df")
      assign(volcano_df_name, sub_df)
      
      pvp <- reactive(volcano_plot(
        sub_df,savedDatasetNameVolcano(),
        log2FC_tresh, pval_tresh,
        fill_up, outline_up,
        fill_down, outline_down,
        fill_ns, outline_ns,
        enable_feature_selection, enable_group_selection,
        available_features, available_groups,
        group_color_df,x_param,y_param, apply_axis_limits,
        pval_col))
      
      # Render the volcano plot
      output$volcano_plot <- renderPlotly({
        pvp()$plot
      })
      
      # Render the table of top features
      output$volcano_table <- DT::renderDataTable({
        df <- pvp()$df
        df <- df %>%
          select(-c(hover_text,Contrast))
        DT::datatable(df, 
                      rownames = FALSE,
                      options = list(scrollX = TRUE,
                                     pageLength = 20,
                                     autoWidth = TRUE))
      })
      
      message(sample(quotes, 1))
    }
  })
  
  ######################
  # Pathway Enrichment #
  ######################
  # Over representation analysis (ORA) ----
  observeEvent(input$select_data_for_enrichment, {
    req(input$select_data_for_enrichment)  # Ensure a dataset is selected
    
    if (!is.null(rv$activeFile)) {
      if (input$select_data_for_enrichment == "Unsaved data") {
        data <- rv$tmpData  # Use the temporary data
      } else {
        # Get the index of the selected dataset
        sd <- which(rv$choices %in% input$select_data_for_enrichment)
        data <- rv$data[[sd]]  # Retrieve the selected dataset
      }
      
      # Extract column names from the selected dataset
      data_colnames <- colnames(data)
      
      # Define the IDs for the selectInputs you wish to update
      select_ids <- c("identifier_column", "compound_column")
      
      for (col in select_ids) {
        # Set default based on which input it is
        if (col == "identifier_column") {
          default_val <- if ("Structure" %in% data_colnames) {
            "Structure"
          } else if ("InChI" %in% data_colnames) {
            "InChI"
          } else {
            ""
          }
        } else if (col == "compound_column") {
          # Change this logic as needed. For example, default to "Compound" if present.
          default_val <- if ("Original annotation" %in% data_colnames) {
            "Original annotation"
          } else {
            data_colnames[1]  # Fallback: use the first column
          }
        }
        
        updateSelectInput(session, col, choices = data_colnames, selected = default_val)
      }
    }
    
    message(sample(quotes, 1))
  })
  observeEvent(input$run_gather_identifiers, {
    req(input$select_data_for_enrichment,
        input$identifier_column,
        input$compound_column)
    
    message("Running pathway enrichment analysis...")
    
    show_modal_spinner(
      spin = "atom",
      color = "#0A4F8F",
      text = paste("Gathering Identifiers... This may take a few minutes.")
    )
    
    # Process logic
    if (!is.null(rv$activeFile)) {
      if (input$select_data_for_enrichment == "Unsaved data") {
        data <- rv$tmpData
        seq <- rv$tmpSequence
      } else {
        sd <- which(rv$choices %in% input$select_data_for_enrichment)
        data <- rv$data[[sd]]
        seq <- rv$sequence[[sd]]
      }
      
      # Get the column name for identifiers
      identifier <- input$identifier_column
      compound <- input$compound_column
      
      message(paste0("Number of features: ", nrow(data)))
      
      subset <- data
      
      message(paste0("Number of features after filtering: ", nrow(subset)))
      
      # Set this to TRUE during development and FALSE in production
      run_development_code <- TRUE
      
      if (run_development_code) {
        # Check how many rows query has initially
        
        update_modal_spinner(
          session = session,
          text = paste("Updateing query file... Please be patient. \n (1/5)")
        )
        
        query_start <- nrow(query)
        message(paste0("Number of established features: ", query_start))
        
        desired_properties <- c(
          "MolecularFormula", "MolecularWeight", "ExactMass", "MonoisotopicMass",
          "CanonicalSMILES", "IsomericSMILES", "InChI", "InChIKey", "IUPACName"
        )
        
        all_results <- data.frame()
        chunk_size <- 5
        
        # Identify new compounds (those not already in query$Identifier) using dplyr
        new_compounds <- subset %>%
          pull(!!sym(compound)) %>%  # extract the column as a vector
          { .[!. %in% query$Identifier] } %>%  # filter out entries that are in query$Identifier
          unique()
        
        clean_names <- TRUE # Set to TRUE to clean compound names else FALSE
        if (clean_names) {
          new_compounds <- gsub(";.*", "", new_compounds)
        }
        message(paste("Number of features not in query:", length(new_compounds)))
        
        if (length(new_compounds) > 0) {
          num_rows <- length(new_compounds)
          row_indices <- seq(1, num_rows, by = chunk_size)
          print("Row indices to be processed:")
          print(row_indices)
          
          if (num_rows > 0) {
            for (start_idx in row_indices) {
              end_idx <- base::min(start_idx + chunk_size - 1, num_rows)
              print(paste("Processing features from", start_idx, "to", end_idx))
              
              compound_subset <- new_compounds[start_idx:end_idx]
              print("Current features subset:")
              print(compound_subset)
              
              props_chunk <- tryCatch({
                get_properties(
                  properties = desired_properties,
                  identifier = compound_subset,
                  namespace = "name",
                  propertyMatch = list(.ignore.case = TRUE, type = "contain")
                )
              }, error = function(e) {
                warning("Failed to get properties for compounds: ", paste(compound_subset, collapse = ", "))
                return(NULL)
              })
              
              if (is.null(props_chunk) || !inherits(props_chunk, "PubChemInstanceList")) {
                print("props_chunk is NULL or not a PubChemInstanceList, skipping this batch.")
                Sys.sleep(1) # respect rate limit
                next
              }
              
              props_retrieved <- tryCatch({
                retrieve(object = props_chunk, .to.data.frame = TRUE, .combine.all = TRUE)
              }, error = function(e) {
                warning("Failed to retrieve data for compounds: ", paste(compound_subset, collapse = ", "))
                return(data.frame())
              })
              
              if (!is.null(props_retrieved) && nrow(props_retrieved) > 0) {
                all_results <- bind_rows(all_results, props_retrieved)
              } else {
                print("No valid rows retrieved for this batch.")
              }
              
              print(Sys.time())
              # Pause to respect the 5 queries/second limit
              Sys.sleep(1)
            }
          } else {
            message("No new compounds to process; skipping property queries.")
          }
          
          
          if (nrow(all_results) > 0) {
            # use dplyr to convert columns to numeric
            all_results <- all_results %>%
              mutate_at(vars(MolecularWeight,
                             ExactMass,
                             MonoisotopicMass),
                        as.numeric)
            
            dir_path <- "~/Desktop/SDU/Cand/Master thesis /GitHub/MetaboLink-Main/csvfiles" # Adjust path as needed
            file_path <- file.path(dir_path, "queried_properties.csv")
            
            # If the file already exists, read it and combine
            if (file.exists(file_path)) {
              existing_data <- read.csv(file_path, stringsAsFactors = FALSE)
              
              # Identify new entries that are not already in existing_data by 'Identifier'
              if ("Identifier" %in% colnames(existing_data) && "Identifier" %in% colnames(all_results)) {
                new_entries <- anti_join(all_results, existing_data, by = "Identifier")
              } else {
                # If 'Identifier' is not present, just append all results
                warning("No 'Identifier' column found. Appending all results without filtering.")
                new_entries <- all_results
              }
              
              # Only bind if there are new entries
              if (nrow(new_entries) > 0) {
                combined_data <- bind_rows(existing_data, new_entries)
                # Remove duplicates based on Identifier if needed
                combined_data <- combined_data[!duplicated(combined_data$Identifier), ]
                
                print(paste("Number of rows in query after gathering identifiers:", nrow(combined_data)))
                
                write.csv(combined_data, file_path, row.names = FALSE)
                message("queried_properties.csv has been updated with new entries.")
              } else {
                message("No genuinely new entries to add. The existing queried_properties.csv remains unchanged.")
              }
              
            } else {
              # No existing file, just create a new one
              write.csv(all_results, file_path, row.names = FALSE)
              message("queried_properties.csv has been created with the new entries.")
            }
            
          } else {
            message("No results retrieved for these new compounds.")
          }
        } else {
          message("No new compounds found to query.")
        }
        
      } else {
        # If run_development_code is FALSE, do nothing here
        message("Development code is disabled. Skipping property queries.")
      }
      
      update_modal_spinner(
        session = session,
        text = paste("Updating InChI... Please be patient. \n (2/5)")
      )
      
      old_columns <- colnames(subset)
      
      # Append the information from query to subset 
      subset <- subset %>%
        left_join(
          query %>% 
            select(CID, InChI, CanonicalSMILES, IsomericSMILES, InChIKey, IUPACName),
          by = setNames("InChI", identifier),
          relationship = "many-to-many"
        )
      
      if ("InChIKey.x" %in% colnames(subset)) {
        # Merge InChIKey.x and InChIKey.y into InChIKey
        subset <- subset %>%
          mutate(InChIKey = coalesce(InChIKey.x, InChIKey.y)) %>%
          select(-c(InChIKey.x, InChIKey.y))
      }
      
      subset <- subset %>%
        relocate(all_of(c("InChIKey", "CID", "CanonicalSMILES", "IsomericSMILES", "IUPACName")), .after = identifier)
      
      # Remove duplicated columns based on InChI 
      subset <- subset %>% distinct(.keep_all = TRUE)
      
      message(paste0("Number of features after joining query: ", nrow(subset)))
      # print(head(subset))
      
      # Update InChI from local cache and online queries
      message("Running update_inchi function...")
      subset_updated <- update_inchi(subset, compound, identifier, query)
      message("Update InChI function completed.")
      
      update_modal_spinner(
        session = session,
        text = paste("Looking for CID's... Please be patient. \n (3/5)")
      )
      
      # Gather CID's after InChI update
      message("Running update_cid function...")
      data_updated <- update_cid(subset_updated, compound, identifier, query)
      # message("DATA UPDATED with CID INFO: ")
      # print(head(data_updated))
      message("Update cid function completed.")
      
      update_modal_spinner(
        session = session,
        text = paste("Adding refmet information... Please be patient. \n (4/5)")
      )
      
      refmet_updated <- refmet %>%
        filter(
          ( !is.na(pubchem_cid) & pubchem_cid %in% data_updated$CID ) |
            ( !is.na(inchi_key) & inchi_key %in% data_updated$InChIKey ))
      
      filtered_refmet <- refmet_updated %>%
        filter(pubchem_cid %in% data_updated$CID |
                 inchi_key %in% data_updated$InChIKey)
      
      filtered_refmet$pubchem_cid <- as.character(filtered_refmet$pubchem_cid)
      colnames_refmet <- colnames(filtered_refmet)
      # print(head(filtered_refmet))
      
      # For data_updated, use CID if available; otherwise, use InChIKey
      data_updated <- data_updated %>%
        mutate(join_key = coalesce(CID, InChIKey))
      
      # For filtered_refmet, use pubchem_cid if available; otherwise, use InChIKey
      filtered_refmet <- filtered_refmet %>%
        mutate(join_key = coalesce(pubchem_cid, inchi_key))
      
      # Now join by join_key. Only rows where at least one key is available and matches will join.
      final_data <- data_updated %>%
        left_join(
          filtered_refmet %>% select(all_of(colnames_refmet), join_key),
          by = "join_key",
          relationship = "many-to-many"
        )
      
      final_data <- final_data %>%
        relocate(any_of(colnames_refmet), .after = IUPACName) %>%
        select(-c(pubchem_cid, inchi_key))
      
      message(paste0("Number of features after merge with refmet: ", nrow(final_data)))
      
      update_modal_spinner(
        session = session,
        text = paste("Looking for pathways... Please be patient. \n (5/5)")
      )
      
      print(head(final_data))
      
      # add pathways to final_data 
      data_updated_list <- get_kegg_pathways(final_data)
      
      pathways_long <- data_updated_list$pathways_long
      data_updated_pathways <- data_updated_list$data_joined
      
      # data_updated_pathways <- data_updated_pathways %>%
      #   select(-join_key)
      
      # add new columns to data_updated_pathways
      new_cols <- setdiff(colnames(data_updated_pathways), old_columns)
      print(new_cols)
      
      identifier_index <- which(names(data_updated_pathways) == identifier)
      
      original_cols <- names(data_updated_pathways)[!(names(data_updated_pathways) %in% new_cols)]
      
      final_col_order <- append(original_cols, new_cols, after = identifier_index)
      
      final_data <- data_updated_pathways[, final_col_order]
      
      # print(head(final_data))
      
      common_cols <- setdiff(intersect(names(final_data), names(refmet)), c(compound, "refmet_name"))
      
      # Perform the left join using final_data[[compound]] and refmet$refmet_name.
      final_data_updated <- final_data %>%
        left_join(refmet, by = setNames("refmet_name", compound), suffix = c("", ".refmet")) %>%
        # For each overlapping column (excluding the join key columns), update with the value from refmet if available.
        mutate(across(
          .cols = all_of(common_cols),
          .fns = ~ coalesce(get(paste0(cur_column(), ".refmet")), .x)
        )) %>%
        # Remove the temporary columns from refmet (those ending in ".refmet")
        select(-ends_with(".refmet"))
      
      additional_keys <- c("lipid_name", "sum_name", "Normalized.Name", "Species.Name")  # additional keys
      
      for(key in additional_keys) {
        if(key %in% names(final_data_updated)) {
          final_data_updated <- final_data_updated %>%
            left_join(refmet, by = setNames("refmet_name", key),
                      suffix = c("", paste0(".", key))) %>%
            mutate(across(
              .cols = all_of(common_cols),
              .fns = ~ coalesce(get(paste0(cur_column(), ".", key)), .x)
            )) %>%
            select(-ends_with(paste0(".", key)))
        }
      }
      
      final_data_updated <- final_data_updated %>%
        # make the pubchem_cid as charactor
        mutate(pubchem_cid = as.character(pubchem_cid))
      
      final_data_updated <- final_data_updated %>%
        mutate(
          CID = coalesce(CID, pubchem_cid),
          InChIKey = coalesce(InChIKey, inchi_key)
        ) %>%
        select(-pubchem_cid, -inchi_key)
      
      
      message("Number of features after final merge with refmet:", nrow(final_data_updated))
      
      final_data <- final_data_updated
      
      cat("\n--- Final dataset ---\n")
      print(head(final_data))
      message(paste0("Number of features after adding pathways: ", nrow(final_data)))
      
      # make a subset of the data which contains only the identifiers
      identifier_data <- final_data[, c(compound, identifier, new_cols)]
      # remove rows where NA, "", " ", "NA" are present in the compound column
      identifier_data <- identifier_data[!is.na(identifier_data[, compound]) & identifier_data[, compound] != "" & identifier_data[, compound] != " " & identifier_data[, compound] != "NA" & identifier_data[, compound] != "N/A", ]
      
      # sum column with no NA, "", " " or "NA". This will give the number of identifiers
      # print(sapply(final_data,function(x) sum(!is.na(x) & x != "" & x != " " & x != "NA" & x != "N/A")))
      
      # Store identifiers
      rv$identifier_df <- identifier_data
      
      # Debug
      print("Are data column names and sequence row names identical?")
      print(identical(colnames(rv$tmpData), rownames(rv$tmpSequence)))
      
      # Update sequence
      print("Updating sequence...")
      updated_seq <- updateSequence(seq, final_data, identifier, "-")
      
      # Store temporarily
      rv$tmpData <- final_data
      rv$tmpSequence <- updated_seq
      
      # Debug
      print("Are data column names and sequence row names identical?")
      print(identical(colnames(rv$tmpData), rownames(rv$tmpSequence)))
      
      
      # Update dataset and notify user
      updateDataAndSequence(
        notificationMessage = paste("Identifiers gathered from column:", identifier),
        newFileInput = TRUE,
        suffix = "_id",
        additionalInfo = NULL
      )
      
      sendSweetAlert(session, "Success", "Identifiers gathered and data updated.", type = "success")
    }
    
    message(sample(quotes, 1))
    
    remove_modal_spinner()
  })
  
  # Render the identifier count table
  output$identifier_count_table <- renderDT({
    req(input$select_data_for_enrichment)
    
    # Define your desired vector of column names
    desired_cols <- c("Name", "Normalized.Name", "Species.Name" , "Lipid.Abbreviation", "Original.annotation",
                      "Original annotation", "Structure", "InChI", "InChIKey", "CID", "smiles", "CanonicalSMILES", "IsomericSMILES",
                      "IUPACName", "refmet_id", "refmet_name", "super_class", "main_class", "sub_class", "chebi_id",
                      "hmdb_id", "lipidmaps_id", "kegg_id")
    
    sd <- which(rv$choices %in% input$select_data_for_enrichment)
    data <- rv$data[[sd]]
    
    # Only include columns from your desired list that exist in the selected data
    cols_to_include <- intersect(desired_cols, colnames(data))
    req(length(cols_to_include) > 0)
    
    # Compute valid counts for each desired column in data
    valid_counts <- sapply(cols_to_include, function(col) {
      sum(!is.na(data[[col]]) & data[[col]] != "")
    })
    
    # Compute NA/empty counts for each desired column in data
    na_empty_counts <- sapply(cols_to_include, function(col) {
      sum(is.na(data[[col]]) | data[[col]] == "")
    })
    
    # Create a data frame with a row for each statistic and a column for each desired column
    df <- data.frame(
      Statistic = c("Valid Count", "NA/Empty Count"),
      matrix(c(valid_counts, na_empty_counts),
             nrow = 2, byrow = TRUE, 
             dimnames = list(NULL, cols_to_include))
    )
    
    # Render the datatable with additional options for scrolling
    DT::datatable(
      df,
      options = list(
        dom = 't',
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left;',
        HTML(paste0("<b>Total numbers of features: </b>", nrow(data)))
      )
    )
  })
  
  # Create a reactive expression to store the data used in dt_table
  dataForPathEnri <- reactive({
    req(input$select_data_for_enrichment)
    
    if (!is.null(rv$activeFile)) {
      if (input$select_data_for_enrichment == "Unsaved data") {
        data <- rv$tmpData
        seq <- rv$tmpSequence
      } else {
        sd <- which(rv$choices %in% input$select_data_for_enrichment)
        data <- rv$data[[sd]]
        seq <- rv$sequence[[sd]]
      }
      
      # If the specific columns are present, select only those columns.
      if (all(c("Name", "refmet_name", "main_class") %in% colnames(data))) {
        data <- data %>% select(Name, refmet_name, main_class)
      }
      data
    }
  })
  #TODO: Might be relevant to extract single or several features to quickly identify in plots. 
  # Render the datatable 
  # output$dt_table_path <- renderDT({
  #   datatable(dataForPathEnri(),
  #             options = list(autoWidth = TRUE,
  #                            scrollY = "300px",
  #                            pageLength = 50))
  # })
  # Extract the selected rows from the dt_table:
  selectedData <- reactive({
    selectedRows <- input$dt_table_path_rows_selected
    if (length(selectedRows) > 0) {
      dataForPathEnri()[selectedRows, ]
    } else {
      NULL
    }
  })
  
  # Run enrichment analysis
  observeEvent(input$run_enrichment_analysis, {
    req(input$select_data_for_enrichment,
        input$group1_enrichment,
        input$group1_enrichment,
        input$top_x_enrich)
    
    
    # print("Selected rows:")
    # print(selectedData())
    
    # Make sure group1_enrichment and group2_enrichment is not the same 
    if (input$group1_enrichment == input$group2_enrichment) {
      sendSweetAlert(session, "Error", "Select different groups for comparison.", type = "error")
      return()
    }

    # Ensure only one of gene/module is selected
    if (input$gene_selected && input$module_selected) {
      sendSweetAlert(session, "Error", "Select only one: Pathway or Module.", type = "error")
      return()
    } else if (!input$gene_selected && !input$module_selected) {
      sendSweetAlert(session, "Error", "Please select either Pathway or Module.", type = "error")
      return()
    }
    
    sd <- which(rv$choices == input$select_data_for_enrichment)
    data <- rv$data[[sd]]
    seq <- rv$sequence[[sd]]
    
    # display warning if column "kegg_id" or "kegg id" is not present
    if (!("kegg_id" %in% colnames(data) || "kegg id" %in% colnames(data))) {
      sendSweetAlert(session, "Error",
                     "No KEGG ID column found in the dataset. 
      Make sure column named 'kegg_id' is present by running 'Gather Identifiers' ", type = "error")
      return()
    }
    
    # display warning if column "refmet_name" or "refmet name" is not present
    # if (!("refmet_name" %in% colnames(data) || "refmet name" %in% colnames(data))) {
    #   sendSweetAlert(session, "Error",
    #                  "No Reference Metabolite Name column found in the dataset. 
    #   Make sure column named 'refmet_name' is present by running 'Gather Identifiers' ", type = "error")
    #   return()
    # }
    
    # display warning if column "Original annotation" is not present 
    # if (!("Original annotation" %in% colnames(data))) {
    #   sendSweetAlert(session, "Error",
    #                  "No Original annotation column found in the dataset. 
    #   Make sure column named 'Original annotation' is present before running ORA ", type = "error")
    #   return()
    # }
    
    show_modal_spinner(
      spin = "atom",
      color = "#0A4F8F",
      text = "Running analysis"
    )
    
    # Extract user-selected values
    group_of_interest <- input$group1_enrichment
    comparison_group <- input$group2_enrichment
    top_x_features <- as.numeric(input$top_x_enrich)
    dataset_name <- names(rv$data)[sd]
    
    
    p_value_thresh <- input$p_value_threshold_enrich
    pAdjustMethod <- input$pAdjustMethod_enrich
    minGSSize <- input$minGSSize_enrich
    maxGSSize <- input$maxGSSize_enrich
    qvalueCutoff <- input$qvalueCutoff_enrich
    color_con <- input$color_con_enrich
    
    message("P-value theshold: ", p_value_thresh)
    message("pAdjustMethod: ", pAdjustMethod)
    message("minGSSize: ", minGSSize)
    message("maxGSSize: ", maxGSSize)
    message("qvalueCutoff: ", qvalueCutoff)
    message("color_con: ", color_con)
    
    message("Running pathway enrichment analysis...")
    message("Selected dataset: ", dataset_name)
    message("Group of interest: ", group_of_interest)
    message("Comparison group: ", comparison_group)
    message("Top X features: ", top_x_features)
    
    # Subset sequence and data based on "Sample" labels
    filtered_sequence <- seq[seq[, "labels"] %in% c("Sample"), ]
    # Define preference order
    prefs <- c("refmet_name", "Original.annotation", "Original annotation", "Name", "name")
    
    # Find the first one that actually exists in your data.frame
    chosen <- prefs[prefs %in% names(data)][1]
    
    filtered_data <- data[, c(chosen, "kegg_id", rownames(filtered_sequence)), drop = FALSE]
    
    filtered_data <- filtered_data %>%
      filter(!is.na(kegg_id), kegg_id != "", kegg_id != " ", kegg_id != "NA", kegg_id != "N/A")
    
    message("Filtered data:")
    print(head(filtered_data))
    message("Filtered sequence:")
    print(head(filtered_sequence))
    
    selected_labels <- as.character(filtered_data[["kegg_id"]])
    fallback <- if ("Original.annotation" %in% colnames(data)) {
      as.character(data[["Original.annotation"]])
    } else if ("refmet_name" %in% colnames(data)) {
      as.character(data[["Name"]])
    } else if ("Name" %in% colnames(data)) {
      as.character(data[["Name"]])
    } else if ("name" %in% colnames(data)) {
      as.character(data[["name"]])
    } else {
      NULL
    }
    if (is.null(fallback)) {
      showNotification("No fallback column ('Name' or 'name') available.", type = "error")
      return()
    }
    missing <- is.na(selected_labels) | selected_labels == ""
    selected_labels[missing] <- fallback[missing]
    rownames(filtered_data) <- make.unique(selected_labels)

    stat_results <- calculate_stats(filtered_data, filtered_sequence, adjust.method = "BH")
    
    message("Stat results:")
    print(head(stat_results))
    
    target_contrast   <- paste0(group_of_interest, "_vs_", comparison_group)
    reversed_contrast <- paste0(comparison_group, "_vs_", group_of_interest)
    
    message("target_contrast")
    print(target_contrast)
    message("reversed_contrast")
    print(reversed_contrast)
    
    # If  contrast is present in stat_results:
    if (target_contrast %in% stat_results$Contrast) {
      # Just subset
      stats_df <- subset(stat_results, Contrast == target_contrast)
    } else if (reversed_contrast %in% stat_results$Contrast) {
      # Subset, then flip the log2FC & FC
      stats_df <- subset(stat_results, Contrast == reversed_contrast)
      stats_df$log2FC <- -stats_df$log2FC
      stats_df$FC     <- 1 / stats_df$FC
      
      # Rename the contrast column to reflect the new direction
      stats_df$Contrast <- target_contrast
    } else {
      # No matching contrast found; handle how you like (warn user, or return empty)
      warning(
        paste0("No matching contrast found for '", numerator, " vs ", denominator, "'. ",
               "Available contrasts are: ", paste(unique(stat_results$Contrast), collapse=", "))
      )
      stats_df <- data.frame()
    }
    
    stats_df <- stats_df %>%
      rownames_to_column("kegg_id") %>%
      relocate(kegg_id, .before = "Contrast")
    
    # make the rownames as the rownames of the sub_df
    rownames(stats_df) <- stats_df$kegg_id
    stats_df$kegg_id <- gsub("^[^.]+\\.", "", stats_df$kegg_id)
    
    stats_df <- stats_df %>%
      mutate(diffexpressed = case_when(
        log2FC > 0.585 & p.adj < 0.05 ~ "Upregulated", #TODO fix such that user defines
        log2FC < 0.585 & p.adj < 0.05 ~ "Downregulated", # TODO fix such that user defines
        p.adj > 0.05 ~ "Non-Significant"
      ))
    
    # remove .X from the kegg_id
    stats_df$kegg_id <- gsub("\\.\\d+$", "", stats_df$kegg_id)
    
    message("Stats df: ")
    print(head(stats_df))
    
    # If the stats_df is only contain Non-Significant then return nothing and let the user know 
    if (all(stats_df$diffexpressed == "Non-Significant", na.rm = TRUE)) {
      sendSweetAlert(session, "Error", "No significant results found. ", type = "error")
      remove_modal_spinner()
      return()
    }
    
    stat_df_signi <- stats_df[stats_df$diffexpressed != "Non-Significant",]
    
    sig_kegg_id <- sort(stat_df_signi$kegg_id)
    print("Significant KEGG IDs:")
    print(sig_kegg_id)
    
    bg_genes <- as.data.frame(unique(data$kegg_id))
    colnames(bg_genes) <- "unique_kegg_id"
    # remove rows where NA, "", " " or "NA".
    universe <- bg_genes[!is.na(bg_genes$unique_kegg_id) &
                           bg_genes$unique_kegg_id != "" &
                           bg_genes$unique_kegg_id != " " &
                           bg_genes$unique_kegg_id != "NA" &
                           bg_genes$unique_kegg_id != "N/A", ]
    
    # print(universe)
    
    # Run enrichment analysis based on selection
    if (input$gene_selected) {
      update_modal_spinner(
        session = session,
        text = "Pathway enrichment analysis in progress... Please be patient."
      )
      
      title_name <- "Pathway Enrichment Analysis"
      print(paste0("#---", title_name, " ", dataset_name, "---#"))
      
      res_geneCentric_ORA <- enrichKEGG(
        gene = sig_kegg_id,
        organism = "cpd",
        keyType = "kegg",
        pvalueCutoff = p_value_thresh,
        pAdjustMethod = pAdjustMethod,
        universe,
        minGSSize = minGSSize,
        maxGSSize = maxGSSize,
        qvalueCutoff = qvalueCutoff,
        use_internal_data = FALSE
      )
      
      res_df_gene_ORA <- as.data.frame(res_geneCentric_ORA)

      # Convert rownames to a column
      res_df_gene_ORA <- res_df_gene_ORA %>%
        tibble::rownames_to_column(var = "Pathway_ID")
      
      message("Enrichment results DF1:")
      print(head(res_df_gene_ORA))
      
      # if res_df_gene_ora is empty then return nothing and let the user know
      if (nrow(res_df_gene_ORA) == 0) {
        sendSweetAlert(session, "Error", "No significant results found during ORA.", type = "error")
        remove_modal_spinner()
        return()
      }
      
      # Extract the "Upregulated"/"Downregulated" status and store in a new column
      res_df_gene_ORA <- res_df_gene_ORA %>%
        mutate(Regulation = ifelse(grepl("^Upregulated", Pathway_ID), "Upregulated", "Downregulated"),
               Pathway_ID = gsub("^(Upregulated|Downregulated)\\.", "", Pathway_ID)) %>% # Remove label from Pathway_ID
        select(-Pathway_ID) %>%
        relocate(Regulation, .after = "Count")

      # Check the updated dataframe
      message("Enrichment results DF:")
      print(head(res_df_gene_ORA))
      
      # Display message if all p.adjust values are larger than the threshold
      if (all(res_df_gene_ORA$p.adjust > p_value_thresh)) {
        sendSweetAlert(session,
                       "Warning",
                       paste("All adjusted p-values values are larger than the threshold.",
                             "The lowest adjusted p-values value is", min(res_df_gene_ORA$p.adjust),
                             ". Try setting the threshold larger."), 
                       type = "Warning")
        remove_modal_spinner()
        return()
      }
      
      if (all(res_df_gene_ORA$qvalue > qvalueCutoff)) {
        sendSweetAlert(session,
                       "Warning",
                       paste("All q-values are larger than the threshold.",
                             "The lowest q-value value is", min(res_df_gene_ORA$qvalue),
                             ". Try setting the threshold larger."), 
                       type = "Warning")
        remove_modal_spinner()
        return()
      }
      
      message("EnrichRes")
      # Define the new enrichResult object
      enrichres <- new("enrichResult",
                       result = res_df_gene_ORA,  # The data frame of enrichment results
                       organism = "cpd",  # If analyzing KEGG compounds
                       keytype = "kegg",
                       ontology = "UNKNOWN",
                       gene = universe,
                       pAdjustMethod = pAdjustMethod,
                       qvalueCutoff = qvalueCutoff,
                       readable = FALSE)
      
      message("Enrichment results after new object:")
      print(class(enrichres))
      print(enrichres)
      
      
      plots <- bar_dot_plot(enrichres, title_name, top_x_features, color_con)
      
    } else {
      
      update_modal_spinner(
        session = session,
        text = "Module enrichment analysis in progress... Please be patient."
      )
      
      title_name <- "Module Enrichment Analysis"
      print(paste0("#---", title_name, " ", dataset_name, "---#"))
      
      res_moduleCentric_ORA <- enrichMKEGG(
        gene = sig_kegg_id,
        organism = "cpd",
        keyType = "kegg",
        pvalueCutoff = p_value_thresh,
        pAdjustMethod = pAdjustMethod,
        universe,
        minGSSize = minGSSize,
        qvalueCutoff = qvalueCutoff
      )
      
      res_df_module_ORA <- as.data.frame(res_moduleCentric_ORA)
      
      print("Enrich results:")
      print(head(res_df_module_ORA))
      
      # if res_df_module_ORA is empty then return nothing and let the user know
      if (nrow(res_df_module_ORA) == 0) {
        sendSweetAlert(session, "Error", "No significant results found during ORA.", type = "error")
        remove_modal_spinner()
        return()
      }
      
      # Convert rownames to a column
      res_df_module_ORA <- res_df_module_ORA %>%
        tibble::rownames_to_column(var = "Pathway_ID")
      
      # Extract the "Upregulated"/"Downregulated" status and store in a new column
      res_df_module_ORA <- res_df_module_ORA %>%
        mutate(Regulation = ifelse(grepl("^Upregulated", Pathway_ID), "Upregulated", "Downregulated"),
               Pathway_ID = gsub("^(Upregulated|Downregulated)\\.", "", Pathway_ID)) %>% # Remove label from Pathway_ID
        select(-Pathway_ID) %>%
        relocate(Regulation, .after = "ID")
      
      # Check the updated dataframe
      message("Enrichment results DF:")
      print(head(res_df_module_ORA))
      
      # Display message if all p.adjust values are larger than the threshold
      if (all(res_df_module_ORA$p.adjust > p_value_thresh)) {
        sendSweetAlert(session,
                       "Warning",
                       paste("All adjusted p-values are larger than the threshold.",
                             "The lowest adjusted p-value value is", min(res_df_module_ORA$p.adjust),
                             ". Try setting the threshold larger."), 
                       type = "Warning")
        remove_modal_spinner()
        return()
      }
      
      if (all(res_df_module_ORA$qvalue > qvalueCutoff)) {
        sendSweetAlert(session,
                       "Warning",
                       paste("All q-values are larger than the threshold.",
                             "The lowest q-value value is", min(res_df_module_ORA$qvalue),
                             ". Try setting the threshold larger."), 
                       type = "Warning")
        remove_modal_spinner()
        return()
      }
      
      # Define the new enrichResult object
      enrichres <- new("enrichResult",
                       result = res_df_module_ORA,  # The data frame of enrichment results
                       organism = "cpd",
                       keytype = "kegg",
                       ontology = "UNKNOWN",
                       gene = universe,
                       pAdjustMethod = pAdjustMethod,
                       qvalueCutoff = qvalueCutoff,
                       readable = FALSE)
      
      message("Enrichment results after new object:")
      print(class(enrichres))
      print(enrichres)
      
      plots <- bar_dot_plot(enrichres, title_name, top_x_features, color_con)
    }
    
    output$enrichment_barplot <- renderPlot({
      plots$bar
    })
    
    output$enrichment_dotplot <- renderPlot({
      plots$dot
    })
    
    # Extract sample names belonging to the selected group
    selected_group_samples <- rownames(seq[seq$group == group_of_interest & seq$labels == "Sample",])
    
    # Define preference order
    prefs <- c("refmet_name", "Original.annotation", "Original annotation", "Name", "name")
    prefs_class <- c("super_class", "main_class", "sub_class")
    
    filtered_kegg_data <- data %>%
      filter(!is.na(kegg_id), kegg_id != "", kegg_id != " ") %>%
      distinct(kegg_id, .keep_all = TRUE) %>%
      select(
        any_of(prefs), 
        kegg_id, 
        any_of(prefs_class),
        all_of(selected_group_samples)
      )
    
    enrichres_df <- as.data.frame(enrichres)
    
    title_sig <- paste0(title_name," ", dataset_name, " ")

    update_modal_spinner(
      session = session,
      text = "Plotting... Please be patient."
    )
    
    output$enrichment_cnetplot <- renderPlot({
      tryCatch({
        NetGraphPlot(enrichres_df, filtered_kegg_data, title = title_sig,
                               layout_option = input$layout_option,
                               node_size_mult = input$node_size_mult,
                               node_text_size = input$node_text_size,
                               edge_alpha = input$edge_alpha,
                               edge_width_scale = input$edge_width_scale,
                               node_color_by = input$node_color_by) 
      }, error = function(err) {
        plot.new()
        text(0.5, 0.5, "Plot failed to generate.\n
             No significant pathways detected.\n Please check your criteria or adjust the thresholds.\n.", 
             cex = 1.2, col = "red", adj = 0.5)
      })
    })
    
    output$enrichment_table <- renderDT({
      datatable(
        enrichres_df, 
        options = list(
          pageLength = 10,
          autoWidth = FALSE,
          scrollX = TRUE
        ),
        class = "display nowrap"
      )
    })
    
    message(sample(quotes, 1))
    remove_modal_spinner()
    
  })
  
  ####################
  # circular barplot #
  ####################
  observeEvent(input$run_circular_barplot, {
    # Ensure all required inputs are available.
    req(input$select_data_circular_barplot,
        input$top_x_cirbar,
        input$name_column_cirbar,
        input$group_column_cirbar,  
        input$group1_cirbar,
        input$group2_cirbar)
    
    if (!is.null(rv$activeFile)) {
      if (input$select_data_circular_barplot == "Unsaved data") {
        data <- rv$tmpData  # Use the temporary data
        seq <- rv$tmpSequence  # Use the temporary sequence
        dataset_name <- "Unsaved data"
      } else {
        # Get the index of the selected dataset
        sd <- which(rv$choices %in% input$select_data_circular_barplot)
        data <- rv$data[[sd]]  # Retrieve the selected dataset
        seq <- rv$sequence[[sd]]  # Retrieve the selected sequence
        dataset_name <- names(rv$data)[sd]  # Retrieve dataset name
      }
      
      # Retrieve input selections
      names_col   <- input$name_column_cirbar
      grouping    <- input$group_column_cirbar  # use the correct input id
      numerator      <- input$group1_cirbar
      denominator      <- input$group2_cirbar
      top_X <- input$top_x_cirbar
      feature <- input$feature_cirbar
      
      if (numerator == denominator) {
        sendSweetAlert(session, "Error", "Please select different groups for comparison.", type = "error")
        return()
      }
      
      seq_subset <- seq[seq[, "labels"] %in% c("Sample"), ]  # Restrict to "Sample" rows
      
      # if seq[,group] is numeric put group_ in front
      if (is.numeric(seq_subset[, "group"])) {
        seq_subset[, "group"] <- paste0("group_", seq_subset[, "group"])
        numerator <- paste0("group_", numerator)
        denominator <- paste0("group_", denominator)
      }
      
      # remove rows where data[, names_col] is empty or NA
      data <- data[!is.na(data[, names_col]) & data[, names_col] != "", ]
      
      data_subset <- data[, c(rownames(seq_subset)), drop = FALSE]  # Use row names of seq_subset to filter columns
      
      rownames(data_subset) <- make.unique(rep(data[, names_col], length.out = nrow(data_subset)))

      stat_results <- calculate_stats(data_subset, seq_subset, adjust.method = "BH")
      
      target_contrast   <- paste0(numerator, "_vs_", denominator)
      reversed_contrast <- paste0(denominator, "_vs_", numerator)
      
      # If  contrast is present in stat_results:
      if (target_contrast %in% stat_results$Contrast) {
        # Just subset
        sub_df <- subset(stat_results, Contrast == target_contrast)
      } else if (reversed_contrast %in% stat_results$Contrast) {
        # Subset, then flip the log2FC & FC
        sub_df <- subset(stat_results, Contrast == reversed_contrast)
        sub_df$log2FC <- -sub_df$log2FC
        sub_df$FC     <- 1 / sub_df$FC
        
        # Rename the contrast column to reflect the new direction
        sub_df$Contrast <- target_contrast
      } else {
        # No matching contrast found; handle how you like (warn user, or return empty)
        warning(
          paste0("No matching contrast found for '", numerator, " vs ", denominator, "'. ",
                 "Available contrasts are: ", paste(unique(stat_results$Contrast), collapse=", "))
        )
        sub_df <- data.frame()
      }
      
      # assign the sub_df a name for debugging
      cirbar_df_name <- paste0(dataset_name, "_cirbar_df")
      assign(cirbar_df_name, sub_df)
      
      # Define custom colors
      custom_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                         "#D55E00", "#CC79A7", "#999999", "#8E44AD", "#1ABC9C",
                         "#2ECC71", "#3498DB", "#F39C12", "#E74C3C", "#95A5A6",
                         "#34495E", "#FB9A99", "brown", "#2980B9", "#C0392B")
      
      # column bind with dplyr 
      plotting_data <- bind_cols(
        data %>% select(all_of(names_col),all_of(grouping)),
        sub_df %>% select(FC, log2FC, p.value, p.adj, AveExpr, t, B)
      )
      
      # for each column return the min and max value in a 2xncol(data) matrix
      min_max_values <- sapply(plotting_data[,3:ncol(plotting_data)], function(x) c(base::min(x, na.rm = TRUE), max(x, na.rm = TRUE)))
      rownames(min_max_values) <- c("min", "max")
      print(round(min_max_values, 2))
      
      plotting_data <- plotting_data %>%
        mutate(!!grouping := as.factor(.data[[grouping]])) %>%
        arrange(p.adj) %>% 
        # select top_X rows 
        slice_head(n = top_X) %>%
        arrange(.data[[grouping]],p.adj)
      
      # Add empty start and end rows
      # empty_start <- data.frame(matrix(0, nrow = 1, ncol = ncol(plotting_data)))
      # colnames(empty_start) <- colnames(plotting_data)
      # empty_start[1, 1:2] <- NA
      
      empty_end <- data.frame(matrix(0, nrow = 1, ncol = ncol(plotting_data)))
      colnames(empty_end) <- colnames(plotting_data)
      empty_end[1, 1:2] <- NA
      
      plotting_data <- rbind(plotting_data, empty_end)
      plotting_data <- plotting_data %>%
        mutate(id = row_number()) %>%
        relocate(id, .before = all_of(names_col)) # Reassign id after adding empty bars
      
      # Remove potential NA values in factor levels
      plotting_data[,grouping] <- factor(plotting_data[,grouping], exclude = NULL)
      
      # Prepare label data
      label_data <- plotting_data
      number_of_bar <- nrow(label_data)

      # Calculate angles and alignment
      label_data <- label_data %>%
        mutate(angle = 90 - 360 * (id - 0.5) / number_of_bar,
               hjust = ifelse(angle < -90, 1, 0),
               angle = ifelse(angle < -90, angle + 180, angle))
      
      valid_data <- plotting_data %>% filter(!is.na(.data[[feature]]))
      
      # Create 5 evenly spaced ticks from min to max
      tick_min <- base::min(plotting_data[[feature]], na.rm = TRUE)
      tick_max <- max(plotting_data[[feature]], na.rm = TRUE)
      ticks <- seq(tick_min, tick_max, length.out = 5)
      
      tick_1 <- ticks[1]  # same as min
      tick_2 <- ticks[2]  # 25% of the range
      tick_3 <- ticks[3]  # 50% of the range
      tick_4 <- ticks[4]  # 75% of the range
      tick_5 <- ticks[5]  # same as max
      
      # Generate the circular bar plot with correctly ordered
      cirbar_plot <- ggplot(plotting_data,
                            aes(x = as.factor(id),
                                y = .data[[feature]],
                                fill = .data[[grouping]])) +
        geom_bar(stat = "identity", alpha = 0.5) +
        
        # geom_segment(data = valid_data,
        #              aes(x = as.factor(id), xend = as.factor(id), y = tick_1, yend = tick_5),
        #              linetype = "dashed", color = "gray", linewidth = 0.3) +
        
        geom_segment(data = valid_data,
                     aes(x = base::min(id), xend = max(id), y = tick_1, yend = tick_1),
                     linetype = "dashed", color = "gray70", linewidth = 0.3) +
        geom_segment(data = valid_data,
                     aes(x = base::min(id), xend = max(id), y = tick_2, yend = tick_2),
                     linetype = "dashed", color = "gray70", linewidth = 0.3) +
        geom_segment(data = valid_data,
                     aes(x = base::min(id), xend = max(id), y = tick_3, yend = tick_3),
                     linetype = "dashed", color = "gray70", linewidth = 0.3) +
        geom_segment(data = valid_data,
                     aes(x = base::min(id), xend = max(id), y = tick_4, yend = tick_4),
                     linetype = "dashed", color = "gray70", linewidth = 0.3) +
        geom_segment(data = valid_data,
                     aes(x = base::min(id), xend = max(id), y = tick_5, yend = tick_5),
                     linetype = "dashed", color = "gray70", linewidth = 0.3) +
        
        annotate("text",
                 x = base::min(plotting_data$id),
                 y = c(tick_1, tick_2, tick_3, tick_4, tick_5),
                 label = c(as.character(round(tick_1, 2)),
                           as.character(round(tick_2, 2)),
                           as.character(round(tick_3, 2)),
                           as.character(round(tick_4, 2)),
                           as.character(round(tick_5, 2))),
                 color = "black", size = 5, angle = 0, fontface = "bold", hjust = 1) +
        
        ylim(base::min(valid_data[,feature]) - 0.25,
             max(valid_data[,feature])) +
        
        theme_minimal() +
        theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(0, 4), "cm")
        ) +
        coord_polar(start = 0) +
        scale_fill_manual(values = custom_colors, na.value = "gray90") +
        geom_text(data = label_data %>% filter(!is.na(.data[[names_col]])),
                  aes(x = as.factor(id),
                      y = tick_5,
                      label = .data[[names_col]],
                      hjust = hjust,
                      angle = angle),
                  color = "black",
                  fontface = "bold",
                  alpha = 1,
                  size = 3,
                  inherit.aes = FALSE)
      
      # Render the plotly plot
      output$circular_barplot <- renderPlot({
        cirbar_plot
      })
      
      # TODO: return plotting_data as DF to investigate the data used for the plot
      
    }
  })
  
  #################
  # Lipid Heatmap #
  #################
  # Values is used for message display before and after data process
  values <- reactiveValues(runProcessClicked = FALSE)
  
  # When bottom clicked in interface, all the following will be processed
  observeEvent(input$run_process, {
    values$runProcessClicked <- TRUE
    
    # Required data files are loaded
    sequence <- rv$sequence[[rv$activeFile]]
    data <- rv$data[[rv$activeFile]]
    
    # Capture the number of rows before filtering, used to compare with the number of rows after filtering
    number_of_rows_before <- nrow(data)
    
    
    ###############
    # Data cleaning
    ###############
    
    # Removes noise, keeping only the name and length (e.g., "CAR 14:1'CAR'[M+H]+" becomes "CAR 14:1")
    data[, 1] <- sapply(data[, 1], extract_pattern)
    
    # Apply the `reposition_ether_lipids` function to change the posistion of ether lipids (O- or P- prefixed lipids). e.g. LP(O-18:1) --> LP-O(18:1)
    data[, 1] <- sapply(data[, 1], reposition_ether_lipids)
    
    # Apply the `clean_lipid_name` function to remove content after semicolons inside parentheses
    data[, 1] <- sapply(data[, 1], clean_lipid_name)
    
    # Apply the `format_strings` function to ensure the lipid names are properly formatted, adding parentheses around numbers if necessary
    data[, 1] <- sapply(data[, 1], format_strings)
    
    # Used to show which data is being filtraed away from the data frame
    removed_data <<- remove_patterned_rows(data)
    
    # Function to filter rows based on the specified pattern, meaning removes any data that are not on X(C:D) format.
    data <- filter_data_by_pattern(data)
    
    # Before the if-block, initialize merged_data_info
    merged_data_info <- NULL
    
    
    # This will make it possible to switch between original data and merged data. OG data: using _1 _2 ... _n. Merged will sum the values of the "duplicated" data. 
    if (input$selected_dataset == "original") {
      data <- unique_compound_names(data)
      
      # Merge duplicates
    }  else if (input$selected_dataset == "merged") {
      
      # Creating info to display inside 'Table of Heatmap' about which lipids are merged. 
      merged_data_info <- merged_info_function(data)
      
      # Removes anything that are not part of the data of the samples and name. So it it possible to sum the rows. 
      data <- data[, sequence[ , 'labels'] %in% c("Name","Sample")]
      sequence <- sequence[sequence[ , 'labels'] %in% c("Name","Sample"), ]
      
      # Sums the duplicates (isoforms of the lipids)
      data <- merge_duplicates(data)
    }
    
    
    # Capture the number of rows after filtering
    number_of_rows_after <- nrow(data)
    
    # Calculate the number of rows removed
    rows_removed <- number_of_rows_before - number_of_rows_after
    output$rows_removed_text <- renderText({
      paste("Rows removed after data cleaning are:", rows_removed, ". The removal is be due to the names in the first column of the data file not being in the X(C:D) format. 
            Keep in mind, that the merged data will also count as a removed row, but they will not show up in the table down below.")
    })
    
    
    # The following is used in the tab: 'Lipid summary'.
    #grouped_samples <- process_lipid_data(sequence, data) # not used?
    output$groups_table <- renderTable({
      
      # Extract the 'Sample' labels and corresponding 'group' from 'sequence'
      sample_rows <- sequence[sequence$labels == "Sample", ]
      unique_groups <- unique(sample_rows$group)
      
      # Create the dataframe to be displayed as a table
      lipid_df_processed_data <- data.frame(
        Group = unique_groups,
        Samples = sapply(unique_groups, function(group) {
          sample_identifiers <- rownames(sample_rows)[sample_rows$group == group]
          paste(sample_identifiers, collapse = ", ")
        })
      )
      # Return the data frame to be rendered as a table
      lipid_df_processed_data
    })
    
    
    
    # Heatmap input selection  
    observeEvent(input$run_process, {
      processed_results <- process_lipid_data(sequence, data)
      grouped_data_frames <<- create_grouped_data_frames(sequence, data)
      
      compound_names <- data[[1]]  # Extract the first column which contains compound names
      
      # Assuming that each grouped data frame has rows in the same order as "data"
      for (i in seq_along(grouped_data_frames)) {
        grouped_data_frames[[i]] <- cbind(Compound_Name = compound_names, grouped_data_frames[[i]])
      }
      
      # Extract unique group names from sequence
      unique_group_names <- unique(sequence[sequence$labels == "Sample", "group"])
      
      # Check if lengths of group names and grouped data frames match
      if (length(unique_group_names) == length(grouped_data_frames)) {
        # Apply the actual group names from the sequence file
        names(grouped_data_frames) <- unique_group_names
      } else {
        # If there's a mismatch, fallback to naming with numbers as before
        names(grouped_data_frames) <- paste("Group", seq_along(grouped_data_frames))
      }
      
      # Render the UI for group selection.
      # Render the UI for group selection.
      output$select_group_ui_heatmap <- renderUI({
        column(
          title = "Select groups for Heatmap test",
          width = 12,
          tagList(
            selectInput("selected_group_for_numerator", "Select Group for numerator:",
                        choices = names(grouped_data_frames),
                        selected = names(grouped_data_frames)[1] # Default to the first group
            ),
            selectInput("selected_group_for_denominator", "Select Group for denominator:",
                        choices = names(grouped_data_frames),
                        selected = names(grouped_data_frames)[2] # Default to the first group
            )
          )
        )
      })
      
      
      
      
      # Create interactive table for selected numerator group. Displayed at the starting page of the 'Lipid Heatmap'
      output$numerator_group_table <- DT::renderDataTable({
        req(input$selected_group_for_numerator) 
        # Create a copy of the data for display purposes
        display_data <- grouped_data_frames[[input$selected_group_for_numerator]]
        
        DT::datatable(
          display_data,  
          options = list(scrollX = TRUE)  # Enable horizontal scrolling
        )
      })
      
      # Create interactive table for selected denominator group
      output$denominator_group_table <- DT::renderDataTable({
        req(input$selected_group_for_denominator)  
        display_data <- grouped_data_frames[[input$selected_group_for_denominator]]
        
        DT::datatable(
          display_data,  
          options = list(scrollX = TRUE)  
        )
      })
      
      
      # Interface of selections of lipids to display
      output$select_lipid_ui <- renderUI({
        # Extract the lipid names from first column of the file 'data'
        lipid_names <<- group_lipids_by_group(data)
        
        selectizeInput("selected_lipid", "Select lipid(s) to display:",
                       choices = c("All", unique(lipid_names$group)),
                       multiple = TRUE,
                       options = list(placeholder = 'Choose lipids...',
                                      onInitialize = I('function() { this.setValue(""); }')))
      })
      
      # Reactive expression to track the number of selected lipids or the "All" selection
      selected_lipid_count <- reactive({
        # If "All" is selected, we could set this to a value that causes the default text size to be used
        if ("All" %in% input$selected_lipid) {
          return(Inf)  # 'Inf' is used here as a flag for "All"
        } else {
          return(length(input$selected_lipid))
        }
      })
      
      
      
      reactiveP_value <- reactive({
        req(input$selected_group_for_numerator, input$selected_group_for_denominator)
        
        numerator_data <- grouped_data_frames[[input$selected_group_for_numerator]]
        denominator_data <- grouped_data_frames[[input$selected_group_for_denominator]]
        
        # Ensure there is data to work with
        if (nrow(numerator_data) == 0 || nrow(denominator_data) == 0) {
          return(NULL)
        }
        
        # Initialize a vector to store the p-values
        p_values <- numeric(nrow(numerator_data))
        
        # Loop through each row to perform the t-test
        for (i in 1:nrow(numerator_data)) {
          # Extract the numerical values for numerator and denominator, excluding the first column
          num_values <- numerator_data[i, -1]
          denom_values <- denominator_data[i, -1]
          
          # Check if data is constant or contains NA values
          if (length(unique(num_values)) == 1 || length(unique(denom_values)) == 1 ||
              any(is.na(num_values)) || any(is.na(denom_values))) {
            p_values[i] <- NA  # Assign NA or another appropriate value
          } else {
            # Perform the t-test
            t_test_result <- t.test(num_values, denom_values)
            p_values[i] <- t_test_result$p.value
          }
        }
        
        # Apply Benjamini-Hochberg correction to p-values
        adjusted_p_values <- p.adjust(p_values, method = "BH")
        
        # Create a new data frame with 'Compound_Name', 'p_value', and 'padj'
        p_value_data <- data.frame(
          Compound_Name = numerator_data$Compound_Name,  
          p_value = p_values,
          padj = adjusted_p_values
        )
        
        # Filter p-values on adjusted_p_values based on ui input
        p_value_data <- p_value_data[!is.na(p_value_data$padj) & p_value_data$padj < input$p_value_adj, ]
        return(p_value_data)
      })
      
      # Reactive expression to calculate logFC
      reactiveLogFC <- reactive({
        # The required data input for the data handling. 
        req(input$selected_group_for_numerator, input$selected_group_for_denominator)
        
        # Define data input, makes it more readable 
        numerator_data <- grouped_data_frames[[input$selected_group_for_numerator]]
        denominator_data <- grouped_data_frames[[input$selected_group_for_denominator]]
        
        # Removes the fir column of the data, as it is not needed for the calculation.
        numerator_data <- numerator_data[, -1]
        denominator_data <- denominator_data[, -1]
        
        numerator_data_means <- rowMeans(numerator_data, na.rm = TRUE) # Makes sure that the calculation is done even NA values are present. # "drop = FALSE" was removed to avoid the error message.
        denominator_data_means <- rowMeans(denominator_data, na.rm = TRUE) # "drop = FALSE" was removed to avoid the error message.
        numerator_data_means <- data.frame(numerator_data_means)
        denominator_data_means <- data.frame(denominator_data_means)
        
        # Extract the compound names, to add it to the data frame logFC, making sure they are sorted by compound names. 
        compound_names <- data[[1]]
        
        # Calculate logFC
        logFC_data <- log2((numerator_data_means + 1e-6) / (denominator_data_means + 1e-6))
        # Rename a single column
        colnames(logFC_data)[colnames(logFC_data) == "numerator_data_means"] <- "logFC"
        
        logFC <- data.frame(Compound_Name = compound_names, logFC = logFC_data)
        
        
        # Continue filtering based on lipid selection
        if (!"All" %in% input$selected_lipid) {
          logFC <- logFC[lipid_names$group %in% input$selected_lipid, ]
        }
        
        filtered_data <- logFC
        return(filtered_data) # Used for Heatmap display 
      })
      
      
      
      ##### Render UI for different thresholds within the app. 
      
      # Render UI for maximum p-value input
      output$p_value_max_ui <- renderUI({
        numericInput("p_value_max", 
                     "Maximum p-value:", 
                     value = 1, 
                     min = 0, 
                     step = 0.01)
      })
      
      # Render UI for maximum p-value input
      output$p_value_adj <- renderUI({
        numericInput("p_value_adj", 
                     "Maximum p-value_adj:", 
                     value = 1, 
                     min = 0, 
                     step = 0.01)
      })
      
      # Render UI for logFC input
      output$logFC_input_ui <- renderUI({
        tagList(
          numericInput("logFC_input", 
                       "Enter logFC threshold:", 
                       value = 0,
                       min = 0)
        )
      })
      
      # Add this in the UI section where other inputs are rendered
      output$min_lipids_per_class_ui <- renderUI({
        numericInput("min_lipids_per_class", 
                     "Minimum number of lipids per class:", 
                     value = 2, 
                     min = 1, 
                     step = 1)
      })
      
      
      
      #### Filtration within the app
      
      # Reactive expression to filter data based on p-value and logFC thresholds, plus the amount of lipids within their class.
      reactiveFilteredData <- reactive({
        logFC_data <- reactiveLogFC()  
        p_value_data <- reactiveP_value()
        
        filtered_data <- merge(logFC_data, p_value_data, by = "Compound_Name")
        
        # Apply initial filtering criteria
        filtered_data <- filtered_data %>%
          filter(!is.na(p_value) & p_value <= input$p_value_max) %>%
          filter(abs(logFC) >= input$logFC_input)
        
        # Check if filtered_data has rows after initial filtering
        if (nrow(filtered_data) == 0) {
          # No data to proceed with, return empty data frame
          return(filtered_data)
        }
        
        # Get lipid class mapping for all lipids in the original data
        names.mapping.all <- map_lipid_names(x = filtered_data[[1]])
        
        # Compute counts per class from the original data
        class_counts <- table(names.mapping.all$Class)
        class_counts_df <- as.data.frame(class_counts)
        colnames(class_counts_df) <- c("Class", "Count")
        
        # Set the threshold from user input
        min_lipids_per_class <- input$min_lipids_per_class
        
        # Get classes that meet the threshold
        classes_to_keep <- class_counts_df$Class[class_counts_df$Count >= min_lipids_per_class]
        
        # Map the class information to the filtered_data
        # Create a data frame of lipid names and class
        lipid_class_df <- data.frame(Compound_Name = filtered_data[[1]], Class = names.mapping.all$Class)
        
        # Merge the class information with filtered_data
        filtered_data <- merge(filtered_data, lipid_class_df, by = "Compound_Name", all.x = TRUE)
        
        # Now filter based on classes_to_keep
        filtered_data <- filtered_data[filtered_data$Class %in% classes_to_keep, ]
        
        # Check again if filtered_data has rows after class filtering
        if (nrow(filtered_data) == 0) {
          # No data to proceed with, return empty data frame
          return(filtered_data)
        }
        
        return(filtered_data)
      })
      
      
      # Warring message if no data meets the threshold, but does not show if 'all' lipids are selected.
      output$filteredDataWarning <- renderUI({
        filtered_data <- reactiveFilteredData()
        
        if (selected_lipid_count() == 0) {
          # No lipids selected, do not show any message
          return(NULL)
        } else if (nrow(filtered_data) == 0 && selected_lipid_count() >= 1) {
          # No data meets the filtering criteria and lipids are selected
          div(
            style = "color: red; font-weight: bold;",
            "Warning: No data meets the filtering criteria."
          )
        } else {
          # Data exists or no lipids are selected, no warning needed
          NULL
        }
      })
      
      
      # Heatmap plot loading
      
      # Reactive expression for heatmap plot data and height
      heatmap_plot_data <- reactive({
        # Used the data frame which is adjusted to the user input.
        filtered_data <- reactiveFilteredData()
        
        # Ensure the data is not NULL and has rows to plot
        req(nrow(filtered_data) > 0)
        
        # Map the lipid names (make sure it returns all necessary columns, including Lipid_Class)
        # This is a function from lipidomeR package. 
        names.mapping <- map_lipid_names(x = filtered_data$Compound_Name)
        
        # Calculate the number of unique classes
        num_classes <- length(unique(names.mapping$Class))
        
        # Calculate number of rows in the facets
        ncol_facets <- 3  # Adjust based on your facet_wrap setting
        num_rows <- ceiling(num_classes / ncol_facets)
        
        # Set base height per row
        height_per_row <- 300  # Adjust as needed to match bubble plot scaling
        
        # Calculate total plot height
        total_plot_height <- num_rows * height_per_row
        
        # Ensure minimum and maximum height limits
        total_plot_height <- max(total_plot_height, 600)   # Minimum height
        total_plot_height <- base::min(total_plot_height, 4000)  # Maximum height
        
        # Calculate logFC range dynamically
        logFC_range <- range(filtered_data$logFC, na.rm = TRUE)
        
        # Adjust fill.limits based on user input
        if (input$selected_logfc_sclae_bar == "Manual") {
          manual_limit <- input$logFC_scale_manual
          fill.limits <- c(-manual_limit, manual_limit)
        } else {
          # Use dynamic range
          fill.limits <- c(base::min(logFC_range), max(logFC_range))
        }
        
        heatmap_plot <- heatmap_lipidome(
          x = filtered_data[ , c("Compound_Name", "logFC")],
          names.mapping = names.mapping,
          class.facet = "wrap",
          x.names = "Compound_Name",
          fill.limits = fill.limits,  
          melt.value.name = "logFC",
          scales = "free"
        ) +
          scale_fill_gradient2(
            low = input$low_color,
            mid = input$mid_color,
            high = input$high_color,
            limits = fill.limits,  
            space = "Lab",
            name = "logFC",
            guide = guide_colorbar(
              barwidth = input$barwidth, # Adjust width of color bar
              barheight = input$barheight  # Adjust height of color bar
            )
          ) +
          facet_wrap(~ Class, scales = "free", ncol = ncol_facets) +
          theme(
            panel.background = element_rect(fill = input$panel_bg_color, color = "white"),
            strip.background = element_rect(fill = input$strip_bg_color, color = "white"),
            strip.text = element_text(color = input$strip_text_color, face = "bold", size = input$strip_text_size),
            axis.text.x = element_text(
              angle = input$axis_text_x_angle, 
              size = input$axis_text_x_size
            ),
            axis.text.y = element_text(size = input$axis_text_y_size),
            axis.title = element_text(size = input$axis_title_size),
            legend.title = element_text(size = input$legend_title_size),
            legend.text = element_text(size = input$legend_text_size),
            axis.title.x = element_text(size = input$axis_title_x_size),
            axis.title.y = element_text(size = input$axis_title_y_size),
            plot.title = element_text(size = input$plot_title_size, face = "bold")
          ) +
          labs(
            x = input$x_axis_label,
            y = input$y_axis_label,
            title = input$main_title, 
          ) 
        
        
        # Conditionally remove grid lines based on user input
        if (!input$show_grid) {
          heatmap_plot <- heatmap_plot +
            theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()
            )
        }
        
        # Return the plot and height
        list(plot = heatmap_plot, height = total_plot_height)
      })
      
      # Render the heatmap plot
      output$heatmapPlot <- renderPlot({
        heatmap_plot_data()$plot
      }, height = function() {
        heatmap_plot_data()$height
      })
      
      
      
      # IF user want to split the screen, the following will be shown. Heatmap and table side by side.
      output$visualization_ui <- renderUI({
        if (input$split_screen) {
          # Show both heatmap and table side by side
          fluidRow(
            column(width = 6,
                   div(style = "width: 100%; height: 800px; overflow-y: scroll;",  
                       withSpinner(plotOutput("heatmapPlot", width = "100%", height = "800px"))
                   )
            ),
            column(width = 6,
                   div(style = "width: 100%; height: 800px; overflow-y: scroll;",  
                       withSpinner(dataTableOutput("pValueTable_2"))
                   )
            )
          )
        } else {
          # Show only heatmap
          div(style = "width: 100%; height: 2000px; overflow-y: scroll;",  
              withSpinner(plotOutput("heatmapPlot", width = "100%", height = "2000px"))
          )
        }
      })
      
      
      
      # Reactive expression to calculate lipid group summary and total lipid count
      lipid_summary <- reactive({
        lipid_group_df <- as.data.frame(table(group_lipids_by_group(data)$group)) 
        colnames(lipid_group_df) <- c("Lipid group", "Count")
        
        # Calculate and add percentage of total lipids
        lipid_group_df$`Percentage of Total` <- round((lipid_group_df$Count / sum(lipid_group_df$Count)) * 100, 2)
        
        return(lipid_group_df)  # Return the dataframe for use in other parts
      })
      
      # Function to generate lipid group summary and percentage for the table
      output$lipid_group_count <- DT::renderDataTable({
        lipid_group_df <- lipid_summary()  # Use the reactive expression
        
        # Render the table
        DT::datatable(lipid_group_df, options = list(pageLength = 5, autoWidth = TRUE))
      })
      
      # Render total lipids as a text output
      output$lipid_total <- renderText({
        lipid_group_df <- lipid_summary()  # Use the reactive expression
        lipidsum <- sum(lipid_group_df$Count)
        paste("Total lipid count in table (After data cleaning, and are not affect by threshold set in 'Lipid Visualization'):", lipidsum)
      })
      
      
      # Assume merged_data_info has columns: Compound_Name, merged_molecules, count
      # returned by merged_info_function(data)
      
      # render table with "Compound_Name", "Original.annotation", "logFC", "p_value", "padj"
      output$pValueTable <- renderDataTable({
        filtered_data <- reactiveFilteredData()
        req(data)  # We must have data
        
        # If 'merged_data_info' is required only in "merged" scenario, don't use req(merged_data_info)
        # Instead, just check if it's not NULL before merging later.
        
        # Ensure 'Compound_Name' exists in 'data'
        if (!"Compound_Name" %in% colnames(data)) {
          data$Compound_Name <- data[, 1]  # Assuming the first column contains compound names
        }
        
        # Ensure 'Compound_Name' exists in 'filtered_data'
        if (!"Compound_Name" %in% colnames(filtered_data)) {
          stop("'Compound_Name' column not found in 'filtered_data'")
        }
        
        # Check if 'Original annotation' exists in 'data' and rename
        if ("Original annotation" %in% colnames(data)) {
          colnames(data)[colnames(data) == "Original annotation"] <- "Original.annotation"
        }
        
        # Merge filtered_data with Original.annotation if it exists
        if ("Original.annotation" %in% colnames(data)) {
          merged_data <- merge(
            filtered_data,
            data[, c("Compound_Name", "Original.annotation")],
            by = "Compound_Name",
            all.x = TRUE
          )
        } else {
          merged_data <- filtered_data
        }
        
        # Merge in the merged_data_info if available
        if (!is.null(merged_data_info) && "Compound_Name" %in% colnames(merged_data_info)) {
          merged_data <- merge(
            merged_data,
            merged_data_info,
            by = "Compound_Name",
            all.x = TRUE
          )
        }
        
        # Determine which columns to show:
        columns_to_show <- c("Compound_Name", "logFC", "p_value", "padj")
        
        if ("Original.annotation" %in% colnames(merged_data)) {
          columns_to_show <- c("Compound_Name", "Original.annotation", columns_to_show)
        }
        
        if ("merged_molecules" %in% colnames(merged_data)) {
          # Include merged_molecules and count columns
          columns_to_show <- c("Compound_Name", "Original.annotation", "merged_molecules", "count", "logFC", "p_value", "padj")
        }
        
        # Create the final table to show
        dataTableToShow <- merged_data[, intersect(columns_to_show, colnames(merged_data)), drop = FALSE]
        
        # Round numeric columns if they exist
        numeric_cols <- intersect(c("logFC", "p_value", "padj"), colnames(dataTableToShow))
        dataTableToShow[numeric_cols] <- lapply(dataTableToShow[numeric_cols], function(x) round(x, 5))
        
        # Render the DataTable
        datatable(dataTableToShow, options = list(pageLength = 10, scrollX = TRUE))
      })
      
      
      
      # This table is smaler and does not contain the 'Original annotation' column, is used to split the screen with table and heatmap
      output$pValueTable_2 <- renderDataTable({
        filtered_data <- reactiveFilteredData()
        
        
        dataTableToShow <- filtered_data[, c("Compound_Name", "logFC", "p_value", "padj")]
        
        # Round 'logFC' and 'p_value' to the desired number of decimal places
        dataTableToShow$logFC <- round(dataTableToShow$logFC, 5)      # 5 decimal places for logFC
        dataTableToShow$p_value <- round(dataTableToShow$p_value, 5)  # 5 decimal places for p-value
        dataTableToShow$padj <- round(dataTableToShow$padj, 5)  # 5 decimal places for p-value
        
        
        # Render the selected data in a DataTable
        datatable(dataTableToShow, options = list(pageLength = 10, scrollX = TRUE))
      })
      
      
      
      # Observe the action button to open the modal dialog
      observeEvent(input$download_heatmap_btn, {
        showModal(modalDialog(
          title = "Download Heatmap Image",
          selectInput("modal_image_format", "Select Image Format", choices = c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg")),
          numericInput("modal_image_dpi", "Image Resolution (DPI)", value = 300, min = 72, step = 72),
          footer = tagList(
            modalButton("Cancel"),
            downloadButton("modal_download_heatmap", "Download")
          )
        ))
      })
      
      
      
      
      # Reactive expression to calculate plot dimensions
      plot_dimensions <- reactive({
        # Take the input from user in interface and change p-value and logFC
        filtered_data <- reactiveFilteredData()
        
        # Ensure the data is not NULL and has rows to plot
        req(nrow(filtered_data) > 0)
        
        # Map the lipid names (make sure it returns all necessary columns, including Lipid_Class)
        names.mapping <- map_lipid_names(x = filtered_data$Compound_Name)
        
        # Calculate the number of unique classes
        num_classes <- length(unique(names.mapping$Class))
        
        # Calculate number of rows in the facets
        ncol_facets <- 3  # Adjust based on your facet_wrap setting
        num_rows <- ceiling(num_classes / ncol_facets)
        
        # Set base dimensions per row/column
        height_per_row <- 3  # in inches
        width_per_col <- 4   # in inches
        
        # Calculate total plot dimensions
        total_height <- num_rows * height_per_row
        total_width <- ncol_facets * width_per_col
        
        # Ensure minimum and maximum limits
        total_height <- max(total_height, 6)   # Minimum height in inches
        total_height <- base::min(total_height, 40)  # Maximum height in inches
        total_width <- max(total_width, 8)     # Minimum width in inches
        total_width <- base::min(total_width, 40)    # Maximum width in inches
        
        # Return the dimensions
        list(height = total_height, width = total_width)
      })
      
      
      
      # Download handler for the heatmap plot from the modal dialog
      output$modal_download_heatmap <- downloadHandler(
        filename = function() {
          paste("heatmap_plot_", Sys.Date(), ".", input$modal_image_format, sep = "")
        },
        content = function(file) {
          # Generate the plot
          heatmap_plot <- heatmap_plot_data()$plot
          
          # Get plot dimensions
          dims <- plot_dimensions()
          total_height <- dims$height  # in inches
          total_width <- dims$width    # in inches
          
          # Save the plot with user-specified format and resolution
          ggsave(
            filename = file,
            plot = heatmap_plot,
            device = input$modal_image_format,
            width = total_width,    # Width in inches
            height = total_height,  # Height in inches
            units = "in",
            dpi = input$modal_image_dpi
          )
          
          # Close the modal dialog after download
          removeModal()
        }
      )
      
      
      
      # Message shown when hovering over Original data and merged data.
      observe({
        addTooltip(session, "selected_dataset", 
                   "Choose 'Original Data' to work with the data as it was initially collected. Select 'Merged Data' for a combined dataset.", 
                   placement = "bottem", 
                   trigger = "hover")
      })
      
      # Message shown when hovering over logFC. 
      observe({
        addTooltip(session, "logFC_input_ui", 
                   "Displays lipids where the absolute value of logFC is greater than or equal to the threshold. For example, entering '1' will include lipids with logFC ≥ 1 or logFC ≤ -1.", 
                   placement = "bottom", 
                   trigger = "hover")
      })
      
      
      # Tooltip for p-value max input
      observe({
        addTooltip(session, "p_value_max_ui", 
                   "Displays lipids where the p-value is less than or equal to the threshold.", 
                   placement = "bottom", 
                   trigger = "hover")
      })
      
      # Tooltip for p-value adjusted input
      observe({
        addTooltip(session, "p_value_adj", 
                   "Displays lipids where the adjusted p-value is less than or equal to the threshold.", 
                   placement = "bottom", 
                   trigger = "hover")
      })
      
      # Tooltip for minimum lipids per class input
      observe({
        addTooltip(session, "min_lipids_per_class_ui", 
                   "Includes lipid classes that have at least the specified minimum number of lipids.", 
                   placement = "bottom", 
                   trigger = "hover")
      })
      
      # Tooltip for logFC scale bar input
      observe({
        addTooltip(session, "selected_logfc_sclae_bar", 
                   "Select 'Dynamic' to use the dynamic range of logFC values, 
                   min and max of the logFC will then be set as the scale bar. 
                   Select 'Manual' to specify a custom range.", 
                   placement = "bottom", 
                   trigger = "hover")
      })
      
      # Shows which groups is selected for the logFC calculation in the 'Heatmap visualization' tab.
      output$selected_groups_heatmap_text <- renderUI({
        req(input$selected_group_for_numerator, input$selected_group_for_denominator)
        withMathJax(HTML(paste0(
          "<p>Data being compared is:</p>",
          "$$\\log_2\\left( \\frac{\\text{mean of group ", input$selected_group_for_numerator,
          "} + 10^{-6}}{\\text{mean of group ", input$selected_group_for_denominator,
          "} + 10^{-6}} \\right)$$"
        )))
      })
      
      
      
      
      
    })
  }) # This finishes the first 'observeEvent' when 'Run data processing' is clicked
  
  # Outside of the observeEvent, based on whether runProcessClicked is TRUE or FALSE, the message display will be placed on this: 
  # For the first message, which is placed in the 'Lipid Heatmap' tab.
  output$table_message_1 <- renderUI({
    if (!values$runProcessClicked) {
      HTML('<p>Make sure sequences file is uploaded, when uploaded: Press "Run Data Processing" to get a display of data</p>')
    }
  })
  
  # For the second message, which is placed in the 'Table' tab.
  output$table_message_2 <- renderUI({
    if (!values$runProcessClicked) {
      HTML('<p>Make sure sequences file is uploaded, when uploaded: Press "Run Data Processing" to get a display of data</p>')
    }
  })
  
  # Outside of the observeEvent, so the message both are shown before and after runProcessClicked is clicked. 
  observe({
    addTooltip(session, "selected_dataset", 
               "Choose 'Original Data' to work with the data as it was initially collected, all sum isoforms is within the data. Select 'Merged Data' for a combined dataset, meaning isoforms lipids are summed togehter.", 
               placement = "bottom", 
               trigger = "hover")
  })
  
  # User guide inside 'Heatmap'
  observeEvent(input$show_lipid_info, {
    showModal(modalDialog(
      title = "Lipid Summary",
      textOutput("lipid_total"),  # Display the total number of lipids
      
      
      dataTableOutput("lipid_group_count"),  # Lipid group count table
      
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$show_lipid_cal, {
    showModal(modalDialog(
      title = "Lipid Calculation",
      div(
        tags$b("logFC: "), 
        withMathJax("$$\\log_2\\left( \\frac{\\text{mean of group numerator} + 10^{-6}}{\\text{mean of group denominator} + 10^{-6}} \\right)$$")
      ),
      div(
        tags$b("logFC Explanation: "), 
        "+10^-6 is added to avoid division by zero. The logFC is calculated for each lipid, 
      comparing the mean of the numerator group to the mean of the denominator group, 
      ensuring all sample values are taken into account."
      ),
      div(
        tags$b("p-value: "), 
        "The p-value is calculated using Welch’s t-test, comparing the raw data between the two groups for each lipid. 
      The data used is not filtered by p-value or logFC thresholds beforehand, ensuring an unbiased comparison."
      ),
      div(
        tags$b("Adjusted p-value (p-adj): "), 
        "P-values are adjusted for multiple comparisons using the Benjamini-Hochberg (BH) method. 
      This controls the false discovery rate (FDR), reducing the likelihood of false positives when testing multiple lipids simultaneously."
      ),
      
      div(
        tags$b("Packages Used: "),
        "This analysis utilizes the 'lipidomeR' package version 0.1.2."
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Render the datatable of removed_data
  output$lipid_remove_table <- DT::renderDataTable({
    req(input$show_lipid_remove) # Ensures button has been clicked
    DT::datatable(
      removed_data,
      options = list(pageLength = 5, autoWidth = TRUE, scrollX = TRUE)
    )
  })
  
  # Show the modal dialog containing the table when button is pressed
  observeEvent(input$show_lipid_remove, {
    showModal(modalDialog(
      title = "Lipid filtration summeray",
      textOutput("rows_removed_text"),
      dataTableOutput("lipid_remove_table"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$df_explain, {
    showModal(modalDialog(
      title = "Lipid Data Frames",
      div(
        tags$b("Original vs Merged Data"),
        tags$p("In 'Original data', every molecular feature is shown separately. This includes isobaric species—molecules that share the same mass but differ in structure. Lipidomics datasets often contain thousands of lipids, where even subtle differences (such as double bond positions) matter."),
        tags$p("For example, consider the lipids:"),
        tags$ul(
          tags$li("DG(18:1(9Z)/20:3(8Z,11Z,14Z)/0:0)[iso2]"),
          tags$li("DG(18:0/20:4(5Z,8Z,11Z,14Z)/0:0)[iso2]"),
          tags$p("These lipids share the same sum composition (DG(38:4)) but differ in structure and abundance.")
        ),
        
        tags$p("Although both have the same mass (m/z 644.537975), their structures and abundances differ. In 'Original data', these would appear as DG_1(38:4) and DG_2(38:4), maintaining each unique identity since the program only supports the X(C:D) notation."),
        
        tags$p("In contrast,'Merged data'combines these isobaric species into a single entry (DG(38:4)), summing their abundances and removing the structural distinctions that existed in the original data."),
        
        tags$p("References:"),
        tags$ul(
          tags$li(tags$a(href = "https://www.lipidmaps.org/databases/lmsd/LMGL02010110", "DG 18:1_20:3")),
          tags$li(tags$a(href = "https://www.lipidmaps.org/databases/lmsd/LMGL02010111", "DG 18:0_20:4"))
        ),
        tags$p("To learn more about these formats, please visit the ## WILL BE ADDED I MAIN_SERVER",
               tags$a(href = "http://documentation_link_here_(will_come_in_main_server", "documentation page"), "."),
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$lipid_contact, {
    showModal(
      modalDialog(
        title = "Contact Information",
        div(
          tags$p(tags$b("Lipid Heatmap")),
          tags$p("This program is written in R and Shiny, and is designed to help researchers and students to analyze lipidomics data."),
          tags$p("It is a part of the Bachelor's thesis project at the University of Southern Denmark, and is developed by Jakob R. Hartvig. BS.c. in Biomedicine"), 
          tags$p("Multiple datasets have been tested—both newly generated and previously published—to ensure that the program works as intended."),
          
          tags$p("Feel free to use, modify, and distribute this tool under the terms of its open-source license. If you publish results obtained with this tool, 
                 a citation or acknowledgment would be greatly appreciated."), 
          tags$p("The program is open-source and can be found on Metabolink GitHub:"),
          tags$p(tags$b("If you have any questions, bug finds or concerns, please get in touch:")),
          tags$ul(
            tags$li("Jakob R. Hartvig"),
            tags$li("Email: ", tags$a(href = "mailto:jahar21@student.sdu.dk", "jahar21@student.sdu.dk")),
            tags$li("If there is no response within two to three business days, please try: ",
                    tags$a(href = "mailto:jakobhartvigprivat@gmail.com", "jakobhartvigprivat@gmail.com"),
            ),
            tags$p("I welcome suggestions, feedback, and contributions from the community!"),
            tags$p("Thank you for using Lipid Heatmap!"),
            tags$p("Best regards, Jakob R. Hartvig"),
            
            
            
            tags$svg(
              width = "120px", height = "140px", viewBox = "0 0 120 140",
              style = "display:block; margin:auto;",
              
              # Define a linear gradient for the droplet (from bright yellow at the top to warm pink at the bottom)
              tags$defs(
                tags$linearGradient(
                  id = "funGrad", x1 = "0%", y1 = "0%", x2 = "0%", y2 = "100%",
                  tags$stop(offset = "0%", style = "stop-color:#fdf21c;stop-opacity:1"), # Bright yellow top
                  tags$stop(offset = "100%", style = "stop-color:#ff7d7d;stop-opacity:1") # Warm pink bottom
                )
              ),
              
              # Draw a droplet shape
              tags$path(
                d = "M60,10 
         C 20,10 0,50 60,120 
         C120,50 100,10 60,10 Z",
                fill = "url(#funGrad)",
                stroke = "#333",
                `stroke-width` = 2
              ),
              
              # Add two eyes (small black circles)
              tags$circle(cx="50", cy="50", r="4", fill="#333"),
              tags$circle(cx="70", cy="50", r="4", fill="#333"),
              
              # Add a smiling mouth using a path (a simple arc)
              tags$path(
                d = "M45,65 Q60,80 75,65", 
                fill = "transparent",
                stroke = "#333",
                `stroke-width` = 3,
                `stroke-linecap` = "round"
              ),
              
              # Add the text "Lipid" below the droplet in a bright color
              tags$text(
                "LIPIDS",
                x = "50%", y = "131",
                `text-anchor` = "middle",
                fill = "#ff4f4f",
                style = "font-family:sans-serif; font-size:22px; font-weight:bold;"
              )
            )
            
            
            
            
            
            
          ),
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  }) #### End of all lipid heatmap code for server.r 
  
  ##############
  # Odds Ratio #
  ##############
  
  observeEvent({
    list(input$select_OR_data, input$OR_main_label)
  }, {
    req(input$select_OR_data)
    
    if (!is.null(rv$activeFile)) {
      # Retrieve the dataset and sequence based on selection
      if (input$select_OR_data == "Unsaved data") {
        data <- rv$tmpData    # Use temporary data
        seq  <- rv$tmpSequence
      } else {
        # Find the index of the selected dataset
        sd   <- which(rv$choices %in% input$select_OR_data)
        data <- rv$data[[sd]]
        seq  <- rv$sequence[[sd]]
      }
      
      # Extract column names from the selected dataset
      data_colnames <- colnames(data)
      
      # Define default values based on data columns
      main_default    <- if ("super_class" %in% data_colnames) "super_class" else if ("main_class" %in% data_colnames) "main_class" else data_colnames[1]
      sub_default     <- if ("sub_class" %in% data_colnames) "sub_class" else if ("Lipid.Abbreviation" %in% data_colnames) "Lipid.Abbreviation" else data_colnames[1]
      feature_default <- if ("Original.annotation" %in% data_colnames) "Original.annotation" else if ("Species.Name" %in% data_colnames) "Species.Name" else data_colnames[1]
      
      # Update the select inputs using the data columns;
      updateSelectInput(session, "OR_main_label",
                        choices  = data_colnames,
                        selected = if (input$OR_main_label %in% data_colnames) input$OR_main_label else main_default)
      
      updateSelectInput(session, "OR_sub_label",
                        choices  = data_colnames,
                        selected = if (input$OR_sub_label %in% data_colnames) input$OR_sub_label else sub_default)
      
      updateSelectInput(session, "OR_feature_label",
                        choices  = data_colnames,
                        selected = if (input$OR_feature_label %in% data_colnames) input$OR_feature_label else feature_default)
      
      # Determine which main label to use for computing groups:
      selected_main_label <- if (input$OR_main_label %in% data_colnames) {
        input$OR_main_label
      } else {
        main_default
      }
      
      # Extract unique groups from the determined main label column and remove empty strings
      groups <- sort(unique(data[[selected_main_label]]))
      groups <- groups[nzchar(groups)]
      
      # Create a choices vector with an "All" option as the first element.
      choices <- c("All", groups)
      
      # Update the selectize input for groups with "All" as the default selection.
      updateSelectizeInput(session, "selected_groups_OR",
                           choices  = choices,
                           selected = "All",
                           server   = TRUE)
    }
  })
  
  # OR plot
  observeEvent(input$run_OR_plot, {
    req(input$select_OR_data,
        input$group1_OR,
        input$group2_OR,
        input$OR_main_label,
        input$OR_sub_label,
        input$OR_feature_label,
        input$selected_groups_OR)
    
    if (!is.null(rv$activeFile)) {
      if (input$select_OR_data == "Unsaved data") {
        data <- rv$tmpData  
        seq  <- rv$tmpSequence     
        dataset_name <- "Unsaved data"
      } else {
        sd <- which(rv$choices %in% input$select_OR_data)
        data <- rv$data[[sd]]      
        seq  <- rv$sequence[[sd]]  
        dataset_name <- names(rv$data)[sd]
      }
      
      seq <- seq[seq$labels == "Sample", ]
      
      group1 <- input$group1_OR
      group2 <- input$group2_OR
      Main_label <- input$OR_main_label        
      Sub_label <- input$OR_sub_label          
      Feature_label <- input$OR_feature_label  
      Selected_Groups <- input$selected_groups_OR
      
      # When processing the selected groups:
      Selected_Groups <- if ("All" %in% input$selected_groups_OR) {
        data[[Main_label]] %>% unique() %>% sort()
      } else {
        input$selected_groups_OR
      }
      
      message("Inside Odds ratio")
      message("Group 1: ", group1)
      message("Group 2: ", group2)
      message("Main label: ", Main_label)
      message("Sub label: ", Sub_label)
      message("Feature label: ", Feature_label)
      message("Selected features: ", Selected_Groups)
      
      # Main_label, Sub_label and Feature_label must not be identical 
      if (Main_label == Sub_label || Main_label == Feature_label || Sub_label == Feature_label) {
        sendSweetAlert(session, "Error", "Main label, Sub label and Feature label must not be identical.", type = "error")
        return()
      }
      
      # Check if the desired columns are present
      desired_cols <- c(Main_label, Sub_label, Feature_label)
      if (!all(desired_cols %in% colnames(data))) {
        sendSweetAlert(session, "Error", "The data does not contain all the necessary columns.", type = "error")
        return()
      }
      
      
      sample_cols <- rownames(seq)
      # Dynamically select columns using the input strings
      df_for_or <- data %>%
        select(!!sym(Main_label), !!sym(Sub_label), !!sym(Feature_label), all_of(sample_cols))
      
      # Remove rows where the Main_label column does not have the Selected_Groups 
      df_for_or <- df_for_or %>% filter(!!sym(Main_label) %in% Selected_Groups)
      
      print(head(df_for_or))
      
      # Remove duplicate rows based on the feature column
      # df_for_or <- df_for_or[!duplicated(df_for_or[[Feature_label]]), ]
      
      # Make the feature names unique 
      df_for_or[[Feature_label]] <- make.unique(as.character(df_for_or[[Feature_label]]))
      
      # Pivot the data from wide to long format using the sample columns
      df_long <- df_for_or %>%
        pivot_longer(
          cols = all_of(sample_cols),
          names_to = "sample",
          values_to = "intensity"
        )
      
      # Make the sequence data frame available for joining by converting rownames to a "sample" column
      seq <- seq %>% rownames_to_column(var = "sample")
      
      # Join the sample information from the sequence to the long-format data
      df_long <- df_long %>%
        left_join(seq, by = "sample") %>%
        select(-batch, -order, -time, -paired, -amount)
      
      # Remove rows with NA intensity and apply log2 transformation
      df_long <- df_long %>% filter(!is.na(intensity))
      # df_long$intensity <- log2(df_long$intensity)
      df_long <- df_long %>% filter(!is.na(intensity), !is.infinite(intensity))
      
      # Filter the long data to only include the two groups of interest
      df_long <- df_long %>%
        filter(group %in% c(group1, group2)) %>%
        mutate(group = factor(group, levels = c(group1, group2)))
      
      # Calculate odds ratios for each feature (using the feature column provided by input)
      odds_ratios <- df_long %>%
        group_by(feature = .data[[Feature_label]]) %>%
        do({
          mod <- glm(group ~ intensity, data = ., family = binomial) # Logistic regression
          broom::tidy(mod) # Extract coefficients
        }) %>%
        filter(term == "intensity") %>%
        mutate(odds_ratio = estimate,
               lower_ci   = (estimate - 1.96 * std.error),
               upper_ci   = (estimate + 1.96 * std.error)) #%>% 
        # mutate(odds_ratio = exp(estimate),
        #        lower_ci   = exp(estimate - 1.96 * std.error),
        #        upper_ci   = exp(estimate + 1.96 * std.error))
      
      # Join the odds ratios back with the original features for label info
      odds_ratios <- odds_ratios %>%
        left_join(df_for_or, by = c("feature" = Feature_label)) %>%
        select(feature, odds_ratio, lower_ci, upper_ci,
               main = !!sym(Main_label),
               sub  = !!sym(Sub_label))
      
      # Determine significance based on the confidence interval
      odds_ratios <- odds_ratios %>%
        mutate(Significance = case_when(
          lower_ci > 1 ~ "Positive",
          upper_ci < 1 ~ "Negative",
          TRUE         ~ "Not significant"
        ))
      
      print(head(odds_ratios))
      message("Max odds ratio:")
      print(max(odds_ratios$odds_ratio))
      message("Min CI:")
      print(min(odds_ratios$lower_ci))
      message("Max CI:")
      print(max(odds_ratios$upper_ci))
      
      # Render the plot
      output$OR_plot <- renderPlot({
        ggplot(odds_ratios, aes(x = odds_ratio, y = sub, color = Significance)) +
          geom_point(position = position_dodge(width = 0.5), size = 4) +
          geom_quasirandom() +
          geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci),
                         height = 0, alpha = 0.5,
                         position = position_dodge(width = 0.5),
                         linetype = 3, linewidth = 2) +
          geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
          scale_x_log10() +
          scale_color_manual(values = c("Negative" = "#1f78b4",
                                        "Not significant" = "gray60",
                                        "Positive" = "#e31a1c")) +
          # Facet by the main label (e.g. super_class or main_class) – labels are now on the left
          facet_grid(main ~ ., scales = "free_y", space = "free_y", switch = "y") +
          theme_bw(base_size = 12) +
          theme(
            strip.placement = "outside",
            strip.text.y.left = element_text(angle = 90),
            legend.position = "right"
          ) +
          labs(
            x = "Odds Ratio (95% CI)",
            y = NULL,
            color = "Direction",
            title = "Odds Ratios by Lipid Class"
          )
      })
      
      # Create a table of significant lipids
      sig_results <- odds_ratios %>% filter(Significance != "Not significant")
      sig_results <- sig_results %>%
        relocate(c(odds_ratio, lower_ci, upper_ci), .after = sub)
      
      output$OR_table <- DT::renderDataTable({
        DT::datatable(sig_results,
                  options = list(
                    scrollX = TRUE,
                    pageLength = 20
                  ))
      })
      
      message(sample(quotes, 1))
    }
  })
  
  ###################
  # Summary of data #
  ###################
  observe({ 
    if (!is.null(rv$activeFile)) {
      # Get the data and sequence
      seq <- rv$sequence[[rv$activeFile]]
      dat <- rv$data[[rv$activeFile]]
      blank_mv <- sum(is.na(dat[seq[, 1] %in% "Blank"])) +
        sum(dat[seq[, 1] %in% "Blank"] == 0, na.rm = TRUE)
      qc_mv <- sum(is.na(dat[seq[, 1] %in% "QC"])) +
        sum(dat[seq[, 1] %in% "QC"] == 0, na.rm = TRUE)
      sample_mv <- sum(is.na(dat[seq[, 1] %in% "Sample"])) +
        sum(dat[seq[, 1] %in% "Sample"] == 0, na.rm = TRUE)
      
      sdata <- dat[seq[, 1] %in% "Sample"]
      sclass <- seq[seq[, 1] %in% "Sample", ][, 4]
      
      
      # Get the missing values
      blank_mv <- sum(is.na(dat[seq[, "labels"] %in% "Blank"])) +
        sum(dat[seq[, "labels"] %in% "Blank"] == 0, na.rm = TRUE)
      qc_mv <- sum(is.na(dat[seq[, "labels"] %in% "QC"])) +
        sum(dat[seq[, "labels"] %in% "QC"] == 0, na.rm = TRUE)
      sample_mv <- sum(is.na(dat[seq[, "labels"] %in% "Sample"])) +
        sum(dat[seq[, "labels"] %in% "Sample"] == 0, na.rm = TRUE)
      
      # Subset the data for samples
      data_subset <- dat[seq[, "labels"] %in% "Sample"]
      sclass <- seq[seq[, "labels"] %in% "Sample", ][, "group"]
      # Calculate CV
      if (sum(seq$labels %in% "QC") > 0) {
        qccv <- paste0("CV in QC samples: ",
                       round(cvmean(dat[seq[, 1] %in% "QC"]), 2), "</br>")
      } else {
        qccv <- "No QC in dataset </br>"
      }
      # Calculate CV in groups
      if (sum(!is.na(sclass)) > 0) {
        classcv <- sapply(sort(unique(sclass)), function(x) {
          round(cvmean(sdata[sclass %in% x]), 2)
        })
        classcv <- sapply(seq_along(classcv), function(x) {
          paste0("CV in group ", sort(unique(sclass))[x], ": ", classcv[x], "</br>")
        })
      } else {
        classcv <- NULL
      }
      # Combine the text
      text <- c(qccv, classcv)
      output$title <- renderText({
        HTML("<h3>", names(rv$data)[rv$activeFile], "</h3>")
      })
      # Update the information UI
      output$info_ui <- renderUI({
        HTML(nrow(dat), " features.<br>",
             ncol(dat[seq[, 1] %in% "Sample"]), " samples.<br>",
             ncol(dat[seq[, 1] %in% "QC"]), " QC samples.<br>",
             ncol(dat[seq[, 1] %in% "Blank"]), " Blank samples.<br>", "<br>",
             sample_mv, " missing values in Samples<br>",
             qc_mv, " missing values in QC samples<br>",
             blank_mv, " missing values in Blank samples<br><br>")
      })
      # Update the CV information UI
      output$cvinfo_ui <- renderUI({
        HTML(text)
      })
      
      # Update statistics select input options
      groups <- na.omit(seq[, 'group'])
      time <- na.omit(seq[, 'time'])
      
      group_inputs <- c("group1", "group2",
                        "group1_time", "group2_time",
                        "group1_polystest", "group2_polystest")
      
      # Update select input options where groups are needed
      groups <- unique(seq$group[seq$labels == "Sample" & seq$group != ""])
      time <- unique(seq$time[seq$labels == "Sample" & seq$time != ""])
      
      group_inputs <- c("group1", "group2",
                        "group1_time", "group2_time",
                        "group1_polystest", "group2_polystest",
                        "group1_enrichment","group2_enrichment",
                        "group1_cirbar", "group2_cirbar",
                        "group1_vol", "group2_vol",
                        "group1_OR", "group2_OR")
      
      for (x in group_inputs) {
        # Default selected_value to NULL
        selected_value <- NULL
        
        # Check if input corresponds to 'group2' and groups has at least 2 elements
        if (grepl("group2", x) && length(groups) > 1) {
          selected_value <- groups[2]  # Use the second group
        }
        
        # Update selectInput with appropriate choices and default selection
        updateSelectInput(session, x, label = NULL, choices = groups, selected = selected_value)
      }
      
      # Update time select input options
      time_inputs <- c("time1_time", "time2_time",
                       "time1_polystest", "time2_polystest")
      
      for (x in time_inputs) {
        updateSelectInput(session, x, label = NULL, choices = time, selected = "")
      }
      
      features <- c("FC","log2FC","p.value","p.adj","AveExpr","t","B")
      feature_inputs <- c("feature_cirbar")
      
      for (x in feature_inputs) {
        updateSelectInput(session, x, label = NULL, choices = features, selected = "log2FC")
      }
      
      columns <- colnames(dat)
      column_inputs <- c("identifier_column_refmet",
                         "annotation_column_merge",
                         "name_column_lipids",
                         "name_column_annotate", 
                         "name_column_cirbar",
                         "group_column_cirbar",
                         "group_column_heatmap")
      
      for (x in column_inputs) {
        if (grepl("identifier_column_refmet", x)) {
          default_val <- if ("Structure" %in% columns) "Structure" else ""
          updateSelectInput(session, x, label = NULL, choices = columns, selected = default_val)
        } else if (grepl("group_column_cirbar", x)) {
          default_val <- if ("super_class" %in% columns) {
            "super_class"
          } else if ("lipid_class" %in% columns) {
            "lipid_class"
          } else {
            ""
          }
          updateSelectInput(session, x, label = NULL, choices = columns, selected = default_val)
          
        } else if (grepl("name_column_lipids", x)) {
          default_val <- if ("Original annotation" %in% columns) {
            "Original annotation"
          } else if ("Original.annotation" %in% columns) {
            "Original.annotation"
          } else if ("Name" %in% columns) {
            "Name"
          } else {
            ""
          }
          updateSelectInput(session, x, label = NULL, choices = columns, selected = default_val)
          
        } else if (grepl("name_column_annotate", x)) {
          default_val <- if ("Original annotation" %in% columns) {
            "Original annotation"
          } else if ("Original.annotation" %in% columns) {
            "Original.annotation"
          } else if ("Name" %in% columns) {
            "Name"
          } else {
            ""
          }
          updateSelectInput(session, x, label = NULL, choices = columns, selected = default_val)
          
        } else if (grepl("annotation_column_merge", x)) {
          default_val <- if ("ID level" %in% columns) {
            "ID level"
          } else {
            "Name"
          }
          updateSelectInput(session, x, label = NULL, choices = columns, selected = default_val)
          
        } else {
          default_val <- if ("Original annotation" %in% columns) {
            "Original annotation"
          } else if ("Name" %in% columns) {
            "Name"
          } else {
            ""
          }
          updateSelectInput(session, x, label = NULL, choices = columns, selected = default_val)
        }
      }
    }
  })
  
  #################
  # Normalization #
  #################
  
  observeEvent(input$normalize, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if(input$normMethod == "QC (PQN)" & sum(rv$sequence[[rv$activeFile]][, 1] %in% "QC") == 0) {
      sendSweetAlert(session = session, title = "Error", text = "No QC samples in dataset.", type = "error")
    } else if(input$normMethod == "Sample amount" & sum(complete.cases(rv$sequence[[rv$activeFile]][, 'amount'])) == 0) {
      sendSweetAlert(session = session, title = "Error", text = "No sample amount information in dataset.", type = "error")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]
      qualityControls <- data[, sequence[, 1] %in% "QC"] 
      
      normalizedData <- normalization(data, sequence, qualityControls, input$normMethod)
      
      data[, sequence[, 1] %in% c("QC", "Sample")] <- normalizedData
      
      rv$tmpData <- data
      rv$tmpSequence <- sequence
      
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", 
                        choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = 
                                          list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(title = "Success", text = paste0("Data normalized using ", input$normMethod), type = "success")
    }
  })
  
  observeEvent(input$saveNormalization, {
    additionalInfo <- paste("Normalized with", input$normMethod, " method")
    updateDataAndSequence("Normalize first", input$newFileNorm, "_normalized", additionalInfo)
  })
  
  ##################
  # Transformation #
  ##################
  
  
  observeEvent(input$transform, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if(input$logTransform == "None" & input$scaling == "None") {
      sendSweetAlert(session = session, title = "Warning", text = "No method selected.", type = "warning")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]
      
      cat("Selected transformation method:", input$logTransform, "\n")
      cat("Selected scaling method:", input$scaling, "\n")
      
      transformed <- transformation(data, sequence, input$logTransform, input$scaling)
      
      data[, sequence[, 1] %in% c("QC", "Sample")] <- transformed
      rv$tmpData <- data
      rv$tmpSequence <- sequence
      
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(session, title = "Success", text = "Data transformed.", type = "success")
    }
  })
  
  observeEvent(input$saveTransform, {
    additionalInfo <- paste(
      "Transformed with log transformation: ", input$logTransform,
      " and scaling: ", input$scaling
    )
    updateDataAndSequence("Transform first", input$newFileTransform, "_transformed", additionalInfo)
  })
  
  ########################
  # Statistical analysis #
  ########################
  
  observeEvent(input$testType, {
    sequence <- rv$sequence[[rv$activeFile]]
    enable("selectTest")
    switch(input$testType,
           GroupsUnpaired = {
             if(!any(complete.cases(sequence[, 4]))) {
               sendSweetAlert(session, "Oops!", "Invalid test. Provide information on different groups/conditions.", type = "error")
               disable("selectTest")
             }
           },
           GroupsPaired = {
             if(!any(complete.cases(sequence[, 4]))) {
               sendSweetAlert(session, "Oops!", "Invalid test. Provide information on different groups/conditions.", type = "error")
               disable("selectTest")
             }
             if(!any(complete.cases(sequence[, 6]))) {
               sendSweetAlert(session, "Oops!", "Invalid test. Indicate paired samples.", type = "error")
               disable("selectTest")
             }
           },
           GroupsTimeUnpaired = {
             if(!any(complete.cases(sequence[, 4]))) {
               sendSweetAlert(session, "Oops!", "Invalid test. Provide information on different groups/conditions.", type = "error")
               disable("selectTest")
             }
             if(!any(complete.cases(sequence[, 5]))) {
               sendSweetAlert(session, "Oops!", "Invalid test. Indicate time points.", type = "error")
               disable("selectTest")
             }
           },
           GroupsMultipleTime = {
             if(any(complete.cases(sequence[, 5])) & any(complete.cases(sequence[, 6]))) {
               sequence <- sequence[sequence[, 1] %in% "Sample" & complete.cases(sequence[, 4]), ]
               group_time <- getGroupTime(sequence)
               unique_values <- unique(group_time)
               combinations <- combn(unique_values, 2)
               valid_combinations <- combinations[, apply(combinations, 2, function(cols) is_valid_combination(cols[1], cols[2]))]
               contrasts <- generate_contrasts(as.matrix(valid_combinations)) # matrix bc if it's only 1 combination, valid_combinations is not a matrix and generate_contrasts fails
               updateCheckboxGroupInput(session, "contrasts", choices = contrasts, selected = NULL)
             } else {
               sendSweetAlert(session, "Oops!", "Invalid test. No paired samples or time points in dataset.", type = "error")
               disable("selectTest")
             }
           },
           CompareToReference = {
             if(!any(complete.cases(sequence[, 4]))) { #TODO or only one group
               sendSweetAlert(session, "Oops!", "Invalid test. Provide information on different groups/conditions.", type = "error")
               disable("selectTest")
             } else {
               updateSelectInput(session, "referenceGroup", label = NULL, choices = na.omit(sequence[, 'group']))
             }
           },
           {
             print('default')
           }
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$selectTest, {
    data <- rv$data[[rv$activeFile]]
    sequence <- rv$sequence[[rv$activeFile]]
    dataset_name <- gsub("^\\d+ :\\s+", "", rv$choices[rv$activeFile])
    
    # print(head(data))
    # print(str(sequence))
    # print(dataset_name)
    
    switch(input$testType, 
           GroupsUnpaired = {
             if(input$group1 == input$group2) {
               sendSweetAlert(session, "Oops!", "Choose different groups to compare.", type="error")
             } else {
               results <- groupComparison(data, sequence, c(input$group1, input$group2))
               rv$results[[rv$activeFile]][[length(rv$results[[rv$activeFile]])+1]] <- results
               names(rv$results[[rv$activeFile]])[length(rv$results[[rv$activeFile]])] <- paste0(dataset_name,": ",input$group1, "_vs_", input$group2)
               # print(head(rv$results[[rv$activeFile]]))
             }
           },
           GroupsPaired = {
             if(input$group1 == input$group2) {
               sendSweetAlert(session, "Oops!", "Choose different groups to compare.", type="error")
             } else {
               results <- groupComparisonPaired(data, sequence, c(input$group1, input$group2))
               rv$results[[rv$activeFile]][[length(rv$results[[rv$activeFile]])+1]] <- results
               names(rv$results[[rv$activeFile]])[length(rv$results[[rv$activeFile]])] <- paste0(dataset_name,": ",input$group1, "_vs_", input$group2)
             }
           },
           GroupsTimeUnpaired = {
             groups <- c(input$group1_time, input$group2_time)
             times <- c(input$time1_time, input$time2_time)
             groupTime <- paste(groups, times, sep = "_")
             print(groupTime)
             results <- groupComparisonTime(data, sequence, groups, times)
             rv$results[[rv$activeFile]][[length(rv$results[[rv$activeFile]])+1]] <- results
             names(rv$results[[rv$activeFile]])[length(rv$results[[rv$activeFile]])] <- paste0(dataset_name,": ",input$group1, "_vs_", input$group2)
           },
           GroupsMultipleTime = { # multi-level in limma 
             data <- data[sequence[, 1] %in% c("Name", "Sample")]
             sequence <- sequence[sequence[, 1] %in% c("Sample"), ]
             
             group_time <- getGroupTime(sequence)
             group_time <- factor(group_time, exclude = NA)
             paired <- factor(sequence[, 'paired'],  exclude = NA)
             results <- pairedAnalysis(data, group_time, input$contrasts, paired)
             
             rv$results[[rv$activeFile]] <- results
           },
           CompareToReference = {
             data <- data[sequence[, 1] %in% c("Name", "Sample")]
             groups <- sequence[complete.cases(sequence[, 4]), 4]
             results <- referenceGroupComparison(data, as.numeric(input$referenceGroup), groups)
             rv$results[[rv$activeFile]][[length(rv$results[[rv$activeFile]])+1]] <- results
           },
           {
             print('default')
           }
    )
    # Render one table for each contrast
    output$results_ui <- renderUI({
      lapply(seq_along(rv$results[[rv$activeFile]]), function(i) {
        fluidRow(
          column(12, strong(names(rv$results[[rv$activeFile]])[i])),
          column(12, box(width = NULL, DTOutput(paste0("results", i))))
        )
      })
    })
    lapply(seq_along(rv$results[[rv$activeFile]]), function(i) {
      output[[paste0("results", i)]] <- renderDT(
        rv$results[[rv$activeFile]][[i]], options = list(scrollX = TRUE)
      )
    })
  })
  
  #########################
  # PolySTest and VSClust #
  #########################
  
  observeEvent(input$export_polystest, {
    sequence <- rv$sequence[[rv$activeFile]]
    tdata <- rv$data[[rv$activeFile]][, sequence[, 1] %in% c("Name",  "Sample")]
    groups <- c(input$group1_polystest, input$group2_polystest)
    time <- c(input$time1_polystest, input$time2_polystest)
    selected <- selectPolySTest(tdata, sequence, groups, time)
    PolySTestMessage <- prepareMessage2(selected$selected, selected$selected_sequence, time)
    js$send_message(url="http://computproteomics.bmb.sdu.dk:443/app_direct/PolySTest/", 
                    dat=PolySTestMessage, tool="PolySTest")
  })
  
  observeEvent(input$send_polystest, {
    sequence <- rv$sequence[[rv$activeFile]]
    tdata <- rv$data[[rv$activeFile]][, sequence[, 1] %in% c("Name",  "Sample")]
    tseq <- sequence[sequence[, 1] %in% c("Name",  "Sample"), ]
    time <- complete.cases(tseq[, 5])
    if(any(complete.cases(tseq[, 5]))) {
      time <- unique(tseq[complete.cases(tseq[, 5]), 5])
    } else {
      time <- c("")
    }
    PolySTestMessage <- prepareMessage2(tdata, tseq, time)
    js$send_message(url="http://computproteomics.bmb.sdu.dk:443/app_direct/PolySTest/", 
                    dat=PolySTestMessage, tool="PolySTest")
  })
  
  observeEvent(input$send_vsclust, {
    sequence <- rv$sequence[[rv$activeFile]]
    tdata <- rv$data[[rv$activeFile]][, sequence[, 1] %in% c("Name",  "Sample")]
    tseq <- sequence[sequence[, 1] %in% c("Name",  "Sample"), ]
    VSClustMessage <- prepareMessage2(tdata, tseq)
    js$send_message(url="http://computproteomics.bmb.sdu.dk/app_direct/VSClust/",
                    dat=VSClustMessage, tool="VSClust")
  })
})

