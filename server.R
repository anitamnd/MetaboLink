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
                       timer_active = NULL, # Timer active status
                       start_time = NULL, # Start time for time tracking
                       index = NULL) # Index for volcano plot
  
  userConfirmation <- reactiveVal(FALSE)
  disable("upload")
  
  rankings_merge <- data.frame(
    name = c("high", "medium", "low"),
    priority = c(1, 2, 3)
  )
  
  massCorrection <- read.csv("./csvfiles/adducts.csv") # Import mass correction data
  refmet <- read.csv("./csvfiles/refmet.csv") # Import reference metabolite data
  query <- read.csv("./csvfiles/queried_properties.csv") # Import query data
  
  # Window/panel selection
  observeEvent(list(c(input$sequence, input$example, input$upload)), {
    windowselect("sequence")
  }, ignoreInit = T
  )
  observeEvent(input$explore, {
    windowselect("datatable")
  })
  observeEvent(input$export, {
    windowselect("export")
  })
  observeEvent(input$statistics_button, {
    windowselect("statistics")
  })
  
  
  ### Functions ###
  
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
    inputFile <<- read.csv(input$inputFile$datapath, header = 1, stringsAsFactors = F, check.names = FALSE)
    enable("upload")
  })
  
  observeEvent(input$upload, {
    shinyCatch({
      #inputFile <- read.csv(input$inputFile$datapath, header = 1, stringsAsFactors = F, check.names = FALSE)
      if(input$fileType == "Samples in rows") {
        inputFile <- t(inputFile)
      }
    },
    blocking_level = 'message'
    )
    if(any(duplicated(names(inputFile)))) {
      sendSweetAlert(session, title = "Error", text = paste("Duplicate columns found."), type = "error")
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
    
    if(any(grepl("[^[:alnum:]]", sequence$group))) {
      showModal(
        modalDialog(
          title = "Invalid group names", size = "m", easyClose = TRUE,
          footer = list(actionButton("group_name_format", "Format names"), modalButton("Dismiss")),
          fluidRow(
            column(12, p("Invalid group names found. Group names must be alphanumeric and not include spaces."))
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
    row.names(sequence) <- sequence[, 1]
    sequence <- sequence[, -1]
    rv$sequence[[length(rv$sequence) + 1]] <- sequence
    rv$data[[length(rv$data) + 1]] <- data
    names(rv$data)[length(rv$data)] <- "Liverfetus_negative"
    initializeVariables()
    
    # Positive ion mode
    data <- read.csv("./example_files/Liverfetus_lipid_pos1.csv", stringsAsFactors = FALSE)
    sequence <- read.csv("./example_files/fetus seq pos.csv", stringsAsFactors = FALSE)
    row.names(sequence) <- sequence[, 1]
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
    print(paste("Active file is", rv$activeFile))
    # Make a debugging statement for the choices
    print(paste("Choices are", rv$choices))
    # Make a debugging statement for the selectDataset
    print(paste("SelectDataset is", input$selectDataset))
    
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
    
    output$dt_boxplot_panel <- renderDT(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"], rownames = FALSE, 
                                        options = list(autoWidth = TRUE, scrollY = "700px", pageLength = 20))
    
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
    req(rv$activeFile, rv$data[[rv$activeFile]], rv$sequence[[rv$activeFile]])
    
    data <- rv$data[[rv$activeFile]]
    sequence <- rv$sequence[[rv$activeFile]]
    
    output$histogram <- renderPlotly({
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
        p <- ggplot(median_data, aes(x = Sample, y = Median, fill = Group)) +
          geom_col(position = position_dodge(width = 0.8), color = "black", width = 0.7) +
          labs(x = "Samples", y = "Median", fill = "Group") +
          facet_wrap(~Group, scales = "free_x", nrow = 1) + # Group samples by their groups
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right",
            strip.text = element_text(size = 12, face = "bold") # Style facet labels
          )
      } else {
        # No group information: Use default "skyblue" for all bars
        
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
                "select_heatmap_data", "select_volcano_data") # Update select inputs
    
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
  
  
  ## Blank filtration ##
  
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
  
  
  ## IS normalization ##
  
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
  
  
  ## Missing value filtration ##
  
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
  
  
  ## Imputation ##
  
  observeEvent(input$runImputation, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Sample") < 1) {
      showNotification("Data must have at least one Sample", type = "error")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]
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
  
  
  ## Drift correction ##
  
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
  
  
  ## Merge datasets ##
  
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
        out_dub <- data.frame(
          "nClust" = nclust,
          "Cluster_ID" = dub_dat$mergeID,
          "Ion_mode" = dub_dat$ionmode,
          "Adductor" = dub_dat$adduct,
          "Name" = dub_dat[, which(activeSequence[, 1] %in% "Name")],
          "RT" = dub_dat[, which(activeSequence[, 1] %in% "RT")],
          "Mass" = dub_dat[, which(activeSequence[, 1] %in% "Mass")],
          "CV" = cov
        )
        out_dub <- out_dub[order(out_dub[, 1], out_dub[, 2], decreasing = T), ]
        md_dup <<- out_dub
        cluster_ends <- which(!duplicated(out_dub[, 2]))
        output$md_modal_dt <- renderDataTable({
          datatable(out_dub,
                    rownames = F,
                    options = list(dom = "t", autowidth = T, paging = F),
                    selection = list(selected = finddup(out_dub, rankings_merge))
          ) %>% formatStyle(1:8, `border-top` = styleRow(cluster_ends, "solid 2px"))
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
  
  
  ## Principal Component Analysis ##
  #TODO
  observeEvent(input$run_pca1, {
    if (!is.null(rv$activeFile)) {
      if (input$selectpca1 == "Unsaved data") {
        data <- rv$tmpData
        seq <- rv$tmpSequence
      } else {
        selectchoices <- paste(seq_along(rv$data), ": ", names(rv$data))
        sd <- which(rv$choices %in% input$selectpca1)
        data <- rv$data[[sd]]
        seq <- rv$sequence[[sd]]
      }
      
      if ("Sample" %in% seq[, 1]) {
        if(any(seq[, 1] %in% "QC"))
          seq[seq[, 1] %in% "QC", ][, 4] <- "QC"
        
        sdata <- data[seq[, 1] %in% c("Sample", "QC")]
        sclass <- seq[seq[, 1] %in% c("Sample", "QC"), ][, 4]
        pca <- pcaplot(sdata, sclass, input$pca1_islog)
        output$plotpca1 <- renderPlotly(pca)
        
        if ("Sample"  %in% seq[, "labels"]) { # Check if the sequence file contains a "Sample" column
          if (any(seq[, "labels"] %in% "QC")) { # Check if the sequence file contains a "QC" column
            seq[seq[, "labels"] %in% "QC", "group"] <- "QC" # Set the "QC" column to "QC"
          } else {
            cat("No 'QC' labels found in the sequence.\n")
          }
          
          # make a print statement of the name of the dataset used
          # cat("Using dataset: ", names(rv$data)[sd], "\n")
          # make a print statement of all the sequence files 
          # cat("Sequence files: ", names(rv$sequence), "\n")
          # make a print statement of the name of the sequence used
          # cat("Using sequence: ", names(rv$sequence[[rv$activeFile]]), "\n")
          
          
          # check that data[,"Name"] exist
          name_exists <- "Name" %in% colnames(data)
          # Print the result as TRUE or FALSE
          # cat("Does the 'Name' column exist? ", name_exists, "\n")
          
          # Check if 'Name' column is unique
          is_unique <- length(unique(as.character(data[, "Name"]))) == length(as.character(data[, "Name"]))
          # Print the result as TRUE or FALSE
          # cat("Are all names unique? ", is_unique, "\n")
          
          # check if the 'Name' column is a empty string
          is_empty <- any(data[, "Name"] == "")
          # Print the result as TRUE or FALSE
          # cat("Is there an empty string in the 'Name' column? ", is_empty, "\n")
          
          # Check for duplicated names 
          is_duplicated <- any(duplicated(as.character(data[, "Name"])))
          # Print the result as TRUE or FALSE
          # cat("Are there duplicated names? ", is_duplicated, "\n")
          # Print which names are duplicated
          # cat("Duplicated names: \n")
          # print(data[duplicated(as.character(data[, "Name"])), "Name"])
          
          data_subset <- data[seq[, "labels"] %in% c("Sample", "QC")] # Get the data for the samples and QC
          rownames(data_subset) <- make.unique(as.character(data[, "Name"])) # Make the rownames unique
          
          # Debugging to show the dimensions of the data_subset
          # cat("Data before PCA: \n")
          # print(class(data_subset))
          # print(str(data))
          # any(is.na(rownames(data)))
          # any(is.na(rownames(data)))
          # any(rownames(data) == "")
          # any(rownames(data) == "NA")
          # # print(str(data_subset[,1:6]))
          # # print(str(data_subset[,(ncol(data_subset)-5):ncol(data_subset)]))
          # print(dim(data_subset))
          
          seq_subset <- seq[seq[, "labels"] %in% c("Sample", "QC"), ] # Get the sequence for the samples and QC
          
          # Debugging to show the dimensions of the seq_subset
          # cat("Seq before PCA: \n")
          # print(str(seq_subset))
          
          
          # Perform PCA once and save the results to pca_result
          pca_result <- pcaplot(data_subset, seq_subset, input$pca1_islog)
          
          # Generate a unique name for the PCA result based on the dataset name
          if (input$selectpca1 == "Unsaved data") {
            dataset_name <- "Unsaved data"
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
          
          if (sum(seq[, 1] %in% "QC") > 0) {
            qccv <- paste0("CV in QC samples: ", round(cvmean(data[seq[, 1] %in% "QC"]), 2), "</br>")
          } else {
            qccv <- "No QC in dataset </br>"
          }
          sclass <- sclass[sclass != "QC"]
          if (sum(!is.na(sclass)) > 0) {
            classcv <- sapply(sort(unique(sclass)), function(x) {
              round(cvmean(sdata[, sclass %in% x]), 2)
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
      
      sdata <- data[seq[, 1] %in% c("Sample", "QC")]
      sclass <- seq[seq[, 1] %in% c("Sample", "QC"), ][, 4]
      pca <- pcaplot(sdata, sclass, input$pca2_islog)
      output$plotpca2 <- renderPlotly(pca)
      
      if (sum(seq$labels %in% "QC") > 0) {
        qccv <- paste0("CV in QC samples: ", round(cvmean(data[seq[, 1] %in% "QC"]), 2), "</br>")
      } else {
        qccv <- "No QC in dataset </br>"
      }
      sclass <- sclass[sclass != "QC"]
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
  
  # Outlier Detection 
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
                            "wss" = fviz_nbclust(pca_df[, c("PC1", "PC2")], kmeans, method = "wss", k.max = nrow(pca_df) - 1) +
                              scale_x_discrete(breaks = seq(1, nrow(pca_df) - 1, by = 5)) +  # Reduce number of x-axis ticks
                              labs(title = "Optimal number of clusters (Elbow Method)") +
                              theme_bw(),
                            "silhouette" = fviz_nbclust(pca_df[, c("PC1", "PC2")], kmeans, method = "silhouette", k.max = nrow(pca_df) - 1) + 
                              scale_x_discrete(breaks = seq(1, nrow(pca_df) - 1, by = 5)) +  # Reduce number of x-axis ticks
                              labs(title = "Optimal number of clusters (Silhouette Method)") +
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
      
      # Retrieve parameters from the UI
      method <- input$clustering_method   # Clustering method selected
      k <- input$num_clusters_hierarchical  # Number of clusters
      threshold <- input$threshold  # Dendrogram threshold
      
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
      
      # Render the confusion matrix plot
      output$conf_matrix_plot <- renderPlotly({
        hierarchical_results$conf_matrix_plot  # Pass the plotly object to renderPlotly
      })
      
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
  # DBSCAN Clustering
  observeEvent(input$compute_knn, {
    req(rv$pca_results[[input$dbscan_pca]])  # Ensure PCA data is loaded
    pca_data <- rv$pca_results[[input$dbscan_pca]]  # Fetch the selected PCA result (list)
    pca_df <- pca_data[[1]]  # Extract pca_df from the list
    k <- input$knn
    
    # Debug print
    # print(paste("Computing kNN distance plot with k =", k))
    
    output$knn_plot <- renderPlotly({
      perform_kNN_dist_plot(pca_df, k)
    })
  })
  # Run DBSCAN
  observeEvent(input$run_dbscan, {
    req(rv$pca_results[[input$dbscan_pca]])  # Ensure PCA data is loaded
    pca_data <- rv$pca_results[[input$dbscan_pca]]  # Fetch the selected PCA result (list)
    pca_df <- pca_data[[1]]  # Extract pca_df from the list
    
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
    
    min_pts <- input$min_pts_hdbscan  # Minimum number of points for HDBSCAN
    threshold <- input$threshold_hdbscan  # Outlier threshold
    
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
    
    # Debugging print
    min_pts <- input$min_pts_optics
    eps <- if (is.na(input$eps_optics)) NULL else input$eps_optics
    eps_cl <- input$eps_cl_optics
    
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
    
    threshold <- input$lof_threshold
    min_pts <- input$lof_k
    
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
  
  # Generate Heatmap
  output$group_selection_ui <- renderUI({
    
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
      
      if (input$select_groups) {  # Only render if the checkbox is checked
        selectInput(
          "selected_groups", 
          "Select Groups:", 
          choices = seq$group, 
          selected = seq$group[1],  # Default to the first group
          multiple = TRUE,          # Allow multiple selections
          width = "100%"
        )
      }}
  })
  observeEvent(input$run_heatmap, {
    # Ensure a dataset is selected
    req(input$select_heatmap_data)
    
    cat("Success is a journey, not a destination \n")
    
    if (!is.null(rv$activeFile)) {
      if (input$select_heatmap_data == "Unsaved data") {
        data <- rv$tmpData  # Use the temporary data
        seq <- rv$tmpSequence  # Use the temporary sequence
      } else {
        # Get the index of the selected dataset
        sd <- which(rv$choices %in% input$select_heatmap_data)
        data <- rv$data[[sd]]  # Retrieve the selected dataset
        seq <- rv$sequence[[sd]]  # Retrieve the selected sequence
        dataset_name <- names(rv$data)[sd]  # Retrieve dataset name
      }
      
      # Subset data for "Sample" labels
      seq_subset <- seq[seq[, "labels"] %in% c("Sample"), ]  # Restrict to "Sample" rows
      data_subset <- data[, rownames(seq_subset), drop = FALSE]  # Use row names of seq_subset to filter columns
      
      # Check group selection
      if (input$select_groups) {
        if (is.null(input$selected_groups) || length(input$selected_groups) < 2) {
          showNotification("Please select at least two groups for the heatmap.", type = "error")
          return()  # Stop execution
        }
        # Filter seq_subset and data_subset by selected groups
        selected_groups <- input$selected_groups
        seq_subset <- seq_subset[seq_subset$group %in% selected_groups, ]
        data_subset <- data[, rownames(seq_subset), drop = FALSE]  # Subset columns by rownames of seq_subset
      }
      
      # check if the data$Name names are unique else append "(1), (2), ..." to duplicated name
      data$Name <- make.unique(data$Name)
      rownames(data_subset) <- data$Name  # Assign row names for data_subset
      
      TOP_X <- as.numeric(input$top_x)
      if (is.na(TOP_X) || TOP_X < 1) {
        showNotification("'Number of Top Features' must be a positive integer", type = "error")
        return()
      }
      
      if (TOP_X > nrow(data) ) {
        showNotification(paste0("'Number of Top Features' must be less than or equal to ", nrow(data)), type = "error")
        return()
      }
      
      show_column_names <- input$show_column_names
      show_row_names <- input$show_row_names
      cluster_rows <- input$cluster_rows
      show_row_dend <- input$show_row_dend
      
      # Log selected groups for debugging
      print(paste("Selected groups:", paste(unique(seq_subset$group), collapse = ", ")))
      print(head(seq_subset))
      print(head(data_subset, 1))
      
      # Generate the heatmap
      heatmap_plot <- plot_heatmap(data_subset, seq_subset, TOP_X, dataset_name)
      
      # Render the heatmap plot using renderPlot and draw()
      output$heatmap_plot <- renderPlot({
        if (!is.null(heatmap_plot)) {
          draw(heatmap_plot)
        }
      }, height = 600)
      
    }
  })
  
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
    cat("Your efforts are making a difference\n")
    
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
      
      log2FC_tresh <- input$log2fc_threshold
      pval_tresh <- input$pval_threshold
      
      fill_up <- input$color_up_fill
      outline_up <- input$color_up_outline
      fill_down <- input$color_down_fill
      outline_down <- input$color_down_outline
      fill_ns <- input$color_ns_fill
      outline_ns <- input$color_ns_outline
      
      
      print(paste0("dataset_name: ", dataset_name))
      print(paste0("label column: ", label_column))
      print(paste0("numerator: ", numerator))
      print(paste0("denominator: ", denominator))
      print(paste0("log2FC_tresh: ", log2FC_tresh))
      print(paste0("pval_tresh: ", pval_tresh))
      
      seq_subset <- seq[seq[, "labels"] %in% c("Sample"), ]  # Restrict to "Sample" rows
      data_subset <- data[, c(rownames(seq_subset)), drop = FALSE]  # Use row names of seq_subset to filter columns
      
      # make a check that data[,label_column] are unique else return error
      # if (length(unique(data[, label_column])) != length(data[, label_column])) {
      #   showNotification(
      #     paste0("The column selected (", label_column , ") for feature labels is not unique!"),
      #     type = "warning",   # "message", "warning", or "error"
      #     duration = NULL     # or a specific number of seconds to show
      #   )
      #   return(NULL) # return from the function/observe and don't proceed
      # }
      
      
      rownames(data_subset) <- make.unique(as.character(data[, label_column])) # Make the rownames unique
      
      stat_results <- calculate_stats(data_subset, seq_subset)
      
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
      
      print(head(sub_df))
      
      # assign the sub_df a name for debugging
      volcano_df_name <- paste0(dataset_name, "_volcano_df")
      assign(volcano_df_name, sub_df)
      
      vlcn <- pretty_volcano_plot(sub_df,volcano_df_name,
                                  log2FC_tresh, pval_tresh,
                                  fill_up, outline_up,
                                  fill_down, outline_down,
                                  fill_ns, outline_ns)
      
      output$volcano_plot <- renderPlotly({
        vlcn
      })
      
      message("Let god have mercy on our souls")
      
      # Output the data table of upregulated/downregulated features
      # output$volcano_table <- renderDT({
      #   up_down_regulated <- volcano_df %>%
      #     filter(
      #       (`p adj` < pval_threshold & Log2FC > log2fc_threshold) |
      #         (`p adj` < pval_threshold & Log2FC < -log2fc_threshold)
      #     ) %>%
      #     select(Metabolite, Comparison, `p adj`, Log2FC) 
      #   
      #   datatable(up_down_regulated, options = list(pageLength = 20, autoWidth = TRUE))
      # })
    }
    
  })
  
  #### Pathway Enrichment Analysis 
  observeEvent(input$select_data_for_enrichment, {
    req(input$select_data_for_enrichment) # Ensure a dataset is selected
    
    cat("Stay curious and keep coding \n")
    
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
      
      columns <- c("identifier_column","compound_column")
      
      for (column in columns) {
        # Update the 'identifier_column' select input with the new choices
        updateSelectInput(session, column, choices = data_colnames)
      }
    }
    
    cat("Keep pushing forward \n")
    
  })
  observeEvent(input$run_gather_identifiers, {
    req(input$select_data_for_enrichment,
        input$identifier_column,
        input$compound_column)
    
    show_modal_spinner(
      spin = "atom",
      color = "#0A4F8F",
      text = "Gathering Identifiers... This may take a few minutes."
    )
    cat("Keep up the good work \n")
    
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
      
      # Use subset_data function to subset the data
      subset <- subset_data(data, compound)
      
      # Set this to TRUE during development and FALSE in production
      run_development_code <- TRUE
      
      if (run_development_code) {
        # Check how many rows query has initially
        query_start <- nrow(query)
        print(paste("Number of rows in query before gathering identifiers:", query_start))
        
        desired_properties <- c(
          "MolecularFormula", "MolecularWeight", "ExactMass", "MonoisotopicMass",
          "CanonicalSMILES", "IsomericSMILES", "InChI", "InChIKey", "IUPACName"
        )
        
        all_results <- data.frame()
        chunk_size <- 5
        
        # Identify new compounds (those not already in query$Identifier)
        new_compounds <- subset[[compound]][!subset[[compound]] %in% query$Identifier]
        new_compounds <- unique(new_compounds)
        
        clean_names <- FALSE # Set to TRUE to clean compound names else FALSE
        if (clean_names) {
          new_compounds <- remove_top_level_comma(new_compounds)
          new_compounds <- clean_compound_names(new_compounds)
        }
        
        print(paste("Number of new compounds not in query:", length(new_compounds)))
        
        if (length(new_compounds) > 0) {
          num_rows <- length(new_compounds)
          print(paste("Number of rows/elements in new_compounds for querying:", num_rows))
          
          row_indices <- seq(1, num_rows, by = chunk_size)
          print("Row indices to be processed:")
          print(row_indices)
          
          for (start_idx in row_indices) {
            end_idx <- min(start_idx + chunk_size - 1, num_rows)
            print(paste("Processing rows from", start_idx, "to", end_idx))
            
            compound_subset <- new_compounds[start_idx:end_idx]
            
            print("Current compound subset:")
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
              all_results <- dplyr::bind_rows(all_results, props_retrieved)
            } else {
              print("No valid rows retrieved for this batch.")
            }
            
            print(Sys.time())
            # Pause to respect the 5 queries/second limit
            Sys.sleep(1)
          }
          
          if (nrow(all_results) > 0) {
            # Convert these columns to numeric if they aren't already
            all_results$MolecularWeight <- as.numeric(all_results$MolecularWeight)
            all_results$ExactMass <- as.numeric(all_results$ExactMass)
            all_results$MonoisotopicMass <- as.numeric(all_results$MonoisotopicMass)
            
            dir_path <- "~/Desktop/SDU/Cand/Master thesis /GitHub/MetaboLink-Main/csvfiles" # Adjust path as needed
            file_path <- file.path(dir_path, "queried_properties.csv")
            
            # If the file already exists, read it and combine
            if (file.exists(file_path)) {
              existing_data <- read.csv(file_path, stringsAsFactors = FALSE)
              
              # Identify new entries that are not already in existing_data by 'Identifier'
              if ("Identifier" %in% colnames(existing_data) && "Identifier" %in% colnames(all_results)) {
                new_entries <- dplyr::anti_join(all_results, existing_data, by = "Identifier")
                
                query_final <- nrow(new_entries)
                print(paste("Number of rows in query after gathering identifiers:", query_final))
                
              } else {
                # If 'Identifier' is not present, just append all results
                warning("No 'Identifier' column found. Appending all results without filtering.")
                new_entries <- all_results
              }
              
              # Only bind if there are new entries
              if (nrow(new_entries) > 0) {
                combined_data <- dplyr::bind_rows(existing_data, new_entries)
                # Remove duplicates based on Identifier if needed
                combined_data <- combined_data[!duplicated(combined_data$Identifier), ]
                
                combined_data_nrow <- nrow(combined_data)
                print(paste("Number of rows in query after gathering identifiers:", combined_data_nrow))
                
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
        text = "Updating InChI... Please be patient."
      )
      
      # Update InChI from local cache and online queries
      print("Running update_inchi function...")
      subset_updated <- update_inchi(subset, compound, identifier, query)
      # print(head(subset_updated))
      
      update_modal_spinner(
        session = session,
        text = "Looking for CID's... Please be patient."
      )
      
      # Gather CID's after InChI update
      print("Running update_cid function...")
      subset_updated <- update_cid(subset_updated, identifier, query)
      print("Update cid function completed.")
      
      subset_updated$cid <- as.integer(subset_updated$cid)
      cid_info <- subset_updated[, c("Name", "cid")]
      # print(head(cid_info))
      
      data_updated <- merge(data, cid_info, by = "Name", all.x = TRUE)
      # print(head(data_updated))
      
      refmet_updated <- refmet[!is.na(refmet$pubchem_cid),]
      filtered_refmet <- refmet_updated[refmet_updated$pubchem_cid %in% subset_updated$cid, ]
      # print(head(filtered_refmet))
      
      final_data <- merge(data_updated, filtered_refmet, by.x = "cid", by.y = "pubchem_cid", all.x = TRUE)
      # print(head(final_data))
      
      update_modal_spinner(
        session = session,
        text = "Looking for pathways... Please be patient."
      )
      
      # add pathways to final_data 
      data_updated_list <- get_kegg_pathways(final_data)
      
      pathways_long <- data_updated_list$pathways_long
      data_updated_pathways <- data_updated_list$data_joined
      
      # Print outputs for verification
      cat("\n--- Long format (pathways_long) ---\n")
      print(head(pathways_long))
      
      cat("\n--- Merged dataset ---\n")
      print(head(data_updated_pathways))
      
      # add new columns to data_updated_pathways
      new_cols <- setdiff(colnames(data_updated_pathways), colnames(data_updated))
      new_cols <- c("cid", new_cols)
      
      identifier_index <- which(names(data_updated_pathways) == "inchi")
      
      original_cols <- names(data_updated_pathways)[!(names(data_updated_pathways) %in% new_cols)]
      
      final_col_order <- append(original_cols, new_cols, after = identifier_index)
      
      final_data <- data_updated_pathways[, final_col_order]
      
      cat("\n--- Final dataset ---\n")
      print(head(final_data))
      
      # add pathways to final_data 
      # final_data <- get_pathways(final_data)
      
      
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
    
    cat("Your code is working \n")
    
    # Stop the timer and remove the spinner
    rv$timer_active <- FALSE
    remove_modal_spinner()
  })
  
  # Render the identifier count table
  output$identifier_count_table <- renderUI({
    req(nrow(rv$identifier_df) > 0)
    
    # Count valid (non-NA and non-empty) values
    valid_counts <- sapply(rv$identifier_df, function(x) {
      sum(!is.na(x) & x != "")
    })
    
    # Count missing or empty values
    na_empty_counts <- sapply(rv$identifier_df, function(x) {
      sum(is.na(x) | x == "")
    })
    
    # Build the HTML content
    html_content <- HTML(
      paste0("<b>Total Identifiers retrieved:</b> ", nrow(rv$identifier_df), "<br><br>"),
      "<b>Identifiers Found (valid vs. NA/empty):</b><br>",
      paste(
        sapply(names(valid_counts), function(col_name) {
          paste0(
            col_name, ": ",
            valid_counts[col_name], " valid ( ",
            na_empty_counts[col_name], " NA/empty)", 
            "<br>"
          )
        }),
        collapse = ""
      )
    )
    html_content
  })
  
  # Render the identifier table
  output$identifier_table <- renderDT({
    req(nrow(rv$identifier_df) > 0)
    datatable(
      rv$identifier_df, 
      options = list(
        pageLength = 10,
        autoWidth = FALSE,
        scrollX = TRUE
      ),
      class = "display nowrap"
    )
  })
  
  # Plot of classes in the identifer table
  output$super_class_plot <- renderPlotly({
    req(input$select_data_for_enrichment)
    
    sd <- which(rv$choices == input$select_data_for_enrichment)
    data_sub <- rv$data[[sd]]
    
    # remove rows where NA, "", " " or "NA" are present in the super_class column
    data_sub <- data_sub[!is.na(data_sub$super_class) &
                           data_sub$super_class != "" &
                           data_sub$super_class != " " &
                           data_sub$super_class != "NA" &
                           data_sub$super_class != "N/A", ]
    
    # Check if "super_class" column exists; if not, send an error alert and exit.
    if (!("super_class" %in% colnames(data_sub))) {
      sendSweetAlert(
        session,
        title = "Error",
        text = "No Super Class column found in the dataset. 
                  Make sure a column named 'super_class' is present 
                  by running 'Gather Identifiers'.",
        type = "error"
      )
      return(NULL)  # Stop further execution of the renderPlotly block.
    }
    
    # Create ggplot
    p <- data_sub %>%
      arrange(super_class) %>%  # Sort the dataframe lexicographically
      mutate(super_class = factor(super_class, levels = unique(super_class))) %>%  # Ensure unique levels
      ggplot(aes(x = super_class)) +
      geom_bar(fill = "blue", width = 0.2) +
      labs(title = "Distribution of Super Classes",
           x = "Super Class",
           y = "Count") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels
    
    # Convert ggplot to interactive plotly plot
    ggplotly(p)
  })
  output$main_class_plot <- renderPlotly({
    req(input$select_data_for_enrichment)
    
    sd <- which(rv$choices == input$select_data_for_enrichment)
    data_sub <- rv$data[[sd]]
    
    # Check if "super_class" column exists; if not, send an error alert and exit.
    if (!("main_class" %in% colnames(data_sub))) {
      sendSweetAlert(
        session,
        title = "Error",
        text = "No Main Class column found in the dataset. 
              Make sure a column named 'main_class' is present 
              by running 'Gather Identifiers'.",
        type = "error"
      )
      return(NULL)  # Stop further execution of the renderPlotly block.
    }
    
    # If the column exists, create the Plotly bar chart.
    plot_ly(
      x = names(table(data_sub$main_class)),
      y = as.vector(table(data_sub$main_class)),
      type = "bar",
      name = "Main Class",
      marker = list(color = "red")
    ) %>%
      layout(
        title = "Distribution of Main Classes",
        xaxis = list(
          tickangle = 45,            # Rotate labels 45 degrees
          tickfont  = list(size = 10) # Make label text smaller
        ),
        margin = list(b = 100)        # Add bottom margin if labels get cut off
      )
  })
  output$sub_class_plot <- renderPlotly({
    req(input$select_data_for_enrichment)
    
    sd <- which(rv$choices == input$select_data_for_enrichment)
    data_sub <- rv$data[[sd]]
    
    # Check if "sub_class" column exists; if not, send an error alert and exit.
    if (!("sub_class" %in% colnames(data_sub))) {
      sendSweetAlert(
        session,
        title = "Error",
        text = "No Sub Class column found in the dataset. 
              Make sure a column named 'sub_class' is present 
              by running 'Gather Identifiers'.",
        type = "error"
      )
      return(NULL)  # Stop further execution of the renderPlotly block.
    }
    
    # If the column exists, create the Plotly bar chart.
    plot_ly(
      x = names(table(data_sub$sub_class)),
      y = as.vector(table(data_sub$sub_class)),
      type = "bar",
      name = "Sub Class",
      marker = list(color = "green")
    ) %>%
      layout(
        title = "Distribution of Sub Classes",
        xaxis = list(
          tickangle = 45,            # Rotate labels 45 degrees
          tickfont  = list(size = 10) # Make label text smaller
        ),
        margin = list(b = 100)        # Add bottom margin if labels get cut off
      )
  })
  
  observeEvent(input$run_enrichment_analysis, {
    req(input$select_data_for_enrichment)
    
    # Ensure only one of gene/module is selected
    if (input$gene_selected && input$module_selected) {
      sendSweetAlert(session, "Error", "Select only one: Gene or Module.", type = "error")
      return()
    } else if (!input$gene_selected && !input$module_selected) {
      sendSweetAlert(session, "Error", "Please select either Gene or Module.", type = "error")
      return()
    }
    
    cat("You have made it this far \n")
    
    sd <- which(rv$choices == input$select_data_for_enrichment)
    data <- rv$data[[sd]]
    seq <- rv$sequence[[sd]]
    
    data_name <- names(rv$data)[sd]
    
    top_x <- as.numeric(input$top_x_enrich)
    
    # display warning if column "kegg_id" or "kegg id" is not present
    if (!("kegg_id" %in% colnames(data) || "kegg id" %in% colnames(data))) {
      sendSweetAlert(session, "Error",
                     "No KEGG ID column found in the dataset. 
      Make sure column named 'kegg_id' is present by running 'Gather Identifiers' ", type = "error")
      return()
    }
    
    # display warning if column "refmet_name" or "refmet name" is not present
    if (!("refmet_name" %in% colnames(data) || "refmet name" %in% colnames(data))) {
      sendSweetAlert(session, "Error",
                     "No Reference Metabolite Name column found in the dataset. 
      Make sure column named 'refmet_name' is present by running 'Gather Identifiers' ", type = "error")
      return()
    }
    
    # subset data to only include kegg_id column
    kegg_data <- data[c(!is.na(data$kegg_id)),]
    kegg_data <- kegg_data[c(kegg_data$kegg_id != ""),]
    
    # Select only kegg_data where the columns are samples, kegg_id and refmet_name
    sample_names <- rownames(seq[seq$labels == "Sample",])
    kegg_data <- kegg_data[,c("refmet_name","kegg_id", sample_names)]
    
    # TODO Might not be necessary
    kegg_data <- kegg_data[!duplicated(kegg_data$kegg_id),]
    
    # select group for enrichment analysis
    group <- input$group_enrichment
    
    group_samples <- rownames(seq[seq$group == group & seq$labels == "Sample",])
    
    # select data only for group 
    kegg_data <- kegg_data[,c("refmet_name","kegg_id", group_samples)]
    
    # Run enrichment analysis based on selection
    if (input$gene_selected) {
      title_name <- "Gene Enrichment Analysis"
      print(paste0("#---", title_name, " ", data_name, "---#"))
      
      all_kegg <- unique(kegg_data$kegg_id)
      
      print(paste0("# row :" , nrow(kegg_data)))
      sup_kegg_data <- kegg_data %>% 
        filter(rowMeans(!is.na(.)) >= 0.75)
      print(paste0("# row :" , nrow(sup_kegg_data)))
      
      enrichment_result <- run_gene_enrichment(sup_kegg_data, all_kegg)
      print(head(enrichment_result))
      
    } else {
      title_name <- "Module Enrichment Analysis"
      print(paste0("#---", title_name, " ", data_name, "---#"))
      
      all_kegg <- unique(kegg_data$kegg_id)
      
      print(paste0("# row :" , nrow(kegg_data)))
      sup_kegg_data <- kegg_data %>% 
        filter(rowMeans(!is.na(.)) >= 0.75)
      print(paste0("# row :" , nrow(sup_kegg_data)))
      
      enrichment_result <- run_module_enrichment(sup_kegg_data, all_kegg)
      print(head(enrichment_result))
    }
    
    plots <- bar_dot_plot(enrichment_result,title_name,top_x)
    
    output$enrichment_barplot <- renderPlot({
      plots$bar
    })
    
    output$enrichment_dotplot <- renderPlot({
      plots$dot
    })
    
    # TODO 
    if (input$gene_selected) {
      cat("#-----Selected data Gene-----# \n")
      cnet <- plot_cnetplot_subcat(enrichment_result, kegg_data, top_x)
    } else {
      cat("#-----Selected data Module-----# \n")
      cnet <- plot_cnetplot_desc(enrichment_result, kegg_data, top_x)
    }
    
    output$enrichment_cnetplot <- renderPlot({
      cnet$plot
    })
    
    output$enrichment_table <- renderDT({
      datatable(
        cnet$net, 
        options = list(
          pageLength = 10,
          autoWidth = FALSE,
          scrollX = TRUE
        ),
        class = "display nowrap"
      )
    })
    
    cat("You are awesome \n")
    
  })
  
  #### Summary of data
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
        HTML(nrow(dat) - 1, " features.<br>", ncol(dat[seq[, 1] %in% "Sample"]), " samples.<br>", ncol(dat[seq[, 1] %in% "QC"]), " QC samples.<br>", ncol(dat[seq[, 1] %in% "Blank"]), " Blank samples.<br>", "<br>", sample_mv, " missing values in Samples<br>", qc_mv, " missing values in QC samples<br>", blank_mv, " missing values in Blank samples<br><br>")
      })
      # Update the CV information UI
      output$cvinfo_ui <- renderUI({
        HTML(text)
      })
      
      # Update statistics select input options
      groups <- na.omit(seq[, 'group'])
      time <- na.omit(seq[, 'time'])
      
      group_inputs <- c("group1", "group2", "group1_time", "group2_time", "group1_polystest", "group2_polystest")
      
      # Update select input options where groups are needed
      groups <- unique(seq$group[seq$labels == "Sample" & seq$group != ""])
      time <- unique(seq$time[seq$labels == "Sample" & seq$time != ""])
      
      group_inputs <- c("group1", "group2",
                        "group1_time", "group2_time",
                        "group1_polystest", "group2_polystest",
                        "group_enrichment",
                        "group1_vol", "group2_vol")
      
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
      
    }
  })
  
  ## Normalization ##
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
  
  
  ## Transformation ##
  
  observeEvent(input$transform, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if(input$logTransform == "None" & input$scaling == "None") {
      sendSweetAlert(session = session, title = "Warning", text = "No method selected.", type = "warning")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]
      
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
  
  
  ## Statistical analysis ##
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
  
  
  ## Send data to PolySTest and VSClust ##
  
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