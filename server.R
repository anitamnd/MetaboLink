shinyServer(function(session, input, output) {
  options(shiny.maxRequestSize = 30 * 1024^2)

  ## General
  rv <- reactiveValues(data = list(), sequence = list(), activeFile = NULL, 
                  tmpData = NULL, tmpSequence = NULL, 
                  choices = NULL, drift_plot_select = 1, info = vector("character"))

  ## Statistics
  st <- reactiveValues(stats = list(), sequence = list(), results = list(),
                  comparisons = list(), colcomp = list())

  userConfirmation <- reactiveVal(FALSE)

  rankings <- read.csv("./csvfiles/rankings.csv", stringsAsFactors = FALSE)

  observeEvent(list(c(input$sequence, input$example, input$submit)), {
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
    updateSelectInput(session, "group1", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'class']))
    updateSelectInput(session, "group2", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'class']))
    updateSelectInput(session, "time1", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'time']))
    updateSelectInput(session, "time2", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'time']))
  })

  # Functions

  initializeVariables <- function() {
    st$stats[[length(st$stats) + 1]] <- data.frame()
    st$sequence[[length(st$stats) + 1]] <- data.frame()
    st$results[[length(st$results) + 1]] <- list()
    st$comparisons[[length(st$comparisons) + 1]] <- vector("character")
    st$colcomp[[length(st$colcomp) + 1]] <- vector("numeric")
  }

  observeEvent(input$submit, {
    shinyCatch({
      inputFile <- read.csv(input$inputFile$datapath, header = 1, stringsAsFactors = F, check.names = FALSE)
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
      checkColumns(colnames(inputFile), labels)
      batch <- NA
      order <- NA
      class <- NA
      time <- NA
      paired <- NA
      initializeVariables()
      rv$sequence[[length(rv$sequence) + 1]] <- data.frame(labels, batch, order, class, time, paired)
      rv$data[[length(rv$data) + 1]] <- inputFile
      names(rv$data)[length(rv$data)] <- substr(input$inputFile$name, 1, nchar(input$inputFile$name) - 4)
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
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

  observeEvent(input$editSequence, {
    showModal(
      modalDialog(
        title = "Edit columns", size = "s", easyClose = TRUE,
        footer = list(actionButton("seq_edit_confirm", "Confirm"), modalButton("Dismiss")),
        fluidRow(
          column(width = 9, h4("Column name")),
          column(width = 3, style = "text-align: left;", h4("Keep"))
        ),
        lapply(seq(ncol(rv$data[[rv$activeFile]])), function(x) {
          fluidRow(
            column(
              width = 9,
              textInput(paste0("seq_edit_name", x), NULL, value = colnames(rv$data[[rv$activeFile]])[x])
            ),
            column(
              width = 3, style = "text-align: center;",
              prettyCheckbox(paste0("seq_edit_keep", x), NULL, status = "info", value = T)
            ),
          )
        })
      )
    )
  })

  observeEvent(input$editGroups, {
    showModal(
      modalDialog(
        title = "Edit Group Nicknames", size = "s", easyClose = TRUE,
        footer = list(actionButton("group_edit_confirm", "Confirm"), modalButton("Dismiss")),
        fluidRow(
          column(width = 3, h4("Group")),
          column(width = 9, h4("Nickname"))
        ), 
        lapply(seq(unique(rv$sequence[[rv$activeFile]][, 4][!is.na(rv$sequence[[rv$activeFile]][, 4])])), function(x) {
          group <- sort(unique(rv$sequence[[rv$activeFile]][, 4][!is.na(rv$sequence[[rv$activeFile]][, 4])]))[x]
          fluidRow(
            column(width = 2, h5(stri_extract_first_regex(group, "[0-9]+"))),
            column(
              width = 10,
              textInput(paste0("edit_nickname", x), NULL, value = NULL)
            ),
          )
        }),
      )
    )
  })

  # Duplicates not allowed
  observeEvent(input$seq_edit_confirm, {
    # sapply(seq(ncol(rv$data[[rv$activeFile]])), function(x) {
    #   isolate(colnames(rv$data[[rv$activeFile]])[x] <- input[[paste0("seq_edit_name", x)]])
    #   isolate(row.names(rv$sequence[[rv$activeFile]])[x] <- input[[paste0("seq_edit_name", x)]])
    # })
    keep <- sapply(seq(ncol(rv$data[[rv$activeFile]])), function(x) input[[paste0("seq_edit_keep", x)]])
    rv$data[[rv$activeFile]] <- rv$data[[rv$activeFile]][, keep]
    rv$sequence[[rv$activeFile]] <- rv$sequence[[rv$activeFile]][keep, ]
    removeModal()
  })

  observeEvent(input$group_edit_confirm, {
    groups <- rv$sequence[[rv$activeFile]][, 4]
    sapply(seq(ncol(rv$data[[rv$activeFile]])), function(x) {
      if(!is.na(groups[x])) {
        isolate(rv$sequence[[rv$activeFile]][, 4][x] <- paste(rv$sequence[[rv$activeFile]][, 4][x], input[[paste0("edit_nickname", groups[x])]], sep = ": "))
      }
    })
    removeModal()
    show("sequence_panel")
  })

  observeEvent(input$example, {
    # Lipidomics
    data <- read.csv("./csvfiles/Eva pos export from profinder.csv", stringsAsFactors = FALSE)
    sequence <- read.csv("./csvfiles/sequence_lipidomics_pos.csv", stringsAsFactors = FALSE)
    row.names(sequence) <- sequence[, 1]
    sequence <- sequence[, -1]
    rv$sequence[[length(rv$sequence) + 1]] <- sequence
    rv$data[[length(rv$data) + 1]] <- data
    names(rv$data)[length(rv$data)] <- "Lipidomics_pos"
    initializeVariables()

    # Metabolomics
    data <- read.csv("./csvfiles/Woz export from mzmine pos.csv", stringsAsFactors = FALSE)
    sequence <- read.csv("./csvfiles/sequence_metabolomics_pos.csv", stringsAsFactors = FALSE)
    row.names(sequence) <- sequence[, 1]
    sequence <- sequence[, -1]
    rv$sequence[[length(rv$sequence) + 1]] <- sequence
    rv$data[[length(rv$data) + 1]] <- data
    names(rv$data)[length(rv$data)] <- "Metabolomics_pos"
    initializeVariables()
    rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
    updateTabItems(session, "tabs", selected = "Datainput")
    show("buttons")
    updateCollapse(session, "menu", close = "Data input")
    disable("example")
  })

  # Update selected data
  observeEvent(input$selectDataset, ignoreInit = TRUE, { 
    rv$activeFile <- which(rv$choices %in% input$selectDataset)
    rows <- nrow(rv$data[[rv$activeFile]])
    cols <- ncol(rv$data[[rv$activeFile]])
    if(rows > 50)
      rows <- 50
    if(cols > 30)
      cols <- 30
    forVisuals <- rv$data[[rv$activeFile]][1:rows, 1:cols]
    sequence <- rv$sequence[[rv$activeFile]]
    data <- rv$data[[rv$activeFile]]

    output$seq_table <- renderDT(rv$sequence[[rv$activeFile]], extensions = 'Responsive', server = F, 
          editable = T, selection = 'none', options = list(pageLength = nrow(rv$sequence[[rv$activeFile]]), 
          fixedHeader = TRUE))
    output$diboxtitle <- renderText(names(rv$data[rv$activeFile]))
    output$dttable <- renderDT(data, rownames = FALSE, options = list(scrollX = TRUE, 
              scrollY = "700px"))
    output$dt_drift_panel <- renderDT(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"], rownames = FALSE, 
              options = list(autoWidth = TRUE, scrollY = "700px", pageLength = 20))
    output$dt_boxplot_panel <- renderDT(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"], rownames = FALSE, 
              options = list(autoWidth = TRUE, scrollY = "700px", pageLength = 20))
    
    output$histogram <- renderPlotly({
      samples <- data[, sequence[ , 'labels'] %in% "Sample"]
      samples[is.na(samples)] <- 0
      medians <- apply(samples, 2, median)
      median_data <- data.frame(
        Sample = names(medians),
        Median = medians
      )
      ggplot(median_data, aes(x = Sample, y = Median)) +
        geom_col(fill = "skyblue", color = "black") +
        labs(x = "Sample", y = "Median") +
        theme_minimal()
    })

    output$histogram_qc <- renderUI({
      QCs <- data[, sequence[ , 'labels'] %in% "QC"]
      if(ncol(QCs) > 0) {
        QCs[is.na(QCs)] <- 0
        medians <- apply(QCs, 2, median)
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

    if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Name") == 1) {
      internalStandards <- findInternalStandards(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"])
      updateCheckboxGroupInput(session, "isChoose", choices = internalStandards, selected = internalStandards)
      enable("normalizeIS"); enable("optimizeIS"); enable("removeIS"); enable("saveIS")
      if(length(internalStandards) == 0) {
        disable("normalizeIS"); disable("optimizeIS"); disable("removeIS"); disable("saveIS")
      } 
    }

    # Statistics
    updateSelectInput(session, "group1", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'class']))
    updateSelectInput(session, "group2", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'class']))
    updateSelectInput(session, "time1", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'time']))
    updateSelectInput(session, "time2", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'time']))
  })

  observeEvent(rv$choices, {
    output$downloadSequence <- downloadHandler(
      filename <- function() {
        paste0(names(rv$data[rv$activeFile]), "_seq.csv")
      },
      content = function(file) {
        write.csv(cbind("sample" = rownames(rv$sequence[[rv$activeFile]]), rv$sequence[[rv$activeFile]]), file, row.names = FALSE) # TODO
      }
    )
    
    # Export panel
    output$export_ui <- renderUI({
      lapply(1:length(rv$choices), function(x) {
        fluidRow(column(12, downloadLink(paste0("dwn", x), paste0(rv$choices[x], ".csv"))))
      })   
    })
    output$export_metabo <- renderUI({
        lapply(1:length(rv$choices), function(x) {
          fluidRow(column(12, downloadLink(paste0("dwn_metabo", x), paste0(rv$choices[x], "_metabo.csv"))))
        })
    })
    output$export_stats <- renderUI({
        lapply(1:length(rv$choices), function(x) {
          fluidRow(column(12, downloadLink(paste0("dwn_stats", x), paste0(rv$choices[x], "_results.xlsx"))))
        })
    })
    output$export_settings <- renderUI({
        lapply(1:length(rv$choices), function(x) {
          fluidRow(column(12, downloadLink(paste0("dwn_settings", x), paste0(rv$choices[x], ".txt"))))
        })
    })

    lapply(1:length(rv$choices), function(x) {
      output[[paste0("dwn_stats", x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), "_results.xlsx")
        },
        content = function(file) {
          write_xlsx(st$results[[x]], file)
        }
      )
    })

    lapply(1:length(rv$choices), function(x) {
      output[[paste0("dwn", x)]] <- downloadHandler(
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
      class <- c("", seq[seq[, 1] %in% c("Sample", "QC"), 4])
      outdat <- data.frame(dat[seq[, 1] %in% "Name"], dat[seq[, 1] %in% c("Sample", "QC")])
      outdat <- rbind(class, outdat)
      output[[paste0("dwn_metabo", x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), "_metabo.csv")
        },
        content = function(file) {
          write.csv(outdat, file, row.names = FALSE)
        }
      )
    })

    updateCheckboxGroupInput(session, "export_xml_list", choices = rv$choices, selected = NULL)
    updateSelectInput(session, "mergeFile", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "drift_select", choices = c("None", rv$choices))
    updateSelectInput(session, "selectDataset", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca2", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateCheckboxGroupInput(session, "filesToRemove", choices = names(rv$data), selected = NULL)
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
      st$stats <- st$stats[keep]
      st$sequence <- st$sequence[keep]
      st$results <- st$results[keep]
      st$comparisons <- st$comparisons[keep]
      st$colcomp <- st$colcomp[keep]
      rv$activeFile <- names(rv$data)[length(rv$data)]
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
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

  # Blank filtration
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
      sendSweetAlert(
        session = session,
        title = "Success",
        text = paste0(nrow(rv$data[[rv$activeFile]]) - nrow(rv$tmpData), " features removed"),
        type = "success"
      )
    }
  })

  observeEvent(input$saveBF, {
    if (is.null(rv$tmpData)) {
      showNotification("Blank filtrate first", type = "error")
    } else {
      if (input$newFileBF) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpData
        rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_", input$signalStrength, "xb")
        initializeVariables()
      } else {
        rv$data[[rv$activeFile]] <- rv$tmpData
        rv$sequence[[rv$activeFile]] <- rv$tmpSequence
        names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_", input$signalStrength, "xb")
      }
      rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Blank filtrated with singal strength above blank =", input$signalStrength, "\n")
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL   
    }
  })

  # IS normalization
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
      rv$tmpData <- normalized
      rv$tmpSequence <- sequence
      sendSweetAlert(session, title = "Success", text = paste0("Internal standards normalized with ", input$isMethod, " method"), type = "success")
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
    }
  })

  observeEvent(input$optimizeIS, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else {
      sequence <- rv$sequence[[rv$activeFile]]
      data <- rv$data[[rv$activeFile]]
      optimized <- optimizeIS(data, sequence, input$isChoose, input$isMethod, input$normalizeQC)
      updateCheckboxGroupInput(session, "isChoose", selected = optimized)
    }
  })

  observeEvent(input$saveIS, {
    if(is.null(rv$tmpData)) {
      showNotification("IS normalize first", type = "error")
    } else {
      if(input$newFileIS) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpData
        rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_is")
        initializeVariables()
      } else {
        rv$data[[rv$activeFile]] <- rv$tmpData
        rv$sequence[[rv$activeFile]] <- rv$tmpSequence
        names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_is")
      }
      rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Internal standards normalized with", input$isMethod, "method\n")
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL
    }
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

  # Missing value filtration
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
    if (is.null(rv$tmpData)) {
      showNotification("Filtrate first", type = "error")
    } else {
      if (input$mvf_newsave) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpData
        rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_mvr")
        initializeVariables()
      } else {
        rv$data[[rv$activeFile]] <- rv$tmpData
        rv$sequence[[rv$activeFile]] <- rv$tmpSequence
        names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_mvr")
      }
      rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Missing value filtration using", 
                input$cutoffNAs, "% as threshold and method -", paste(input$filterNAmethod, collapse=", "), "\n")
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL   
    }
  })

  # Imputation
  observeEvent(input$runImputation, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Sample") < 1) {
      showNotification("Data must have at least one Sample", type = "error")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]
      imputed <- imputation(data, sequence,
        method = input$imputationMethod,
        minx = input$imputationMinX,
        onlyqc = input$imp_onlyQC,
        remaining = input$remainingNAs
      )
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
    if (is.null(rv$tmpData)) {
      showNotification("Imputate first", type = "error")
    } else {
      if(input$newFileImp) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpData
        rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_imp")
        initializeVariables()
      } else {
        rv$data[[rv$activeFile]] <- rv$tmpData
        rv$sequence[[rv$activeFile]] <- rv$tmpSequence
        names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_imp")
      }
      rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Missing values imputation with", input$imputationMethod, "\n")
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL   
    }
  })

  # Drift correction
  observeEvent(input$driftMethod, {
    if (input$driftMethod == "QC-RFSC (random forrest)") {
      hide("dc_qcspan_hide")
      hide("dc_degree_hide")
      show("dc_ntree_hide")
    } else {
      hide("dc_ntree_hide")
      show("dc_qcspan_hide")
      show("dc_degree_hide")
    }
  })

  observeEvent(input$runDrift, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else {
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]  
      dat_qc <- data[, sequence[, 1] %in% "QC"]

      if(any(colSums(!is.na(dat_qc)) != nrow(dat_qc))) {
        sendSweetAlert(session = session, title = "Error", text = "QCs cannot have missing values.", type = "error")
      }
      else {
        corrected <- driftcorrection(data, sequence,
          method = input$driftMethod,
          ntree = input$driftTrees,
          QCspan = input$driftQCspan
        )
        rv$tmpData <- corrected
        rv$tmpSequence <- sequence
        updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
        output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      }
    }
  })

  observeEvent(input$saveDrift, {
    if (is.null(rv$tmpData)) {
      showNotification("Drift correct first", type = "error")
    } else {
      if (input$newFileDrift) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpData
        rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_dc")
        initializeVariables()

      } else {
        rv$data[[rv$activeFile]] <- rv$tmpData
        rv$sequence[[rv$activeFile]] <- rv$tmpSequence
        names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_dc")
      }
      rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Drift correction", input$driftMethod, "and", input$driftTrees, "\n")
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL   
    }
  })

  # Merge datasets
  observeEvent(input$mergeRankings, {
    showModal(
      modalDialog(
        title = "Change the priority of annotations", size = "s", easyClose = TRUE,
        footer = list(actionButton("md_edit_rankings", "Save edits"), modalButton("Dismiss")),
        lapply(1:10, function(x) {
          fluidRow(
            column(
              width = 8,
              textInput(paste0("md_rankings_text", x), NULL, value = rankings[x, 1], placeholder = "Empty")
            ),
            column(
              width = 4,
              numericInput(paste0("md_rankings_prio", x), NULL, value = rankings[x, 2], min = 0, max = 10)
            ),
          )
        })
      )
    )
  })

  observeEvent(input$md_edit_rankings, {
    sapply(1:10, function(x) {
      rankings[x, 1] <<- toupper(input[[paste0("md_rankings_text", x)]])
      rankings[x, 2] <<- input[[paste0("md_rankings_prio", x)]]
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

        out_dub <- data.frame(
          "nClust" = nclust,
          "Cluster_ID" = dub_dat$mergeID,
          "Ion_mode" = dub_dat$ionmode,
          "Adductor" = dub_dat$add,
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
              selection = list(selected = finddup(out_dub, rankings))
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

  #TODO
  observeEvent(input$run_pca1, {
    if (!is.null(rv$activeFile)) {
      if (input$selectpca1 == "Unsaved data") {
        data <- rv$tmpData
        seq <- rv$tmpSequence
      } else {
        selectchoices <- paste(1:length(rv$data), ": ", names(rv$data))
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
            paste0("CV in class ", x, ": ", classcv[x], "</br>")
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
    selectchoices <- paste(1:length(rv$data), ": ", names(rv$data))
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
          paste0("CV in class ", x, ": ", classcv[x], "</br>")
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
        lapply(1:length(input$dt_drift_panel_rows_selected), function(i) {
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
        lapply(1:length(input$dt_boxplot_panel_rows_selected), function(i) {
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
      for (i in 1:length(input$dt_drift_panel_rows_selected)) {
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
      for (i in 1:length(input$dt_boxplot_panel_rows_selected)) {
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

  observe({ 
    if (!is.null(rv$activeFile)) {
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

      if (sum(seq$labels %in% "QC") > 0) {
        qccv <- paste0("CV in QC samples: ",
                      round(cvmean(dat[seq[, 1] %in% "QC"]), 2), "</br>")
      } else {
        qccv <- "No QC in dataset </br>"
      }

      if (sum(!is.na(sclass)) > 0) {
        classcv <- sapply(sort(unique(sclass)), function(x) {
          round(cvmean(sdata[sclass %in% x]), 2)
        })
        classcv <- sapply(seq_along(classcv), function(x) {
          paste0("CV in class ", x, ": ", classcv[x], "</br>")
        })
      } else {
        classcv <- NULL
      }
      text <- c(qccv, classcv)
      output$title <- renderText({
        HTML("<h3>", names(rv$data)[rv$activeFile], "</h3>")
      })
      output$info_ui <- renderUI({
        HTML(nrow(dat) - 1, " features.<br>", ncol(dat[seq[, 1] %in% "Sample"]), " samples.<br>", ncol(dat[seq[, 1] %in% "QC"]), " QC samples.<br>", ncol(dat[seq[, 1] %in% "Blank"]), " Blank samples.<br>", "<br>", sample_mv, " missing values in Samples<br>", qc_mv, " missing values in QC samples<br>", blank_mv, " missing values in Blank samples<br><br>")
      })
      output$cvinfo_ui <- renderUI({
        HTML(text)
      })
    }
  })

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
      normalizedQCs <- data[, sequence[, 1] %in% "QC"]
      rv$tmpData <- data
      rv$tmpSequence <- sequence
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", 
            choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = 
            list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(title = "Success", text = paste0("Data normalized using ", input$normMethod), type = "success")
      # Plot variance in QC samples before and after normalization
      # output$beforeNormalization <- renderPlot({
      #   boxplot(log2(qualityControls), main = "Before Normalization", xlab = "Metabolite", ylab = "Intensity")
      # })
      # output$afterNormalization <- renderPlot({
      #   boxplot(log2(normalizedQCs), main = "After Normalization", xlab = "Metabolite", ylab = "Intensity")
      # })

      # showModal(
      #   modalDialog(
      #     title = "Assess data quality", size = "m",
      #     fluidRow(
      #         column(6, plotOutput("beforeNormalization", height = 280, width = "100%")),
      #         column(6, plotOutput("afterNormalization", height = 280, width = "100%"))
      #     ),
      #     footer = list(actionButton("saveNormalization", "Save changes"), modalButton("Dismiss"))
      #   )
      # )
    }
  })

  observeEvent(input$saveNormalization, {
    if(is.null(rv$tmpData)) {
      showNotification("Normalize first", type = "error")
    } else {
      if (input$newFileNorm) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpData
        rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_normalized")
        initializeVariables()
      } else {
        rv$data[[rv$activeFile]] <- rv$tmpData
        rv$sequence[[rv$activeFile]] <- rv$tmpSequence
        names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_normalized")
      }
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Normalized with", input$normMethod, " method\n")
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL   
    }
  })

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
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(session, title = "Success", text = "Data transformed.", type = "success")
    }
  })

  observeEvent(input$saveTransform, {
    if(is.null(rv$tmpData)) {
      showNotification("Transform first", type = "error")
    } else {
      rv$data[[length(rv$data) + 1]] <- rv$tmpData
      rv$sequence[[length(rv$sequence) + 1]] <- rv$sequence[[rv$activeFile]]
      names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_transformed")
      initializeVariables()
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL
    }
  })

  # Statistics
  observeEvent(input$testType, { #TODO move this to functions file? 
    sequence <- rv$sequence[[rv$activeFile]]
    enable("selectTest")
    switch(input$testType,
      GroupsUnpaired = {
        if(!any(complete.cases(sequence[, 4]))) {
          sendSweetAlert(session, "Oops!", "Invalid test. Provide information on different groups/conditions.", type = "error")
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
          contrasts <- generate_contrasts(matrix(valid_combinations)) # matrix bc if it's only 1 combination, valid_combinations is not a matrix and generate_contrasts fails

          updateCheckboxGroupInput(session, "contrasts", choices = contrasts, selected = NULL)
        } else {
          sendSweetAlert(session, "Oops!", "Invalid test. No paired samples or time points in dataset.", type = "error")
          disable("selectTest")
        }
      },
      CompareToReference = {
        if(!any(complete.cases(sequence[, 4]))) { # or if only one group
          sendSweetAlert(session, "Oops!", "Invalid test. Provide information on different groups/conditions.", type = "error")
          disable("selectTest")
        } else {
          updateSelectInput(session, "referenceGroup", label = NULL, choices = na.omit(sequence[, 'class']))
        }
      },
      {
         print('default')
      }
    )
  }, ignoreInit = TRUE
  )

  observeEvent(input$selectTest, { #TODO move this to functions file?
    data <- rv$data[[rv$activeFile]]
    sequence <- rv$sequence[[rv$activeFile]]
    switch(input$testType, 
      GroupsUnpaired = {
        if(input$group1 == input$group2) {
          sendSweetAlert(session, "Oops!", "Choose different groups to compare.", type="error")
        } else {
          results <- groupComparison(data, sequence, c(input$group1, input$group2))
          st$results[[rv$activeFile]][[length(st$results[[rv$activeFile]])+1]] <- results
          names(st$results[[rv$activeFile]])[length(st$results[[rv$activeFile]])] <- paste0(input$group1, "_vs_", input$group2)
        }
      },
      GroupsMultipleTime = { # multi-level in limma 
        data <- data[sequence[, 1] %in% c("Name", "Sample")]
        sequence <- sequence[sequence[, 1] %in% c("Sample"), ]

        group_time <- getGroupTime(sequence)
        group_time <- factor(group_time, exclude = NA)
        paired <- factor(sequence[, 'paired'],  exclude = NA)
        results <- pairedAnalysis(data, group_time, input$contrasts, paired)

        st$results[[rv$activeFile]] <- results
      },
      CompareToReference = {
        data <- data[sequence[, 1] %in% c("Name", "Sample")]
        groups <- sequence[complete.cases(sequence[, 4]), 4]
        results <- referenceGroupComparison(data, as.numeric(input$referenceGroup), groups)
        st$results[[rv$activeFile]][[length(st$results[[rv$activeFile]])+1]] <- results
      },
      {
         print('default')
      }
    )
    # Render one table for each contrast
        output$results_ui <- renderUI({
          lapply(seq_along(st$results[[rv$activeFile]]), function(i) {
            fluidRow(
              column(12, strong(names(st$results[[rv$activeFile]])[i])),
              column(12, box(width = NULL, DTOutput(paste0("results", i))))
            )
          })
        })
        lapply(seq_along(st$results[[rv$activeFile]]), function(i) {
          output[[paste0("results", i)]] <- renderDT({
            st$results[[rv$activeFile]][[i]]
          })
        })
    #enable("runTest")
  })

  observeEvent(input$send_polystest, {
    # Select Name and Samples (no QCs)
    sequence <- rv$sequence[[rv$activeFile]]
    tdata <- rv$data[[rv$activeFile]][, sequence[, 1] %in% c("Name",  "Sample")]
    tseq <- sequence[sequence[, 1] %in% c("Name",  "Sample"), ]
    groups <- factor(tseq[, 4], exclude = NA)
    NumReps <- max(table(groups))
    NumCond <- length(levels(groups))
    groups <- levels(groups)

    tdata <- addEmptyCols(tdata, tseq, groups, NumReps)
    PolySTestMessage <- toJSON(list(
      numrep=NumReps, numcond=NumCond, grouped=F,
      firstquantcol=2, expr_matrix=as.list(as.data.frame(tdata))
    ))
    js$send_message(url="http://computproteomics.bmb.sdu.dk:443/app_direct/PolySTest/", 
                    dat=PolySTestMessage, tool="PolySTest")
  })

  observeEvent(input$send_vsclust, {
    sequence <- rv$sequence[[rv$activeFile]]
    tdata <- rv$data[[rv$activeFile]][, sequence[, 1] %in% c("Name",  "Sample")]
    tseq <- sequence[sequence[, 1] %in% c("Name",  "Sample"), ]
    groups <- factor(tseq[, 4], exclude = NA)
    NumReps <- max(table(groups))
    NumCond <- length(levels(groups))
    groups <- levels(groups)

    tdata <- addEmptyCols(tdata, tseq, groups, NumReps)

    VSClustMessage <- toJSON(list(
      numrep=NumReps, numcond=NumCond, grouped=F, 
      modsandprots=F, expr_matrix=as.list(as.data.frame(tdata))
    ))
    js$send_message(url="http://computproteomics.bmb.sdu.dk/app_direct/VSClust/",
                    dat=VSClustMessage, tool="VSClust")
  })
})
