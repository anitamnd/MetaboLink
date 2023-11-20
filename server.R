shinyServer(function(session, input, output) {
  options(shiny.maxRequestSize = 30 * 1024^2)

  ## General
  rv <- reactiveValues(data = list(), sequence = list(), activeFile = NULL, 
                  tmpData = NULL, tmpSequence = NULL, 
                  choices = NULL, drift_plot_select = 1, info = vector("character"))

  ## Statistics
  st <- reactiveValues(stats = list(), sequence = list(), results = list(),
                  comparisons = list(), colcomp = list())

  rankings <- read.csv("./csvfiles/rankings.csv", stringsAsFactors = FALSE)

  observeEvent(list(c(input$sequence, input$example, input$inputFile)), {
      windowselect("sequence")
    }, ignoreInit = T
  )
  observeEvent(input$datatable, {
    windowselect("datatable")
  })
  observeEvent(input$pca_button, {
    windowselect("pca")
  })
  observeEvent(input$drift_button, {
    windowselect("drift")
  })
  observeEvent(input$export, {
    windowselect("export")
  })
  observeEvent(input$feature_button, {
    windowselect("feature")
  })
  observeEvent(input$info_button, {
    windowselect("info")
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
    st$results[[length(st$results) + 1]] <- data.frame()
    st$comparisons[[length(st$comparisons) + 1]] <- vector("character")
    st$colcomp[[length(st$colcomp) + 1]] <- vector("numeric")
  }

  output$input_stats <- renderText({
    sequence <- isolate(rv$sequence[[rv$activeFile]])
    paste("Selected:<br/>",
        "group ", input$group1, ": ", paste(rownames(sequence)[sequence[, 4] %in% input$group1], collapse = ", "),
        "<br/>group ", input$group2, ": ", paste(rownames(sequence)[sequence[, 4] %in% input$group2], collapse = ", "))
  })

  observeEvent(input$inputFile, {
    shinyCatch({
      inputFile <- read.csv(input$inputFile$datapath, header = 1, stringsAsFactors = F, check.names = FALSE)
      if(input$fileType == "Samples in rows") {
        inputFile <- t(inputFile)
      }
      labels <- identifyLabels(inputFile)
      checkColumns(colnames(inputFile), labels)
    },
      blocking_level = 'message'
    )
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
    sapply(seq(ncol(rv$data[[rv$activeFile]])), function(x) {
      isolate(colnames(rv$data[[rv$activeFile]])[x] <- input[[paste0("seq_edit_name", x)]])
      isolate(row.names(rv$sequence[[rv$activeFile]])[x] <- input[[paste0("seq_edit_name", x)]])
    })
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
  })

  # Update selected data
  observeEvent(input$selectDataset, ignoreInit = TRUE, { 
    rv$activeFile <- which(rv$choices %in% input$selectDataset)
    rows <- nrow(rv$data[[rv$activeFile]])
    cols <- ncol(rv$data[[rv$activeFile]])
    if(rows > 30)
      rows <- 30
    if(cols > 15)
      cols <- 15
    forVisuals <- rv$data[[rv$activeFile]][1:rows, 1:cols]

    output$seq_table <- renderDT(rv$sequence[[rv$activeFile]], extensions = c('FixedHeader', 'Responsive'), server = F, 
          editable = T, selection = 'none', options = list(pageLength = nrow(rv$sequence[[rv$activeFile]]), 
          fixedHeader = TRUE))
    output$diboxtitle <- renderText(names(rv$data[rv$activeFile]))
    output$dttable <- renderDT(forVisuals, rownames = FALSE, options = list(scrollX = TRUE, 
              scrollY = "700px"))
    output$dt_drift_panel <- renderDT(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"], rownames = FALSE, 
              options = list(autoWidth = TRUE, scrollY = "700px", pageLength = 20))
    output$dt_boxplot_panel <- renderDT(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"], rownames = FALSE, 
              options = list(autoWidth = TRUE, scrollY = "700px", pageLength = 20))

    if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Name") == 1) {
      internalStandards <- findInternalStandards(rv$data[[rv$activeFile]][rv$sequence[[rv$activeFile]][, 1] %in% "Name"])
      updateCheckboxGroupInput(session, "isChoose", choices = internalStandards, selected = internalStandards)
      enable("normalizeIS"); enable("optimizeIS"); enable("removeIS"); enable("saveIS")
      if(length(internalStandards) == 0) {
        output$notFoundIS <- renderText({"No internal standards found."})
        disable("normalizeIS"); disable("optimizeIS"); disable("removeIS"); disable("saveIS")
      } 
    }

    # Statistics
    updateSelectInput(session, "group1", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'class']))
    updateSelectInput(session, "group2", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'class']))
    updateSelectInput(session, "time1", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'time']))
    updateSelectInput(session, "time2", label = NULL, choices = na.omit(rv$sequence[[rv$activeFile]][, 'time']))
    output$results_table <- renderDT(st$results[[rv$activeFile]], rownames = TRUE, options = list(scrollX = TRUE,
              scrollY = TRUE, pageLength = 20))

  })

  observeEvent(rv$choices, {
    output$export_ui <- renderUI({
      box(
        title = ".csv", width = 4,
        lapply(1:length(rv$choices), function(x) {
          fluidRow(column(12, downloadLink(paste0("dwn", x), paste0(rv$choices[x], ".csv"))))
        }),
        fluidRow(column(3, actionButton("export_edit", "Edit", width = "100%")))
      )
    })

    output$export_metabo <- renderUI({
      box(
        title = ".csv for metaboanalyst", width = 4,
        lapply(1:length(rv$choices), function(x) {
          fluidRow(column(12, downloadLink(paste0("dwn_metabo", x), paste0(rv$choices[x], "_metabo.csv"))))
        })
      )
    })

    output$export_stats <- renderUI({
      box(
        title = "Statistics results", width = 4,
        lapply(1:length(rv$choices), function(x) {
          fluidRow(column(12, downloadLink(paste0("dwn_stats", x), paste0(rv$choices[x], "_results.csv"))))
        })
      )
    })

    output$export_settings <- renderUI({
      box(
        title = "Settings used in app", width = 4,
        lapply(1:length(rv$choices), function(x) {
          fluidRow(column(12, downloadLink(paste0("dwn_settings", x), paste0(rv$choices[x], ".txt"))))
        })
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
      output[[paste0("dwn_settings", x)]] <- downloadHandler(
        filename = function() {
          paste0(names(rv$data[x]), ".txt")
        },
        content = function(file) {
          write(rv$info[x], file)
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

    output$downloadSequence <- downloadHandler(
      filename <- function() {
        paste0(names(rv$data[rv$activeFile]), "_seq.csv")
      },
      content = function(file) {
        write.csv(cbind("sample" = rownames(rv$sequence[[rv$activeFile]]), rv$sequence[[rv$activeFile]]), file, row.names = FALSE) # TODO
      }
    )
    updateCheckboxGroupInput(session, "export_xml_list", choices = rv$choices, selected = NULL)
    updateSelectInput(session, "mergeFile", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "drift_select", choices = c("None", rv$choices))
    updateSelectInput(session, "selectDataset", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca2", choices = rv$choices, selected = rv$choices[length(rv$choices)])

  })

  observeEvent(input$export_edit, {
    showModal(
      modalDialog(
        title = "Edit datasets", size = "m", easyClose = TRUE,
        footer = list(actionButton("export_edit_confirm", "Confirm"), modalButton("Dismiss")),
        fluidRow(
          column(width = 10, h4("Names")),
          column(width = 2, style = "text-align: left;", h4("Keep"))
        ),
        lapply(1:length(rv$choices), function(x) {
          fluidRow(
            column(
              width = 10,
              textInput(paste0("export_edit_name", x), NULL, value = names(rv$data[x]), width = "100%")
            ),
            column(
              width = 2, style = "text-align: left;",
              prettyCheckbox(paste0("export_edit_keep", x), NULL, status = "info", value = TRUE)
            ),
          )
        }),
      )
    )
  })

  observeEvent(input$export_edit_confirm, {
    sapply(1:length(rv$choices), function(x) {        
        isolate(names(rv$data)[x] <- input[[paste0("export_edit_name", x)]])
    })
    keep <- sapply(1:length(rv$choices), function(x) input[[paste0("export_edit_keep", x)]])
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
    removeModal()
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

  observeEvent(input$updateSequence, {
    if (!is.null(rv$activeFile)) {
      labs <- sapply(1:ncol(rv$data[[rv$activeFile]]), function(x) {
        isolate(input[[paste0("labels", rv$activeFile, x)]])
      })

      if (sum(labs == "Name") > 1) {
        sendSweetAlert(title = "Error", text = "Only one name Label allowed", type = "error")
      } else {
        lapply(1:ncol(rv$data[[rv$activeFile]]), function(x) {
          isolate(rv$sequence[[rv$activeFile]][x, 1] <- input[[paste0("labels", rv$activeFile, x)]])
          isolate(rv$sequence[[rv$activeFile]][x, 2] <- input[[paste0("bat", rv$activeFile, x)]])
          isolate(rv$sequence[[rv$activeFile]][x, 3] <- input[[paste0("ord", rv$activeFile, x)]])
          isolate(rv$sequence[[rv$activeFile]][x, 4] <- input[[paste0("cla", rv$activeFile, x)]])
        })
      }
    }
  })

  observeEvent(input$extractAdducts, {
    sequence <- rv$sequence[[rv$activeFile]]
    data <- rv$data[[rv$activeFile]]
    adduct <- findAdduct(data, sequence)
    data <- cbind(data, adduct)
    ionMode <- switch(input$selectIonMode,
      "Positive" = "Adduct_pos",
      "Negative" = "Adduct_neg"
    )
    adduct <- c(ionMode, rep(NA, ncol(sequence)-1))
    sequence <- rbind(sequence, "adduct" = adduct)
    sequence[, 2] <- as.numeric(sequence[, 2])
    sequence[, 3] <- as.numeric(sequence[, 3])
    sequence[, 4] <- as.numeric(sequence[, 4])
    rv$sequence[[rv$activeFile]] <- sequence
    rv$data[[rv$activeFile]] <- data
  })

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
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
    }
  })

  observeEvent(input$optimizeIS, {
    sequence <- rv$sequence[[rv$activeFile]]
    data <- rv$data[[rv$activeFile]]
    optimized <- optimizeIS(data, sequence, input$isChoose, input$isMethod, input$normalizeQC)
    updateCheckboxGroupInput(session, "isChoose", selected = optimized)
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
    data <- rv$data[[rv$activeFile]]
    sequence <- rv$sequence[[rv$activeFile]]
    toRemove <- data[sequence[, 1] %in% "Name"]
    data <- data[!grepl("\\(IS\\)", toupper(toRemove[ , 1])), ]
    rv$data[[rv$activeFile]] <- data
    updateCheckboxGroupInput(session, "isChoose", choices = character(0), selected = NULL)
  })

  observeEvent(input$mvf_run, {
    if (is.null(rv$activeFile)) {
      sendSweetAlert(session, "No data", type = "error")
    } else {
      mvf_seq <- rv$sequence[[rv$activeFile]]
      method <- input$mvf_conditions
      if(("in group" %in% method) & !any(complete.cases(mvf_seq[, 4]))) {
        shinyalert("Error!", "Group information needed.")
      }
      else if(is.null(method)) {
        shinyalert("Error!", "No method selected.")
      }
      else {
        mvf_dat <- cutoffrm(rv$data[[rv$activeFile]], mvf_seq, input$mvf_cutoff, method)
        rv$tmpData <- mvf_dat
        rv$tmpSequence <- mvf_seq
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

  observeEvent(input$mvf_save, {
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
                input$mvf_cutoff, "% as threshold and method -", paste(input$mvf_conditions, collapse=", "), "\n")
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL   
    }
  })

  observeEvent(input$imp_run, {
    if (is.null(rv$activeFile)) {
      showNotification("No data", type = "error")
    } else if (sum(rv$sequence[[rv$activeFile]][, 1] %in% "Sample") < 1) {
      showNotification("Data must have at least one Sample", type = "error")
    } else {
      imp_seq <- rv$sequence[[rv$activeFile]]
      imp_dat <- imputation(
        dat = rv$data[[rv$activeFile]],
        seq = imp_seq,
        method = input$imp_method,
        minx = input$imp_minx,
        onlyqc = input$imp_onlyqc,
        remaining = input$imp_remaining
      )
      rv$tmpData <- imp_dat
      rv$tmpSequence <- imp_seq
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(
        title = "Success",
        text = paste0(sum(is.na(rv$data[[rv$activeFile]]) | rv$data[[rv$activeFile]] == 0) - sum(is.na(rv$tmpData) | rv$tmpData == 0), " missing values was imputed"),
        type = "success"
      )
    }
  })

  observeEvent(list(input$imp_method, input$imp_remaining), {
    if (input$imp_method == "KNN") {
      hide("imp_minx_hide")
      hide("imp_remaining_hide")
    } else {
      show("imp_remaining_hide")
    }

    if (input$imp_method == "Min/X" || input$imp_remaining == "Min/X") {
      show("imp_minx_hide")
    } else {
      hide("imp_minx_hide")
    }
  })

  observeEvent(input$imp_save, {
    if (is.null(rv$tmpData)) {
      showNotification("Imputate first", type = "error")
    } else {
      if(input$imp_newsave) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpData
        rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_imp")
        initializeVariables()
      } else {
        rv$data[[rv$activeFile]] <- rv$tmpData
        rv$sequence[[rv$activeFile]] <- rv$tmpSequence
        names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_imp")
      }
      rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Missing values imputation with", input$imp_method, "\n")
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL   
    }
  })

  observeEvent(input$dcMethod, {
    if (input$dcMethod == "QC-RFSC (random forrest)") {
      hide("dc_qcspan_hide")
      hide("dc_degree_hide")
      show("dc_ntree_hide")
    } else {
      hide("dc_ntree_hide")
      show("dc_qcspan_hide")
      show("dc_degree_hide")
    }
  })

  observeEvent(input$dc_run, {
    dc_dat <- rv$data[[rv$activeFile]]
    dc_seq <- rv$sequence[[rv$activeFile]]  
    dat_qc <- dc_dat[, dc_seq[, 1] %in% "QC"]

    if(any(colSums(!is.na(dat_qc)) != nrow(dat_qc))) {
      sendSweetAlert(session = session, title = "Error", text = "QCs cannot have missing values.", type = "error")
    }
    else {
      dc_dat <- driftcorrection(
        dat = dc_dat,
        seq = dc_seq,
        method = input$dcMethod,
        ntree = input$dc_ntree,
        QCspan = input$dc_qcspan
      )
    }
    rv$tmpData <- dc_dat
    rv$tmpSequence <- dc_seq
    updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
    output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
  })

  observeEvent(input$dc_save, {
    if (is.null(rv$tmpData)) {
      showNotification("Drift correct first", type = "error")
    } else {
      if (input$dc_newsave) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpData
        rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_dc")
        initializeVariables()

      } else {
        rv$data[[rv$activeFile]] <- rv$tmpData
        rv$sequence[[rv$activeFile]] <- rv$tmpSequence
        names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_dc")
      }
      rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Drift correction", input$dcMethod, "and", input$dc_ntree, "\n")
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL   
    }
  })

  observeEvent(input$md_rankings, {
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
    activeSequence <- rv$sequence[[rv$activeFile]]
    activeDataset <- rv$data[[rv$activeFile]]
    selected <- which(rv$choices %in% input$mergeFile)
    sequenceToMerge <- rv$sequence[[selected]]
    datasetToMerge <- rv$data[[selected]]

    if(names(rv$data)[rv$activeFile] == names(rv$data)[selected]) {
      sendSweetAlert(session = session, title = "Error", text = "Cannot merge a dataset with itself.", type = "error")
    }
    else {

      if (sum(activeSequence[, 1] %in% c("Adduct_pos", "Adduct_neg")) != 1 || sum(sequenceToMerge[, 1] %in% c("Adduct_pos", "Adduct_neg")) != 1) {
        sendSweetAlert(session = session, title = "Error", text = "Each dataset must contain exactly one adduct column labeled in the sequence file.", type = "error")
      } else if (ncol(activeDataset) != ncol(datasetToMerge)) {
        sendSweetAlert(session = session, title = "Error", text = "Datasets must have the same number of columns", type = "error")
      } else {
        mergedDatasets <<- mergeDatasets(activeDataset, activeSequence,
              datasetToMerge, sequenceToMerge, input$md_ppm, input$md_rt)
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

        showModal(
          modalDialog(
            title = "Select features to keep", size = "l",
            p(paste0(length(unique(dub_dat$mergeID))), " duplicate clusters found, of those ", paste0(length(unique(out_dub[out_dub[, 1] > 2, ][, 2]))), " consists of more than 2 features."),
            DTOutput("md_modal_dt"),
            footer = list(actionButton("md_modal_go", "Remove duplicates"), modalButton("Dismiss"))
          )
        )
      }
    }
  })

  observeEvent(input$md_modal_go, {
    duplicates <- as.numeric(rownames(md_dup[-input$md_modal_dt_rows_selected, ]))
    merged <<- mergedDatasets[-duplicates, ]
    output$dttable <- renderDataTable(merged, rownames = F, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
    removeModal()
    confirmSweetAlert(session, inputId = "md_newsave", title = "Merge complete", text = "Save as new file?", btn_labels = c("No", "Yes"), type = "success")
  })

  observeEvent(input$md_newsave, {
    if (isTRUE(input$md_newsave)) {
      rv$data[[length(rv$data) + 1]] <- merged[, seq(ncol(merged) - 2)]
      rv$sequence[[length(rv$sequence) + 1]] <- rv$sequence[[rv$activeFile]]
      names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_merged")
      initializeVariables()
    } else if (isFALSE(input$md_newsave)) {
      rv$data[[rv$activeFile]] <- merged[, seq(ncol(merged) - 2)]
      names(rv$data)[rv$activeFile] <- paste0(names(rv$data)[rv$activeFile], "_merged")
    }
    rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Positive and negative mode merged: M/z tolerance ppm", input$md_ppm, "and RT tolerance", input$md_rt, "\n")
    rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
  })

  observeEvent(list(input$selectpca1, input$blankFiltrate, input$updateSequence, input$is, input$inputSequence, input$imp_run), ignoreInit = T, {
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
        pca <- pcaplot(sdata, sclass)
        output$plotpca1 <- renderPlotly(pca)

        if (sum(seq[, 1] %in% "QC") > 0) {
          qccv <- paste0("CV in QC samples: ", round(cvmean(data[seq[, 1] %in% "QC"]), 2), "</br>")
        } else {
          qccv <- "No QC in dataset </br>"
        }

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

  observeEvent(input$selectpca2, ignoreInit = T, {
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
      pca <- pcaplot(sdata, sclass)
      output$plotpca2 <- renderPlotly(pca)

      if (sum(seq$labels %in% "QC") > 0) {
        qccv <- paste0("CV in QC samples: ", round(cvmean(data[seq[, 1] %in% "QC"]), 2), "</br>")
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
      output$info_ui <- renderUI({
        HTML("<h2>", names(rv$data)[rv$activeFile], "</h2>", nrow(dat) - 1, " features.<br>", ncol(dat[seq[, 1] %in% "Sample"]), " samples.<br>", ncol(dat[seq[, 1] %in% "QC"]), " QC samples.<br>", ncol(dat[seq[, 1] %in% "Blank"]), " Blank samples.<br>", "<br>", sample_mv, " missing values in Samples<br>", qc_mv, " missing values in QC samples<br>", blank_mv, " missing values in Blank samples<br><br>")
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
        
      # Plot variance in QC samples before and after normalization
      output$beforeNormalization <- renderPlot({
        boxplot(log2(qualityControls), main = "Before Normalization", xlab = "Metabolite", ylab = "Intensity")
      })
      output$afterNormalization <- renderPlot({
        boxplot(log2(normalizedQCs), main = "After Normalization", xlab = "Metabolite", ylab = "Intensity")
      })

      showModal(
        modalDialog(
          title = "Assess data quality", size = "m",
          fluidRow(
              column(6, plotOutput("beforeNormalization", height = 280, width = "100%")),
              column(6, plotOutput("afterNormalization", height = 280, width = "100%"))
          ),
          footer = list(actionButton("saveNormalization", "Save changes"), modalButton("Dismiss"))
        )
      )
    }
  })

  observeEvent(input$saveNormalization, {
    rv$data[[length(rv$data) + 1]] <- rv$tmpData
    rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
    names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_normalized")
    initializeVariables()
    rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
    rv$info[length(rv$data)] <- paste(ifelse(is.na(rv$info[rv$activeFile]), "", rv$info[rv$activeFile]), "Normalized with", input$normMethod, " method\n")
    removeModal()
    rv$tmpData <- NULL
    rv$tmpSequence <- NULL   
  })

  observeEvent(input$transform, {
    data <- rv$data[[rv$activeFile]]
    sequence <- rv$sequence[[rv$activeFile]]

    transformed <- transformation(data, sequence, input$logTransform, input$scaling)
    data[, sequence[, 1] %in% c("QC", "Sample")] <- transformed
    rv$tmpData <- data

    output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
  })

  observeEvent(input$saveTransform, {
    rv$data[[length(rv$data) + 1]] <- rv$tmpData
    rv$sequence[[length(rv$sequence) + 1]] <- rv$sequence[[rv$activeFile]]
    names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$activeFile], "_transformed")
    initializeVariables()
    #updates()
    rv$tmpData <- NULL
    rv$tmpSequence <- NULL   
  })

  # Statistics

  observeEvent(input$selectTest, {
    if(input$group1==input$group2 & input$time1==input$time2) {
      shinyalert("Oops!", "Choose different groups or time points to compare.")
    } else if(input$group1 != "" & input$group2 != "") {
      group1 <- input$group1
      group2 <- input$group2
      time1 <- if(input$time1 == "") NA else input$time1
      time2 <- if(input$time2 == "") NA else input$time2
      data <- rv$data[[rv$activeFile]]
      sequence <- rv$sequence[[rv$activeFile]]
      keep <- sequence[, 1] %in% "Name" | (sequence[, 4] %in% c(group1, group2) & sequence[, 5] %in% c(time1, time2))
      keepSeq <- sequence[, 4] %in% c(group1, group2) & sequence[, 5] %in% c(time1, time2)

      if(any(complete.cases(sequence[, 6])))
        sequence <- sequence[keepSeq, 4:6]
      else if(any(complete.cases(sequence[, 5]))) 
        sequence <- sequence[keepSeq, 4:5]
      else {
        sequence <- data.frame(sequence[keepSeq, 4], row.names=rownames(sequence[keepSeq,]))
        colnames(sequence) <- "group" 
      }

      comparison <- paste("Comparison ", paste("G", group1, sep=""), if(is.na(time1)) "" else paste("T", time1, sep=""), " vs. ", 
            paste("G", group2, sep=""), if(is.na(time2)) "" else paste("T", time2, sep=""))

      st$comparisons[[rv$activeFile]] <- append(st$comparisons[[rv$activeFile]], comparison)

      data <- data[, keep] # first column = feature names
      st$stats[[rv$activeFile]] <- data
      st$sequence[[rv$activeFile]] <- sequence

      enable("runTest")
    }
  })

  observeEvent(input$runTest, {
    sequence <- st$sequence[[rv$activeFile]]
    data <- st$stats[[rv$activeFile]]
    results <- data.frame()
    rownames(data) <- data[, 1]
    data <- data[, -1]

    if(ncol(sequence) == 0) {
      shinyalert("Error!", "No dataset selected.")
    } else if(ncol(sequence) == 1) {
      results <- groupsTest(data, sequence)
    }
    else {
      isPaired <- input$isPaired
      results <- timeSeriesTest(data, sequence, isPaired)
    } 
    
    if(nrow(st$results[[rv$activeFile]]) == 0) {
      st$colcomp[[rv$activeFile]] <- c(ncol(results))
      st$results[[rv$activeFile]] <- results 
    } else {
      st$colcomp[[rv$activeFile]] <- append(st$colcomp[[rv$activeFile]], ncol(results))
      st$results[[rv$activeFile]] <- cbind(st$results[[rv$activeFile]], results)      
    }

    sketch = htmltools::withTags(table(
      class = 'display',
        thead(
          tr(
            th('', style="text-align: center;"),
            lapply(1:length(st$comparisons[[rv$activeFile]]), function(i) th(colspan = st$colcomp[[rv$activeFile]][i], st$comparisons[[rv$activeFile]][i], style = "text-align: center;border-left:thin solid;"))
          ),
          tr(
            th('Feature', style="text-align: center;"),
            lapply(colnames(st$results[[rv$activeFile]]), th, style="text-align: center;border-left:thin solid;")
          )
        )
      ))

    output$results_table <- DT::renderDataTable(DT::datatable(st$results[[rv$activeFile]],
                            options = list(scrollX=TRUE, 
                              #columnDefs = list(list(width = '20%', targets = colnames(data))),
                              autowidth=TRUE),
                              container = sketch))

    # TODO check for a better way to do this
    lapply(1:length(rv$choices), function(x) {
      if(!(length(st$comparisons[[x]]) == 0)) {
        comparisons <- st$comparisons[[x]]
        colcomp <- st$colcomp[[x]]
        hrow <- c()
        for(i in 1:length(comparisons)) {
          hrow <- append(hrow, comparisons[i])
          hrow <- append(hrow, rep("", colcomp[i]-1))
        }
        outdata <- rbind(hrow, st$results[[x]])

        output[[paste0("dwn_stats", x)]] <- downloadHandler(
          filename = function() {
            paste0(names(rv$data[x]), "_results.csv")
          },
          content = function(file) {
            write.csv(outdata, file, row.names = TRUE)
          }
        )
      }
    })
  })
})