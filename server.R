shinyServer(function(session, input, output) {
  options(shiny.maxRequestSize = 30 * 1024^2)

  ## General data
  rv <- reactiveValues(data = list(), sequence = list(), selectedFile = NULL, 
                  tmpData = NULL, tmpSequence = NULL, 
                  choices = NULL, drift_plot_select = 1)
  
  ## Statistics data
  st <- reactiveValues(stats = list(), sequence = list(), results = list(), groups = list(), 
                  numReps = c(), numCond = c(), comparisons = list(), colcomp = list())

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
  })

  initializeVariables <- function() {
    st$stats[[length(st$stats) + 1]] <- data.frame()
    st$sequence[[length(st$stats) + 1]] <- data.frame()
    st$results[[length(st$results) + 1]] <- data.frame()
    st$groups[[length(st$groups) + 1]] <- NA
    st$numReps[[length(st$numReps) + 1]] <- NA
    st$numCond[[length(st$numCond) + 1]] <- NA
    st$comparisons[[length(st$comparisons) + 1]] <- vector("character")
    st$colcomp[[length(st$colcomp) + 1]] <- vector("numeric")
  }

  observeEvent(input$inputFile, { 
    dat <- read.csv(input$inputFile$datapath, header = 1, stringsAsFactors = F, check.names = FALSE)
    labels <- identifyLabels(dat)
    batch <- NA
    order <- NA
    class <- NA
    time <- NA
    paired <- NA
    rv$tmpData <- NULL
    rv$tmpSequence <- NULL
    initializeVariables()
    rv$sequence[[length(rv$sequence) + 1]] <- data.frame(labels, batch, order, class, time, paired)
    rv$data[[length(rv$data) + 1]] <- dat
    names(rv$data)[length(rv$data)] <- substr(input$inputFile$name, 1, nchar(input$inputFile$name) - 4)
    rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
    updateSelectInput(session, "selectDataset", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca2", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateTabItems(session, "tabs", selected = "Datainput")
    show("buttons")
  })

  observeEvent(input$inputSequence, {
    shinyCatch( {
      inputSequence <- read.csv(input$inputSequence$datapath, header = 1, stringsAsFactors = FALSE)
      colnames(inputSequence) <- tolower(colnames(inputSequence))
      inputSequence <- checkSequence(inputSequence)
    },
      blocking_level = 'message'
    )
    sequence <- rv$sequence[[rv$selectedFile]]
    labeledSequence <- data.frame("sample" = row.names(sequence), sequence)
    inputSequence["sample"] <- lapply(inputSequence["sample"], as.character)
    sequence <- left_join(labeledSequence[, 1:2], inputSequence, by = "sample")
    row.names(sequence) <- sequence[, 1]
    sequence <- sequence[, -1]
    rv$sequence[[rv$selectedFile]] <- sequence

    if(any(complete.cases(sequence[, 'class']))) {
      data <- rv$data[[rv$selectedFile]][, sequence[, 1] %in% c("Name",  "Sample")]
      sequence <- sequence[sequence[, 1] %in% c("Name",  "Sample"), ]
      group_data()
      group_timeUI(colnames(data), sequence[, 5])
    }
  })

  observeEvent(input$reuseSequence, {
    inputSequence <- read.csv(input$inputSequence$datapath, header = 1, stringsAsFactors = FALSE)
    colnames(inputSequence) <- tolower(colnames(inputSequence))
    inputSequence <- checkSequence(inputSequence)
    sequence <- rv$sequence[[rv$selectedFile]]
    labeledSequence <- data.frame("sample" = row.names(sequence), sequence)
    inputSequence["sample"] <- lapply(inputSequence["sample"], as.character)
    labeledSequence <- left_join(labeledSequence[, 1:2], inputSequence, by = "sample")
    row.names(labeledSequence) <- labeledSequence[, 1]
    labeledSequence <- labeledSequence[, -1]
    rv$sequence[[rv$selectedFile]] <- labeledSequence
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
        lapply(seq(ncol(rv$data[[rv$selectedFile]])), function(x) {
          fluidRow(
            column(
              width = 9,
              textInput(paste0("seq_edit_name", x), NULL, value = colnames(rv$data[[rv$selectedFile]])[x])
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
        lapply(seq(unique(rv$sequence[[rv$selectedFile]][, 4][!is.na(rv$sequence[[rv$selectedFile]][, 4])])), function(x) {
          group <- sort(unique(rv$sequence[[rv$selectedFile]][, 4][!is.na(rv$sequence[[rv$selectedFile]][, 4])]))[x]
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
    sapply(seq(ncol(rv$data[[rv$selectedFile]])), function(x) {
      isolate(colnames(rv$data[[rv$selectedFile]])[x] <- input[[paste0("seq_edit_name", x)]])
      isolate(row.names(rv$sequence[[rv$selectedFile]])[x] <- input[[paste0("seq_edit_name", x)]])
    })
    keep <- sapply(seq(ncol(rv$data[[rv$selectedFile]])), function(x) input[[paste0("seq_edit_keep", x)]])
    rv$data[[rv$selectedFile]] <- rv$data[[rv$selectedFile]][, keep]
    rv$sequence[[rv$selectedFile]] <- rv$sequence[[rv$selectedFile]][keep, ]
    removeModal()
  })

  observeEvent(input$group_edit_confirm, {
    groups <- rv$sequence[[rv$selectedFile]][, 4]
    sapply(seq(ncol(rv$data[[rv$selectedFile]])), function(x) {
      if(!is.na(groups[x])) {
        isolate(rv$sequence[[rv$selectedFile]][, 4][x] <- paste(rv$sequence[[rv$selectedFile]][, 4][x], input[[paste0("edit_nickname", groups[x])]], sep = ": "))
      }
    })
    removeModal()
    show("sequence_panel")
  })

  observeEvent(input$example, {
    # Lipidomics
    dat <- read.csv("./csvfiles/Eva pos export from profinder.csv", stringsAsFactors = FALSE)
    labels <- identifyLabels(dat)
    labels[5] <- "-"
    batch <- c(NA, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 1, 1, 1, 1)
    order <- c(NA, NA, NA, NA, NA, 11, 16, 13, 15, 14, 6, 8, 5, 4, 10, 9, 3, NA, NA, NA, 1, 2, 7, 12, 17)
    class <- c(NA, NA, NA, NA, NA, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, NA, NA, NA, NA, NA, NA, NA, NA)
    time <- rep(NA, length(class))
    paired <- rep(NA, length(class))
    rv$sequence[[length(rv$sequence) + 1]] <- data.frame(labels, batch, order, class, time, paired)
    rv$data[[length(rv$data) + 1]] <- dat
    names(rv$data)[length(rv$data)] <- "Lipidomics_pos"
    initializeVariables()
    
    # Metabolomics
    dat <- read.csv("./csvfiles/Woz export from mzmine pos.csv", stringsAsFactors = FALSE)
    labels <- identifyLabels(dat)
    labels[1] <- "Name"
    batch <- c(NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, NA)
    order <- c(NA, NA, NA, 10, 28, 46, 19, 49, 47, 48, 2, 37, 1, 57, 42, 18, 15, 23, 56, 30, 13, 52, 44, 36, 51, 41, 5, 27, 25, 39, 17, 11, 33, 21, 43, 40, 32, 20, 12, 45, 35, 8, 29, 4, 7, 9, 50, 24, 53, 38, 54, 55, 6, 22, 34, 16, 14, 26, 3, 31, NA, NA, NA, NA)
    class <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2, 6, 5, 2, 4, 2, 1, 3, 1, 3, 3, 4, 5, 3, 4, 1, 1, 6, 5, 2, 5, 6, 3, 6, 2, 5, 3, 4, 2, 1, 4, 1, 5, 6, 1, 2, 4, 5, 6, 6, 5, 1, 2, 4, 6, 3, NA, NA, NA, NA)
    time <- rep(NA, length(class))
    paired <- rep(NA, length(class))
    rv$sequence[[length(rv$sequence) + 1]] <- data.frame(labels, batch, order, class, time, paired)
    rv$data[[length(rv$data) + 1]] <- dat
    names(rv$data)[length(rv$data)] <- "Metabolomics_pos"
    rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
    initializeVariables()
    updateSelectInput(session, "selectDataset", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca1", choices = rv$choices)
    updateSelectInput(session, "selectpca2", choices = rv$choices)
    updateTabItems(session, "tabs", selected = "Datainput")
    show("buttons")
  })

  # Update selected data
  observeEvent(input$selectDataset, ignoreInit = TRUE, { 
    rv$selectedFile <- which(rv$choices %in% input$selectDataset)
    output$seq_table <- renderDT(rv$sequence[[rv$selectedFile]], extensions = c('FixedHeader', 'Responsive'), server = F, 
          editable = T, selection = 'none', options = list(pageLength = nrow(rv$sequence[[rv$selectedFile]]), 
          fixedHeader = TRUE))
    output$diboxtitle <- renderText(names(rv$data[rv$selectedFile]))
    output$dttable <- renderDT(rv$data[[rv$selectedFile]], rownames = FALSE, options = list(scrollX = TRUE, 
              scrollY = "700px", pageLength = 20))
    output$dt_drift_panel <- renderDT(rv$data[[rv$selectedFile]][rv$sequence[[rv$selectedFile]][, 1] %in% "Name"], rownames = FALSE, 
              options = list(autoWidth = TRUE, scrollY = "700px", pageLength = 20))
    output$dt_boxplot_panel <- renderDT(rv$data[[rv$selectedFile]][rv$sequence[[rv$selectedFile]][, 1] %in% "Name"], rownames = FALSE, 
              options = list(autoWidth = TRUE, scrollY = "700px", pageLength = 20))

    if (sum(rv$sequence[[rv$selectedFile]][, 1] %in% "Name") == 1) {
      internalStandards <- findInternalStandards(rv$data[[rv$selectedFile]][rv$sequence[[rv$selectedFile]][, 1] %in% "Name"])
      updateCheckboxGroupInput(session, "isChoose", choices = internalStandards, selected = internalStandards)
    }

    # Statistics
    output$results_table <- renderDT(st$results[[rv$selectedFile]], rownames = TRUE, options = list(scrollX = TRUE,
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
        paste0(names(rv$data[rv$selectedFile]), "_seq.csv")
      },
      content = function(file) {
        write.csv(cbind("sample" = rownames(rv$sequence[[rv$selectedFile]]), rv$sequence[[rv$selectedFile]]), file, row.names = FALSE) # TODO
      }
    )
    updateCheckboxGroupInput(session, "export_xml_list", choices = rv$choices, selected = NULL)
    updateSelectInput(session, "md_select", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "drift_select", choices = c("None", rv$choices))
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
    rv$selectedFile <- names(rv$data)[length(rv$data)]
    rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
    updateSelectInput(session, "selectDataset", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca1", choices = rv$choices)
    updateSelectInput(session, "selectpca2", choices = rv$choices)
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
    if (!is.null(rv$selectedFile)) {
      labs <- sapply(1:ncol(rv$data[[rv$selectedFile]]), function(x) {
        isolate(input[[paste0("labels", rv$selectedFile, x)]])
      })

      if (sum(labs == "Name") > 1) {
        sendSweetAlert(title = "Error", text = "Only one name Label allowed", type = "error")
      } else {
        lapply(1:ncol(rv$data[[rv$selectedFile]]), function(x) {
          isolate(rv$sequence[[rv$selectedFile]][x, 1] <- input[[paste0("labels", rv$selectedFile, x)]])
          isolate(rv$sequence[[rv$selectedFile]][x, 2] <- input[[paste0("bat", rv$selectedFile, x)]])
          isolate(rv$sequence[[rv$selectedFile]][x, 3] <- input[[paste0("ord", rv$selectedFile, x)]])
          isolate(rv$sequence[[rv$selectedFile]][x, 4] <- input[[paste0("cla", rv$selectedFile, x)]])
        })
      }
    }
  })

  observeEvent(input$extractAdducts, {
    sequence <- rv$sequence[[rv$selectedFile]]
    data <- rv$data[[rv$selectedFile]]
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
    rv$sequence[[rv$selectedFile]] <- sequence
    rv$data[[rv$selectedFile]] <- data
  })

  observeEvent(input$blankFiltrate, {
    if (is.null(rv$selectedFile)) {
      showNotification("No data", type = "error")
    } else if (!"QC" %in% rv$sequence[[rv$selectedFile]][, 1]) {
      showNotification("Data must have atleast 1 QC", type = "error")
    } else if (!"Blank" %in% rv$sequence[[rv$selectedFile]][, 1]) {
      showNotification("Data must have atleast 1 Blank", type = "error")
    } else if (sum(rv$sequence[[rv$selectedFile]][, 1] %in% "Name") != 1) {
      showNotification("Data must have exactly 1 \"Name\" column", type = "error")
    } else {
      bfseq <- rv$sequence[[rv$selectedFile]]

      bfdat <- blankFiltration(
        dat = rv$data[[rv$selectedFile]],
        seq = bfseq,
        xbf = input$xbf,
        keepis = input$bfkeepis
      )
      if (input$bfdiscard) {
        bfdat <- bfdat[!bfseq[, 1] %in% "Blank"]
        bfseq <- bfseq[!bfseq[, 1] %in% "Blank", ]
      }
      rv$tmpData <- bfdat
      rv$tmpSequence <- bfseq
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(
        session = session,
        title = "Success",
        text = paste0(nrow(rv$data[[rv$selectedFile]]) - nrow(rv$tmpData), " features was removed"),
        type = "success"
      )
    }
  })

  observeEvent(input$bfsave, {
    if (is.null(rv$tmpData)) {
      showNotification("Blank filtrate first", type = "error")
    } else {
      if (input$bfnewsave) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpData
        rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$selectedFile], "_", input$xbf, "xb")
        initializeVariables()
      } else {
        rv$data[[rv$selectedFile]] <- rv$tmpData
        rv$sequence[[rv$selectedFile]] <- rv$tmpSequence
        names(rv$data)[rv$selectedFile] <- paste0(names(rv$data)[rv$selectedFile], "_", input$xbf, "xb")
      }
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      updateSelectInput(session, "selectDataset", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca2", choices = rv$choices, selected = input$selectpca2)
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL
    }
  })

  observeEvent(input$is, {
    if (is.null(rv$selectedFile)) {
      showNotification("No data", type = "error")
    } else if (sum(rv$sequence[[rv$selectedFile]][, 1] %in% "Name") != 1) {
      showNotification("Data must have exactly 1 \"Name\" column", type = "error")
    } else if (is.null(input$isChoose)) {
      showNotification("No internal standards selected", type = "error")
    } else {
      isseq <- rv$sequence[[rv$selectedFile]]
      isdat <- isfunc(
        dat = rv$data[[rv$selectedFile]],
        seq = isseq,
        is = input$isChoose,
        method = input$ismethod,
        qc = input$isqc
      )
      rv$tmpData <- isdat
      rv$tmpSequence <- isseq
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
    }
  })

  observeEvent(input$is_optimize, {
    seq <- rv$sequence[[rv$selectedFile]]
    dat <- rv$data[[rv$selectedFile]]
    isop <- isopti(
      dat = dat,
      seq = seq,
      is = input$isChoose,
      method = input$ismethod,
      qc = input$isqc
    )
    updateCheckboxGroupInput(session, "isChoose", selected = isop)
  })

  observeEvent(input$issave, {
    if (is.null(rv$tmpData)) {
      showNotification("IS normalize first", type = "error")
    } else {
      if (input$isnewsave) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpData
        rv$sequence[[length(rv$sequence) + 1]] <- rv$tmpSequence
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$selectedFile], "_is")
        initializeVariables()
      } else {
        rv$data[[rv$selectedFile]] <- rv$tmpData
        rv$sequence[[rv$selectedFile]] <- rv$tmpSequence
        names(rv$data)[rv$selectedFile] <- paste0(names(rv$data)[rv$selectedFile], "_is")
      }
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      updateSelectInput(session, "selectDataset", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca2", choices = rv$choices, selected = input$selectpca2)
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL
    }
  })

  observeEvent(input$isremove, {
    dat <- rv$data[[rv$selectedFile]]
    rmdat <- rv$data[[rv$selectedFile]][rv$sequence[[rv$selectedFile]][, 1] %in% "Name"]
    dat <- dat[!grepl("\\(IS\\)", toupper(rmdat[ , 1])), ]
    rv$data[[rv$selectedFile]] <- dat
    updateCheckboxGroupInput(session, "isChoose", choices = character(0), selected = NULL)
  })

  observeEvent(input$mvf_run, {
    if (is.null(rv$selectedFile)) {
      sendSweetAlert(session, "No data", type = "error")
    } else {
      mvf_seq <- rv$sequence[[rv$selectedFile]]
      method <- input$mvf_conditions
      if(("in group" %in% method) & !any(complete.cases(mvf_seq[, 4]))) {
        shinyalert("Error!", "Group information needed.")
      }
      else if(is.null(method)) {
        shinyalert("Error!", "No method selected.")
      }
      else {
        mvf_dat <- cutoffrm(
          rv$data[[rv$selectedFile]],
          mvf_seq,
          input$mvf_cutoff,
          method
        )
        rv$tmpData <- mvf_dat
        rv$tmpSequence <- mvf_seq
        updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
        output$dttable <- renderDataTable(rv$tmpData, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
        sendSweetAlert(
          title = "Success",
          text = paste0(nrow(rv$data[[rv$selectedFile]]) - nrow(rv$tmpData), " feature(s) removed"),
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
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$selectedFile], "_mvr")
        initializeVariables()
      } else {
        rv$data[[rv$selectedFile]] <- rv$tmpData
        rv$sequence[[rv$selectedFile]] <- rv$tmpSequence
        names(rv$data)[rv$selectedFile] <- paste0(names(rv$data)[rv$selectedFile], "_mvr")
      }
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      updateSelectInput(session, "selectDataset", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca2", choices = rv$choices, selected = input$selectpca2)
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL
    }
  })

  observeEvent(input$imp_run, {
    if (is.null(rv$selectedFile)) {
      showNotification("No data", type = "error")
    } else if (sum(rv$sequence[[rv$selectedFile]][, 1] %in% "Sample") < 1) {
      showNotification("Data must have at least one Sample", type = "error")
    } else {
      imp_seq <- rv$sequence[[rv$selectedFile]]
      imp_dat <- imputation(
        dat = rv$data[[rv$selectedFile]],
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
        text = paste0(sum(is.na(rv$data[[rv$selectedFile]]) | rv$data[[rv$selectedFile]] == 0) - sum(is.na(rv$tmpData) | rv$tmpData == 0), " missing values was imputed"),
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
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$selectedFile], "_imp")
        initializeVariables()
      } else {
        rv$data[[rv$selectedFile]] <- rv$tmpData
        rv$sequence[[rv$selectedFile]] <- rv$tmpSequence
        names(rv$data)[rv$selectedFile] <- paste0(names(rv$data)[rv$selectedFile], "_imp")
      }
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      updateSelectInput(session, "selectDataset", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca2", choices = rv$choices, selected = input$selectpca2)
      rv$tmpData <- NULL
      rv$tmpSequence <- NULL
    }
  })

  observeEvent(input$dc_method, {
    if (input$dc_method == "QC-RFSC (random forrest)") {
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
    dc_dat <- rv$data[[rv$selectedFile]]
    dc_seq <- rv$sequence[[rv$selectedFile]]  
    dat_qc <- dc_dat[, dc_seq[, 1] %in% "QC"]

    if(any(colSums(!is.na(dat_qc)) != nrow(dat_qc))) {
      sendSweetAlert(session = session, title = "Error", text = "QCs cannot have missing values.", type = "error")
    }
    else {
      dc_dat <- driftcorrection(
        dat = dc_dat,
        seq = dc_seq,
        method = input$dc_method,
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
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$selectedFile], "_dc")
        initializeVariables()

      } else {
        rv$data[[rv$selectedFile]] <- rv$tmpData
        rv$sequence[[rv$selectedFile]] <- rv$tmpSequence
        names(rv$data)[rv$selectedFile] <- paste0(names(rv$data)[rv$selectedFile], "_dc")
      }
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      updateSelectInput(session, "selectDataset", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca2", choices = rv$choices, selected = input$selectpca2)
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

  observeEvent(input$md_run, {
    seq1 <- rv$sequence[[rv$selectedFile]]
    dat1 <- rv$data[[rv$selectedFile]]
    sel <- which(rv$choices %in% input$md_select)
    seq2 <- rv$sequence[[sel]]
    dat2 <- rv$data[[sel]]

    if(names(rv$data)[rv$selectedFile] == names(rv$data)[sel]) {
      sendSweetAlert(session = session, title = "Error", text = "Cannot merge a dataset with itself.", type = "error")
    }
    else {

      if (sum(seq1[, 1] %in% c("Adduct_pos", "Adduct_neg")) != 1 || sum(seq2[, 1] %in% c("Adduct_pos", "Adduct_neg")) != 1) {
        sendSweetAlert(session = session, title = "Error", text = "Each dataset must contain exactly one adduct column labeled in the sequence file.", type = "error")
      } else if (ncol(dat1) != ncol(dat2)) {
        sendSweetAlert(session = session, title = "Error", text = "Datasets must have the same number of columns", type = "error")
      } else {
        merged_dat <<- mergeDatasets(dat1, seq1,
          dat2,
          seq2,
          input$md_ppm,
          input$md_rt
        )
        clustn <- data.frame(table(merged_dat$mergeID))
        dub_clust <- clustn[clustn$Freq > 1, ]
        dub_dat <- merged_dat[merged_dat$mergeID %in% dub_clust[, 1], ]
        dub_qc <- dub_dat[, seq1[, 1] %in% "QC"]
        cov <- cv(dub_qc)
        nclust <- sapply(dub_dat$mergeID, function(x) {
          table(dub_dat$mergeID)[names(table(dub_dat$mergeID)) == x]
        })

        out_dub <- data.frame(
          "nClust" = nclust,
          "Cluster_ID" = dub_dat$mergeID,
          "Ion_mode" = dub_dat$ionmode,
          "Adductor" = dub_dat$add,
          "Name" = dub_dat[, which(seq1[, 1] %in% "Name")],
          "RT" = dub_dat[, which(seq1[, 1] %in% "RT")],
          "Mass" = dub_dat[, which(seq1[, 1] %in% "Mass")],
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
    dat <<- merged_dat[-duplicates, ]
    output$dttable <- renderDataTable(dat, rownames = F, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
    removeModal()
    confirmSweetAlert(session, inputId = "md_newsave", title = "Merge complete", text = "Save as new file?", btn_labels = c("No", "Yes"), type = "success")
  })

  observeEvent(input$md_newsave, {
    if (isTRUE(input$md_newsave)) {
      rv$data[[length(rv$data) + 1]] <- dat[, seq(ncol(dat) - 2)]
      rv$sequence[[length(rv$sequence) + 1]] <- rv$sequence[[rv$selectedFile]]
      names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$selectedFile], "_merged")
      initializeVariables()
    } else if (isFALSE(input$md_newsave)) {
      rv$data[[rv$selectedFile]] <- dat[, seq(ncol(dat) - 2)]
      names(rv$data)[rv$selectedFile] <- paste0(names(rv$data)[rv$selectedFile], "_merged")
    }
    rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
    updateSelectInput(session, "selectDataset", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca2", choices = rv$choices, selected = input$selectpca2)
  })

  observeEvent(list(input$selectpca1, input$bf, input$updateseq, input$is, input$inputSequence, input$imp_run), ignoreInit = T, {
    if (!is.null(rv$selectedFile)) {
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
        output$info1 <- renderUI({
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
      output$info2 <- renderUI({
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
      if (is.null(rv$selectedFile)) {
        p("No data")
      } else if (input$drift_select != "None" && nrow(rv$data[[rv$selectedFile]]) != nrow(rv$data[[which(rv$choices %in% input$drift_select)]])) {
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
      if (is.null(rv$selectedFile)) {
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
              data = rv$data[[rv$selectedFile]][input$dt_drift_panel_rows_selected[my_i], ],
              seq = rv$sequence[[rv$selectedFile]]
            )
          })
        })
        if (input$drift_select != "None") {
          local({
            my_i <- i
            output[[paste0("driftplotoutput2", my_i)]] <- renderPlot({
              driftplot(
                data = rv$data[[which(rv$choices %in% input$drift_select)]][input$dt_drift_panel_rows_selected[my_i], ],
                seq = rv$sequence[[rv$selectedFile]]
              )
            })
          })
        }
      }
    } else if (rv$drift_plot_select == 2) {
      output$cvscatterplot <- renderPlot({
        cvscatterplot(
          data = rv$data[[rv$selectedFile]],
          data2 = rv$data[[which(rv$choices %in% input$drift_select)]],
          seq = rv$sequence[[rv$selectedFile]],
          name1 = names(rv$data)[rv$selectedFile],
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
              data = rv$data[[rv$selectedFile]][input$dt_boxplot_panel_rows_selected[my_i], ],
              seq = rv$sequence[[rv$selectedFile]],
              log = input$bloxplot_log,
              ylog = input$bloxplot_ylog
            )
          })
        })
      }
    }
  })

  observe({ 
    if (!is.null(rv$selectedFile)) {
      seq <- rv$sequence[[rv$selectedFile]]
      dat <- rv$data[[rv$selectedFile]]
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
        HTML("<h2>", names(rv$data)[rv$selectedFile], "</h2>", nrow(dat) - 1, " features.<br>", ncol(dat[seq[, 1] %in% "Sample"]), " samples.<br>", ncol(dat[seq[, 1] %in% "QC"]), " QC samples.<br>", ncol(dat[seq[, 1] %in% "Blank"]), " Blank samples.<br>", "<br>", sample_mv, " missing values in Samples<br>", qc_mv, " missing values in QC samples<br>", blank_mv, " missing values in Blank samples<br><br>")
      })
      output$cvinfo_ui <- renderUI({
        HTML(text)
      })
    }
  })

  
  group_timeUI <- function(names, time) {
    numCond <- st$numCond[[rv$selectedFile]]
    numReps <- st$numReps[[rv$selectedFile]]
    output$input_stats <- renderText({ 
      paste(numCond, " groups were detected<br/>", 
        paste("<i>Condition ", 1:numCond,":</i>", 
        sapply(0:(numCond-1), function(x) paste(names[(2:(numReps+1))+x*numReps],collapse=", ")),
        "<br/>",collapse=""), "</br>")
    })
    #TODO only IF it has group info!
    updateSelectInput(session, "group1", label = NULL, choices = append(st$groups[[rv$selectedFile]], "", after=0))
    updateSelectInput(session, "group2", label = NULL, choices = append(st$groups[[rv$selectedFile]], "", after=0))
    if(any(complete.cases(time))) {
      #TODO what if the groups dont all have the same times? Does it happen?
      #TODO add select input here instead? Don't show time if it doesnt have it
      updateSelectInput(session, "time1", label = NULL, choices = time)
      updateSelectInput(session, "time2", label = NULL, choices = time)
    }
  }
  #TODO do we need this?
  group_data <- function() {
    sequence <- rv$sequence[[rv$selectedFile]]
    groups <- factor(sequence[, 4], exclude = NA)
    numReps <- max(table(groups))
    groups <- levels(groups)
    numCond <- length(groups)
    st$groups[[rv$selectedFile]] <- groups
    st$numReps[[rv$selectedFile]] <- numReps
    st$numCond[[rv$selectedFile]] <- numCond
    #data <- addcols(data, sequence, groups, numReps)
    #return(dat)
  }

  observeEvent(input$norm, {
     if (is.null(rv$selectedFile)) {
      showNotification("No data", type = "error")
    } else if(input$norm_method == "QC (PQN)" & sum(rv$sequence[[rv$selectedFile]][, 1] %in% "QC") == 0) {
      sendSweetAlert(session = session, title = "Error", text = "No QC samples in dataset.", type = "error")
    } else {
      data <- rv$data[[rv$selectedFile]]
      sequence <- rv$sequence[[rv$selectedFile]]
      qualityControls <- data[, sequence[, 1] %in% "QC"] 
      normalizedData <- normalization(data, sequence, qualityControls, input$norm_method)
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
        boxplot(qualityControls, main = "Before Normalization", xlab = "Metabolite", ylab = "Intensity")
      })
      output$afterNormalization <- renderPlot({
        boxplot(normalizedQCs, main = "After Normalization", xlab = "Metabolite", ylab = "Intensity")
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
    names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$selectedFile], "_normalized")
    initializeVariables()
    rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
    updateSelectInput(session, "selectDataset", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca2", choices = rv$choices, selected = input$selectpca2)
    rv$tmpData <- NULL
    rv$tmpSequence <- NULL
    removeModal()
  })

  observeEvent(input$show_data, {
    if(input$group1==input$group2 & input$time1==input$time2) {
      shinyalert("Oops!", "Choose different groups or time points to compare.")
    } else if(input$group1 != "" & input$group2 != "") {
      group1 <- input$group1
      group2 <- input$group2
      time1 <- if(input$time1 == "") NA else input$time1
      time2 <- if(input$time2 == "") NA else input$time2
      data <- rv$data[[rv$selectedFile]]
      seq <- rv$sequence[[rv$selectedFile]]
      keepd <- seq[, 1] %in% "Name" | (seq[, 4] %in% c(group1, group2) & seq[, 5] %in% c(time1, time2))
      keeps <- seq[, 4] %in% c(group1, group2) & seq[, 5] %in% c(time1, time2)

      if(any(complete.cases(seq[, 6])))
        seq <- seq[keeps, 4:6]
      else if(any(complete.cases(seq[, 5]))) 
        seq <- seq[keeps, 4:5]
      else {
        seq <- data.frame(seq[keeps, 4], row.names=rownames(seq[keeps,]))
        colnames(seq) <- "group" 
      }

      comparison <- paste("Comparison ", paste("G", group1, sep=""), if(is.na(time1)) "" else paste("T", time1, sep=""), " vs. ", 
            paste("G", group2, sep=""), if(is.na(time2)) "" else paste("T", time2, sep=""))

      st$comparisons[[rv$selectedFile]] <- append(st$comparisons[[rv$selectedFile]], comparison)

      data <- data[, keepd] # first column = feature names
      st$stats[[rv$selectedFile]] <- data
      st$sequence[[rv$selectedFile]] <- seq
    }
  })

  # Statistics
  #TODO something about the missing values - keep, impute, remove?
  observeEvent(input$runTest, {
    sequence <- st$sequence[[rv$selectedFile]]
    data <- st$stats[[rv$selectedFile]]
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

    if(nrow(st$results[[rv$selectedFile]]) == 0) {
      st$colcomp[[rv$selectedFile]] <- c(ncol(results))
      st$results[[rv$selectedFile]] <- results 
    } else {
      st$colcomp[[rv$selectedFile]] <- append(st$colcomp[[rv$selectedFile]], ncol(results))
      st$results[[rv$selectedFile]] <- cbind(st$results[[rv$selectedFile]], results)      
    }

    sketch = htmltools::withTags(table(
      class = 'display',
        thead(
          tr(
            th('', style="text-align: center;"),
            lapply(1:length(st$comparisons[[rv$selectedFile]]), function(i) th(colspan = st$colcomp[[rv$selectedFile]][i], st$comparisons[[rv$selectedFile]][i], style = "text-align: center;border-left:thin solid;"))
          ),
          tr(
            th('Feature', style="text-align: center;"),
            lapply(colnames(st$results[[rv$selectedFile]]), th, style="text-align: center;border-left:thin solid;")
          )
        )
      ))

    output$results_table <- DT::renderDataTable(DT::datatable(st$results[[rv$selectedFile]],
                            options = list(scrollX=TRUE, 
                              columnDefs = list(list(width = '20%', targets = colnames(data))),
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