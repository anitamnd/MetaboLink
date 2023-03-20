library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(DT)
library(dplyr)
library(plotly)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(impute)
library(randomForest)
library(shinyWidgets)
library(writexl)
library(igraph)
source("functions.R")
source("plotfunctions.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "JLspec",
    titleWidth = 400
  ),

  #####
  # Sidebar -------------------------------------------------------------------
  #

  dashboardSidebar(
    width = "400",
    useShinyjs(),
    tags$style(HTML(".panel-primary {color: #000000;}")),
    fluidPage(
      fluidRow(
        selectizeInput("selectdata1", "Active dataset", choices = NULL, width = "100%",
                        options = list(placeholder = "Please uploade a file to start"))
      ),
      bsCollapse(
        id = "menu", multiple = FALSE, open = "Data input",
        bsCollapsePanel("Data input",
          style = "primary",
          fluidRow(
            style = "padding: 0px;",
            column(12, fileInput("in_file1", "Select file", accept = c("txt/csv", "text/comma-seperated-values, text/plain", ".csv"),
                      width = "100%"), style = "padding: 0px;")
          ),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, bsButton("reset", "Reset input", width = "100%"), style = "padding-left:0px;"),
            column(6, bsButton("example", "Load example", width = "100%"), style = "padding-left:0px;")
          )
        ),
        bsCollapsePanel("Blank filtration",
          style = "primary",
          fluidRow(
            style = "padding: 0px;",
            column(12,
              sliderInput("xbf", "Signal strength above blank", 0, 10, 5, step = 0.1, width = "100%"),
              style = "padding: 0px"
            )
          ),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, bsButton("bf", "Blank filtrate", width = "100%"), style = "padding-left:0px;"),
            column(6)
          ),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, bsButton("bfsave", "Save", width = "100%"), style = "padding-left:0px;"),
            column(6)
          ),
          splitLayout(
            style = "margin-right: 0px;",
            checkboxInput("bfdiscard", label = "Discard blank", value = T, width = "100%"),
            checkboxInput("bfnewsave", "Save as new file", value = T, width = "100%")
          ),
          splitLayout(
            style = "margin-right: 0px;",
            checkboxInput("bfkeepis", "Keep IS", value = T, width = "100%"),
            NULL
          )
        ),
        bsCollapsePanel("Missing value filtration",
          style = "primary",
          fluidRow(
            style = "padding: 0px;",
            column(12,
              sliderInput("mvf_cutoff", "Minimum percentage of messured values", 0, 100, 80, step = 5, width = "100%"),
              style = "padding: 0px"
            )
          ),
          fluidRow(column(
            6,
            prettyCheckboxGroup("mvf_conditions", "", choices = c("in QC", "in class", "entire data"))
          )),
          fluidRow(column(
            6,
            prettyCheckbox("mvf_newsave", "Save as new file", value = T, width = "100%")
          )),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, bsButton("mvf_run", "Run", width = "100%"), style = "padding-left:0px;"),
            column(6, bsButton("mvf_save", "Save", width = "100%"), style = "padding-left:0px;")
          )
        ),
        bsCollapsePanel("IS normalization",
          style = "primary",
          fluidRow(
            selectInput("ismethod", "Method", choices = c("Nearest RT", "Same lipid structure"), selected = "Nearest RT", width = "100%")
          ),
          fluidRow(
            checkboxGroupInput("isChoose", NULL, choices = NULL, selected = NULL, inline = FALSE)
          ),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, bsButton("is", "Is normalize", width = "100%"), style = "padding-left:0px;"),
            column(6, bsButton("is_optimize", "optimize", width = "100%"), style = "padding-left:0px;")
          ),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, bsButton("issave", "Save", width = "100%"), style = "padding-left:0px;"),
            column(6)
          ),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, checkboxInput("isqc", "Normalize QC", value = T, width = "100%"), style = "padding: 0px; margin-top: -10px; margin-left: 10px; margin-right: -10px;"),
            column(6, checkboxInput("isnewsave", "Save as new file", value = T, width = "100%"), style = "padding: 0px; margin-top: -10px; margin-left: 10px; margin-right: -10px;")
          )
        ),
        bsCollapsePanel("Imputation",
          style = "primary",
          fluidRow(selectInput("imp_method", "Imputation method", choices = c("KNN", "Min/X", "Median"), width = "100%")),
          fluidRow(hidden(div(id = "imp_remaining_hide", selectInput("imp_remaining", "Remaining missing values", choices = c("zero", "Min/X", "Median"), width = "100%")))),
          fluidRow(hidden(div(id = "imp_minx_hide", sliderInput("imp_minx", "Divide min by", min = 1, max = 10, value = 1, step = 1, width = "100%")))),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, prettyCheckbox("imp_onlyqc", "Only imputate QC")),
            column(6)
          ),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, checkboxInput("imp_newsave", "Save as new file", value = T, width = "100%"), style = "padding: 0px; margin-top: -10px; margin-left: 10px; margin-right: -10px;")
          ),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, bsButton("imp_run", "Run", width = "100%"), style = "padding-left:0px;"),
            column(6, bsButton("imp_save", "Save", width = "100%"), style = "padding-left:0px;")
          )
        ),
        bsCollapsePanel("Drift correction",
          style = "primary",
          fluidRow(selectInput("dc_method", "Signal correction method", choices = c("QC-RFSC (random forrest)", "QC-RLSC (robust LOESS)"), width = "100%")),
          fluidRow(div(id = "dc_ntree_hide", sliderInput("dc_ntree", "ntree", min = 100, max = 1000, value = 500, step = 100, width = "100%"))),
          fluidRow(hidden(div(id = "dc_qcspan_hide", sliderInput("dc_qcspan", "QCspan", min = 0.2, max = 0.75, value = 0.7, step = 0.05, width = "100%")))),
          fluidRow(hidden(div(id = "dc_degree_hide", sliderInput("dc_degree", "degree", min = 0, max = 2, value = 2, step = 1, width = "100%")))),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, bsButton("dc_run", "Run", width = "100%"), style = "padding-left:0px;"),
            column(6)
          ),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, bsButton("dc_save", "Save", width = "100%"), style = "padding-left:0px;"),
            column(6)
          ),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, checkboxInput("dc_newsave", "Save as new file", value = T, width = "100%"), style = "padding: 0px; margin-top: -10px; margin-left: 10px; margin-right: -10px;")
          )
        ),
        bsCollapsePanel("Merge datasets",
          style = "primary",
          fluidRow(column(12, selectInput("md_select", "Select dataset to merge with", choices = NULL, width = "100%"))),
          fluidRow(
            column(6, numericInput("md_ppm", "M/z tolerance ppm", min = 0, value = 10)),
            column(6, numericInput("md_rt", "RT tolerance", min = 0, value = 0.1, step = 0.01))
          ),
          fluidRow(column(6, bsButton("md_rankings", "Edit priorities", width = "100%"))),
          fluidRow(column(6, bsButton("md_run", "Run", width = "100%")))
        )
      )
    ),
    sidebarMenu()
  ),

  #####
  # Main Body -----------------------------------------------------------------
  #

  dashboardBody(
    tags$head(tags$style(".modal-sm{ width:300px}
                         .modal-lg{ width:1200px}")),
    fluidRow(hidden(div(
      id = "buttons", style = "padding-bottom: 49px",
      column(3, bsButton("sequence",
        label = "Sequence",
        icon = icon("user"),
        style = "default",
        block = T
      )),
      column(3, bsButton("datatable",
        label = "Data table",
        icon = icon("spinner", class = "spinner-box"),
        style = "default",
        block = T
      )),
      column(3, dropdownButton(
        inputId = "plot2", circle = F, width = "100%",
        label = "Explore data",
        bsButton("pca_button", label = "PCA", icon = icon("thumbs-up"), block = T),
        bsButton("drift_button", label = "Feature drift", block = T),
        bsButton("feature_button", label = "Feature Viewer", block = T),
        bsButton("info_button", label = "Dataset-Info", block = T)
      )),
      tags$style(type = "text/css", "#plot2 {width:100%}"),
      column(3, bsButton("export",
        label = "Export",
        icon = icon("thumbs-up"),
        style = "default",
        block = T
      )),
    ))),
    fluidRow(
      hidden(
        div(
          id = "sequence_panel",
          column(
            width = 8,
            box(
              title = textOutput("diboxtitle"), width = NULL,
              fluidRow(
                column(
                  width = 4,
                  h4("Sample")
                ),
                column(
                  width = 2,
                  h4("Label")
                ),
                column(
                  width = 2,
                  h4("Batch")
                ),
                column(
                  width = 2,
                  h4("Order")
                ),
                column(
                  width = 2,
                  h4("Class")
                )
              ),
              uiOutput("sequi")
            )
          ),
          column(
            width = 4,
            box(
              width = NULL, title = "Upload sequence file", status = "danger",
              fileInput("in_seq", "Select file", accept = c("txt/csv", "text/comma-seperated-values,text/plain", ".csv"), width = "100%"),
              column(6, actionButton("updateseq", label = "Update", width = "100%")), column(6, actionButton("reuseseq", label = "Re-use sequence", width = "100%"))
            ),
            box(
              width = NULL, title = "Extract adducts from name",
              radioGroupButtons("add_mode", "Select ionmode", c("Positive", "Negative"), justified = TRUE),
              actionButton("add_run", "Run", width = 100)
            ),
            box(
              width = NULL, title = "Edit data columns",
              actionButton("seq_edit", "Edit", width = 100)
            ),
            box(
              width = NULL, title = "Class nicknames",
              actionButton("class_edit", "Edit", width = 100)
            ),
            box(
              width = NULL, title = "Download sequence file",
              downloadButton("seq_download", "Download", width = 100)
            )
          )
        )
      )
    ),
    fluidRow(
      hidden(
        div(
          id = "datatable_panel",
          column(
            width = 12,
            box(
              width = NULL,
              DTOutput("dttable")
            )
          )
        )
      )
    ),
    fluidRow(
      hidden(
        div(
          id = "pca_panel",
          fluidPage(
            fluidRow(
              column(6, selectInput("selectpca1", "", choices = NULL, width = "100%")),
              column(6, selectInput("selectpca2", "", choices = NULL, width = "100%"))
            ),
            fluidRow(
              tabBox(
                tabPanel(title = "PCA", plotlyOutput("plotpca1")),
                tabPanel(title = "QC-drift")
              ),
              tabBox(tabPanel(title = "PCA", plotlyOutput("plotpca2")))
            ),
            fluidRow(
              tabBox(tabPanel(title = "Første panel", htmlOutput("info1"))),
              tabBox(tabPanel(title = "second", htmlOutput("info2")))
            )
          )
        )
      )
    ),
    fluidRow(
      hidden(
        div(
          id = "export_panel",
          uiOutput("export_ui"),
          uiOutput("export_metabo"),
          box(title = ".xlsx", width = 4, column(
            12,
            fluidRow(checkboxGroupInput("export_xml_list", "Choose sheets", choices = NULL, selected = NULL)),
            fluidRow(downloadButton("export_xml", "Export combined .xlsx"))
          ))
        )
      )
    ),
    fluidRow(
      hidden(
        div(
          id = "boxplot_panel",
          column(3, box(width = NULL, DTOutput("dt_boxplot_panel"))),
          column(
            9, box(width = NULL, fluidRow(
              column(
                6,
                radioButtons(
                  inputId = "bloxplot_log",
                  label = "Log",
                  choices = c("None", "ln", "log2", "log10"),
                  selected = "None",
                  inline = TRUE
                )
              ),
              column(6, radioButtons(
                inputId = "bloxplot_ylog",
                label = "Y axis log",
                choices = c("None", "log2", "log10"),
                selected = "None",
                inline = TRUE
              ))
            )),
            uiOutput("boxplot_ui")
          )
        )
      )
    ),
    fluidRow(
      hidden(
        div(
          id = "drift_panel",
          column(3, box(width = NULL, DTOutput("dt_drift_panel"))),
          column(
            9, box(
              width = NULL,
              fluidRow(
                column(4, selectizeInput("drift_select", "Select dataset to compare with", choices = NULL, width = "100%", options = list(placeholder = "Select file"))),
                column(2, style = "margin-top: 25px;", bsButton("drift_1", label = "Individual", block = TRUE)),
                column(2, style = "margin-top: 25px;", bsButton("drift_2", label = "CV variation", block = TRUE)),
                column(2, style = "margin-top: 25px;", bsButton("drift_3", label = "CV distribution", block = TRUE))
              ),
            ),
            uiOutput("drift_ui")
          )
        )
      )
    ),
    fluidRow(
      hidden(
        div(
          id = "info_panel",
          column(12, box(width = NULL, uiOutput("info_ui"), htmlOutput("cvinfo_ui")))
        )
      )
    ),
    fluidRow(
      div(
        id = "welcome_panel",
        column(12, box(width = NULL, includeHTML("intro_text.html")))
      )
    )
  )
)

server <- function(session, input, output) {
  options(shiny.maxRequestSize = 30 * 1024^2)

  # Global variables
  rv <- reactiveValues(data = list(), seq = list(), si = NULL, tmp = NULL,
              tmpseq = NULL, choices = NULL, drift_plot_select = 1)
  rankings <- read.csv("./csvfiles/rankings.csv", stringsAsFactors = FALSE)

  #####
  # Dynamic render rules-------------------------------------------------------
  #

  # observeEvent("", {
  #  hide("sequence_panel")
  #  hide("datatable_panel")
  #  hide("plot_panel")
  #  hide("export_panel")
  # }, once = TRUE)

  observeEvent(list(c(input$sequence, input$example, input$in_file1)),
    {
      windowselect("sequence")
    },
    ignoreInit = T
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

  ## Observes file input and creates a new dataset from input.
  observeEvent(input$in_file1, {
    try(dat <- read.csv(input$in_file1$datapath, header = 1, stringsAsFactors = F, check.names = FALSE))
    lab <- identifylabels(dat)
    batch <- NA
    order <- NA
    class <- NA
    rv$seq[[length(rv$seq) + 1]] <- data.frame(lab, batch, order, class)
    rv$data[[length(rv$data) + 1]] <- dat
    names(rv$data)[length(rv$data)] <- substr(input$in_file1$name, 1, nchar(input$in_file1$name) - 4)
    rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
    updateSelectInput(session, "selectdata1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca2", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateTabItems(session, "tabs", selected = "Datainput")
    show("buttons")
    # show("sequence_panel")
  })

  observeEvent(input$in_seq, {
    try(nseq <- read.csv(input$in_seq$datapath, header = 1, stringsAsFactors = FALSE))
    nseq <- nseq[, c("sample", "batch", "order", "class")]
    seq <- rv$seq[[rv$si]]
    imseq <- data.frame("sample" = row.names(seq), seq)
    imseq <- left_join(imseq[, 1:2], nseq, by = "sample")
    row.names(imseq) <- imseq[, 1]
    imseq <- imseq[, -1]
    rv$seq[[rv$si]] <- imseq
  })
  observeEvent(input$reuseseq, {
    try(nseq <- read.csv(input$in_seq$datapath, header = 1, stringsAsFactors = FALSE))
    nseq <- nseq[, c("sample", "batch", "order", "class")]
    seq <- rv$seq[[rv$si]]
    imseq <- data.frame("sample" = row.names(seq), seq)
    imseq <- left_join(imseq[, 1:2], nseq, by = "sample")
    row.names(imseq) <- imseq[, 1]
    imseq <- imseq[, -1]
    rv$seq[[rv$si]] <- imseq
  })
  observeEvent(input$seq_edit, {
    showModal(
      modalDialog(
        title = "Edit colunms", size = "s", easyClose = TRUE,
        footer = list(actionButton("seq_edit_confirm", "Confirm"), modalButton("Dismiss")),
        fluidRow(
          column(width = 9, h4("Colunm name")),
          column(width = 3, style = "text-align: left;", h4("Keep"))
        ),
        lapply(seq(ncol(rv$data[[rv$si]])), function(x) {
          fluidRow(
            column(
              width = 9,
              textInput(paste0("seq_edit_name", x), NULL, value = colnames(rv$data[[rv$si]])[x])
            ),
            column(
              width = 3, style = "text-align: center;",
              prettyCheckbox(paste0("seq_edit_keep", x), NULL, status = "info", value = T)
            ),
          )
        }),
      )
    )
  })

  observeEvent(input$class_edit, {
    showModal(
      modalDialog(
        title = "Edit class Nicknames", size = "s", easyClose = TRUE,
        footer = list(actionButton("class_edit_confirm", "Confirm"), modalButton("Dismiss")),
        fluidRow(
          column(width = 3, h4("Class")),
          column(width = 9, h4("Nickname"))
        ),
        lapply(seq(unique(rv$seq[[rv$si]][, 4][!is.na(rv$seq[[rv$si]][, 4])])), function(x) {
          fluidRow(
            column(width = 2, h5(sort(unique(rv$seq[[rv$si]][, 4][!is.na(rv$seq[[rv$si]][, 4])]))[x])),
            column(
              width = 10,
              textInput(paste0("edit_nickname", x), NULL, value = NULL)
            ),
          )
        }),
      )
    )
  })

  ## Duplicates not allowed
  observeEvent(input$seq_edit_confirm, {
    sapply(seq(ncol(rv$data[[rv$si]])), function(x) {
      isolate(colnames(rv$data[[rv$si]])[x] <- input[[paste0("seq_edit_name", x)]])
      isolate(row.names(rv$seq[[rv$si]])[x] <- input[[paste0("seq_edit_name", x)]])
    })
    keep <- sapply(seq(ncol(rv$data[[rv$si]])), function(x) input[[paste0("seq_edit_keep", x)]])
    rv$data[[rv$si]] <- rv$data[[rv$si]][, keep]
    rv$seq[[rv$si]] <- rv$seq[[rv$si]][keep, ]
    removeModal()
  })

  observeEvent(input$class_edit_confirm, {
    sapply(seq(ncol(rv$data[[rv$si]])), function(x) {
      isolate(colnames(rv$data[[rv$si]])[x] <- input[[paste0("seq_edit_name", x)]])
      isolate(row.names(rv$seq[[rv$si]])[x] <- input[[paste0("seq_edit_name", x)]])
    })
    keep <- sapply(seq(ncol(rv$data[[rv$si]])), function(x) input[[paste0("seq_edit_keep", x)]])
    rv$data[[rv$si]] <- rv$data[[rv$si]][, keep]
    rv$seq[[rv$si]] <- rv$seq[[rv$si]][keep, ]
    removeModal()
  })

  observeEvent(input$example, {
    dat <- read.csv("Eva pos export from profinder.csv", stringsAsFactors = FALSE)
    lab <- identifylabels(dat)
    lab[5] <- "-"
    batch <- c(NA, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 1, 1, 1, 1)
    order <- c(NA, NA, NA, NA, NA, 11, 16, 13, 15, 14, 6, 8, 5, 4, 10, 9, 3, NA, NA, NA, 1, 2, 7, 12, 17)
    class <- c(NA, NA, NA, NA, NA, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, NA, NA, NA, NA, NA, NA, NA, NA)
    rv$seq[[length(rv$seq) + 1]] <- data.frame(lab, batch, order, class)
    rv$data[[length(rv$data) + 1]] <- dat
    names(rv$data)[length(rv$data)] <- "Lipidomics_pos"
    dat <- read.csv("./csvfiles/Woz export from mzmine pos.csv", stringsAsFactors = FALSE)
    lab <- identifylabels(dat)
    lab[1] <- "Name"
    batch <- c(NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, NA)
    order <- c(NA, NA, NA, 10, 28, 46, 19, 49, 47, 48, 2, 37, 1, 57, 42, 18, 15, 23, 56, 30, 13, 52, 44, 36, 51, 41, 5, 27, 25, 39, 17, 11, 33, 21, 43, 40, 32, 20, 12, 45, 35, 8, 29, 4, 7, 9, 50, 24, 53, 38, 54, 55, 6, 22, 34, 16, 14, 26, 3, 31, NA, NA, NA, NA)
    class <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2, 6, 5, 2, 4, 2, 1, 3, 1, 3, 3, 4, 5, 3, 4, 1, 1, 6, 5, 2, 5, 6, 3, 6, 2, 5, 3, 4, 2, 1, 4, 1, 5, 6, 1, 2, 4, 5, 6, 6, 5, 1, 2, 4, 6, 3, NA, NA, NA, NA)
    rv$seq[[length(rv$seq) + 1]] <- data.frame(lab, batch, order, class)
    rv$data[[length(rv$data) + 1]] <- dat
    names(rv$data)[length(rv$data)] <- "Metabolomics_pos"
    rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
    updateSelectInput(session, "selectdata1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca1", choices = rv$choices)
    updateSelectInput(session, "selectpca2", choices = rv$choices)
    updateTabItems(session, "tabs", selected = "Datainput")
    show("buttons")
    # show("sequence_panel")
  })

  ## Observes selected data
  observeEvent(input$selectdata1, ignoreInit = TRUE, {
    # Updates what data is selected.
    rv$si <- which(rv$choices %in% input$selectdata1)
    output$sequi <- renderUI({
      lapply(1:ncol(rv$data[[rv$si]]), function(x) {
        fluidRow(
          column(
            width = 4,
            p(strong(colnames(rv$data[[rv$si]])[x]))
          ),
          column(
            width = 2,
            selectizeInput(paste0("lab", rv$si, x), NULL, choices = c("Name", "Blank", "QC", "Sample", "RT", "Mass", "Adduct_pos", "Adduct_neg", "-"), selected = rv$seq[[rv$si]][x, 1])
          ),
          column(
            width = 2,
            numericInput(paste0("bat", rv$si, x), NULL, value = rv$seq[[rv$si]][x, 2], min = 0, max = 300)
          ),
          column(
            width = 2,
            numericInput(paste0("ord", rv$si, x), NULL, value = rv$seq[[rv$si]][x, 3], min = 0, max = 300)
          ),
          column(
            width = 2,
            numericInput(paste0("cla", rv$si, x), NULL, value = rv$seq[[rv$si]][x, 4], min = 0, max = 300)
          )
        )
      })
    })
    output$diboxtitle <- renderText(names(rv$data[rv$si]))
    output$dttable <- renderDT(rv$data[[rv$si]], rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
    output$dt_drift_panel <- renderDT(rv$data[[rv$si]][rv$seq[[rv$si]][, 1] %in% "Name"], rownames = FALSE, options = list(
      autoWidth = TRUE, scrollY = "700px", pageLength = 20
    ))
    output$dt_boxplot_panel <- renderDT(rv$data[[rv$si]][rv$seq[[rv$si]][, 1] %in% "Name"], rownames = FALSE, options = list(
      autoWidth = TRUE, scrollY = "700px", pageLength = 20
    ))
    if (sum(rv$seq[[rv$si]][, 1] %in% "Name") == 1) {
      is <- findis(rv$data[[rv$si]][rv$seq[[rv$si]][, 1] %in% "Name"])
      updateCheckboxGroupInput(session, "isChoose", choices = is, selected = is)
    }
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
      seq <- rv$seq[[x]]
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

    output$seq_download <- downloadHandler(
      filename <- function() {
        paste0(names(rv$data[rv$si]), "_seq.csv")
      },
      content = function(file) {
        write.csv(cbind("sample" = rownames(rv$seq[[rv$si]]), rv$seq[[rv$si]][, -1]), file, row.names = FALSE)
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
    rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
    updateSelectInput(session, "selectdata1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
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

  observeEvent(input$updateseq, {
    if (!is.null(rv$si)) {
      labs <- sapply(1:ncol(rv$data[[rv$si]]), function(x) {
        isolate(input[[paste0("lab", rv$si, x)]])
      })

      if (sum(labs == "Name") > 1) {
        sendSweetAlert(title = "Error", text = "Only one name Label allowed", type = "error")
      } else {
        lapply(1:ncol(rv$data[[rv$si]]), function(x) {
          isolate(rv$seq[[rv$si]][x, 1] <- input[[paste0("lab", rv$si, x)]])
          isolate(rv$seq[[rv$si]][x, 2] <- input[[paste0("bat", rv$si, x)]])
          isolate(rv$seq[[rv$si]][x, 3] <- input[[paste0("ord", rv$si, x)]])
          isolate(rv$seq[[rv$si]][x, 4] <- input[[paste0("cla", rv$si, x)]])
        })
      }
    }
  })

  observeEvent(input$add_run, {
    seq <- rv$seq[[rv$si]]
    dat <- rv$data[[rv$si]]
    adduct <- findadduct(dat, seq)
    dat <- cbind(dat, adduct)
    ionmode <- switch(input$add_mode,
      "Positive" = "Adduct_pos",
      "Negative" = "Adduct_neg"
    )
    adduct <- c(ionmode, NA, NA, NA)
    seq <- rbind(seq, "adduct" = adduct)
    seq[, 2] <- as.numeric(seq[, 2])
    seq[, 3] <- as.numeric(seq[, 3])
    seq[, 4] <- as.numeric(seq[, 4])
    rv$seq[[rv$si]] <- seq
    rv$data[[rv$si]] <- dat
  })

  observeEvent(input$bf, {
    if (is.null(rv$si)) {
      showNotification("No data", type = "error")
    } else if (!"QC" %in% rv$seq[[rv$si]][, 1]) {
      showNotification("Data must have atleast 1 QC", type = "error")
    } else if (!"Blank" %in% rv$seq[[rv$si]][, 1]) {
      showNotification("Data must have atleast 1 Blank", type = "error")
    } else if (sum(rv$seq[[rv$si]][, 1] %in% "Name") != 1) {
      showNotification("Data must have exactly 1 \"Name\" column", type = "error")
    } else {
      bfseq <- rv$seq[[rv$si]]

      bfdat <- blankfiltration(
        dat = rv$data[[rv$si]],
        seq = bfseq,
        xbf = input$xbf,
        keepis = input$bfkeepis
      )
      if (input$bfdiscard) {
        bfdat <- bfdat[!bfseq[, 1] %in% "Blank"]
        bfseq <- bfseq[!bfseq[, 1] %in% "Blank", ]
      }
      rv$tmpdata <- bfdat
      rv$tmpseq <- bfseq
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpdata, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(
        session = session,
        title = "Success",
        text = paste0(nrow(rv$data[[rv$si]]) - nrow(rv$tmpdata), " features was removed"),
        type = "success"
      )
    }
  })

  observeEvent(input$bfsave, {
    if (is.null(rv$tmpdata)) {
      showNotification("Blank filtrate first", type = "error")
    } else {
      if (input$bfnewsave) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpdata
        rv$seq[[length(rv$seq) + 1]] <- rv$tmpseq
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$si], "_", input$xbf, "xb")
      } else {
        rv$data[[rv$si]] <- rv$tmpdata
        rv$seq[[rv$si]] <- rv$tmpseq
        names(rv$data)[rv$si] <- paste0(names(rv$data)[rv$si], "_", input$xbf, "xb")
      }
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      updateSelectInput(session, "selectdata1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca2", choices = rv$choices, selected = input$selectpca2)
      rv$tmpdata <- NULL
      rv$tmpseq <- NULL
    }
  })

  observeEvent(input$is, {
    if (is.null(rv$si)) {
      showNotification("No data", type = "error")
    } else if (sum(rv$seq[[rv$si]][, 1] %in% "Name") != 1) {
      showNotification("Data must have exactly 1 \"Name\" column", type = "error")
    } else if (is.null(input$isChoose)) {
      showNotification("No intern standars selected", type = "error")
    } else {
      isseq <- rv$seq[[rv$si]]
      isdat <- isfunc(
        dat = rv$data[[rv$si]],
        seq = isseq,
        is = input$isChoose,
        method = input$ismethod,
        qc = input$isqc
      )
      rv$tmpdata <- isdat
      rv$tmpseq <- isseq
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpdata, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
    }
  })

  observeEvent(input$is_optimize, {
    seq <- rv$seq[[rv$si]]
    dat <- rv$data[[rv$si]]
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
    if (is.null(rv$tmpdata)) {
      showNotification("IS normalize first", type = "error")
    } else {
      if (input$isnewsave) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpdata
        rv$seq[[length(rv$seq) + 1]] <- rv$tmpseq
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$si], "_is")
      } else {
        rv$data[[rv$si]] <- rv$tmpdata
        rv$seq[[rv$si]] <- rv$tmpseq
        names(rv$data)[rv$si] <- paste0(names(rv$data)[rv$si], "_is")
      }
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      updateSelectInput(session, "selectdata1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca2", choices = rv$choices, selected = input$selectpca2)
      rv$tmpdata <- NULL
      rv$tmpseq <- NULL
    }
  })

  observeEvent(input$mvf_run, {
    if (is.null(rv$si)) {
      sendSweetAlert(session, "No data", type = "error")
    } else {
      mvf_seq <- rv$seq[[rv$si]]
      mvf_dat <- cutoffrm(
        dat = rv$data[[rv$si]],
        seq = mvf_seq,
        cutoff = input$mvf_cutoff,
        method = input$mvf_conditions
      )
      rv$tmpdata <- mvf_dat
      rv$tmpseq <- mvf_seq
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpdata, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(
        title = "Success",
        text = paste0(nrow(rv$data[[rv$si]]) - nrow(rv$tmpdata), " features was removed"),
        type = "success"
      )
    }
  })

  observeEvent(input$mvf_save, {
    if (is.null(rv$tmpdata)) {
      showNotification("Filtrate first", type = "error")
    } else {
      if (input$mvf_newsave) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpdata
        rv$seq[[length(rv$seq) + 1]] <- rv$tmpseq
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$si], "_mvr")
      } else {
        rv$data[[rv$si]] <- rv$tmpdata
        rv$seq[[rv$si]] <- rv$tmpseq
        names(rv$data)[rv$si] <- paste0(names(rv$data)[rv$si], "_mvr")
      }
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      updateSelectInput(session, "selectdata1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca2", choices = rv$choices, selected = input$selectpca2)
      rv$tmpdata <- NULL
      rv$tmpseq <- NULL
    }
  })

  observeEvent(input$imp_run, {
    if (is.null(rv$si)) {
      showNotification("No data", type = "error")
    } else if (sum(rv$seq[[rv$si]][, 1] %in% "Sample") < 1) {
      showNotification("Data must have atleast one Sample", type = "error")
    } else {
      imp_seq <- rv$seq[[rv$si]]
      imp_dat <- imputation(
        dat = rv$data[[rv$si]],
        seq = imp_seq,
        method = input$imp_method,
        minx = input$imp_minx,
        onlyqc = input$imp_onlyqc,
        remaining = input$imp_remaining
      )
      rv$tmpdata <- imp_dat
      rv$tmpseq <- imp_seq
      updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
      output$dttable <- renderDataTable(rv$tmpdata, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
      sendSweetAlert(
        title = "Success",
        text = paste0(sum(is.na(rv$data[[rv$si]]) | rv$data[[rv$si]] == 0) - sum(is.na(rv$tmpdata) | rv$tmpdata == 0), " missing values was imputed"),
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
    if (is.null(rv$tmpdata)) {
      showNotification("Imputate first", type = "error")
    } else {
      if (input$imp_newsave) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpdata
        rv$seq[[length(rv$seq) + 1]] <- rv$tmpseq
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$si], "_imp")
      } else {
        rv$data[[rv$si]] <- rv$tmpdata
        rv$seq[[rv$si]] <- rv$tmpseq
        names(rv$data)[rv$si] <- paste0(names(rv$data)[rv$si], "_imp")
      }
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      updateSelectInput(session, "selectdata1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca2", choices = rv$choices, selected = input$selectpca2)
      rv$tmpdata <- NULL
      rv$tmpseq <- NULL
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
    dc_seq <- rv$seq[[rv$si]]
    dc_dat <- driftcorrection(
      dat = rv$data[[rv$si]],
      seq = dc_seq,
      method = input$dc_method,
      ntree = input$dc_ntree,
      QCspan = input$dc_qcspan
    )
    rv$tmpdata <- dc_dat
    rv$tmpseq <- dc_seq
    updateSelectInput(session, "selectpca1", selected = "Unsaved data", choices = c("Unsaved data", rv$choices))
    output$dttable <- renderDataTable(rv$tmpdata, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "700px", pageLength = 20))
  })

  observeEvent(input$dc_save, {
    if (is.null(rv$tmpdata)) {
      showNotification("Drift correct first", type = "error")
    } else {
      if (input$dc_newsave) {
        rv$data[[length(rv$data) + 1]] <- rv$tmpdata
        rv$seq[[length(rv$seq) + 1]] <- rv$tmpseq
        names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$si], "_dc")
      } else {
        rv$data[[rv$si]] <- rv$tmpdata
        rv$seq[[rv$si]] <- rv$tmpseq
        names(rv$data)[rv$si] <- paste0(names(rv$data)[rv$si], "_dc")
      }
      rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
      updateSelectInput(session, "selectdata1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
      updateSelectInput(session, "selectpca2", choices = rv$choices, selected = input$selectpca2)
      rv$tmpdata <- NULL
      rv$tmpseq <- NULL
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
    seq1 <- rv$seq[[rv$si]]
    dat1 <- rv$data[[rv$si]]
    sel <- which(rv$choices %in% input$md_select)
    seq2 <- rv$seq[[sel]]
    dat2 <- rv$data[[sel]]

    if (sum(seq1[, 1] %in% c("Adduct_pos", "Adduct_neg")) != 1 || sum(seq2[, 1] %in% c("Adduct_pos", "Adduct_neg")) != 1) {
      sendSweetAlert(session = session, title = "Error", text = "Each dataset must contain exactly one adduct column labeled in the sequence file.", type = "error")
    } else if (ncol(dat1) != ncol(dat2)) {
      sendSweetAlert(session = session, title = "Error", text = "Datasets must have the same number of colunms", type = "error")
    } else {
      merged_dat <<- merge.func(
        dat1 = dat1,
        seq1 = seq1,
        dat2 = dat2,
        seq2 = seq2,
        ppmtol = input$md_ppm,
        rttol = input$md_rt
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
      output$md_modal_dt <- renderDataTable(
        {
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
          p(paste0(length(unique(dub_dat$mergeID))), " duplcicate clusters found, of those ", paste0(length(unique(out_dub[out_dub[, 1] > 2, ][, 2]))), " consists of more than 2 features."),
          DTOutput("md_modal_dt"),
          footer = list(actionButton("md_modal_go", "Remove duplicates"), modalButton("Dismiss"))
        )
      )
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
      rv$seq[[length(rv$seq) + 1]] <- rv$seq[[rv$si]]
      names(rv$data)[length(rv$data)] <- paste0(names(rv$data)[rv$si], "_merged")
    } else if (isFALSE(input$md_newsave)) {
      rv$data[[rv$si]] <- dat[, seq(ncol(dat) - 2)]
      names(rv$data)[rv$si] <- paste0(names(rv$data)[rv$si], "_merged")
    }
    rv$choices <- paste(1:length(rv$data), ": ", names(rv$data))
    updateSelectInput(session, "selectdata1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca1", choices = rv$choices, selected = rv$choices[length(rv$choices)])
    updateSelectInput(session, "selectpca2", choices = rv$choices, selected = input$selectpca2)
  })

  observeEvent(list(input$selectpca1, input$bf, input$updateseq, input$is, input$in_seq, input$imp_run), ignoreInit = T, {
    if (!is.null(rv$si)) {
      if (input$selectpca1 == "Unsaved data") {
        data <- rv$tmpdata
        seq <- rv$tmpseq
      } else {
        selectchoices <- paste(1:length(rv$data), ": ", names(rv$data))
        sd <- which(rv$choices %in% input$selectpca1)
        data <- rv$data[[sd]]
        seq <- rv$seq[[sd]]
      }
      if ("Sample" %in% seq[, 1]) {
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
    if ("Sample" %in% rv$seq[[sd]][, 1]) {
      data <- rv$data[[sd]]
      seq <- rv$seq[[sd]]
      seq[seq[, 1] %in% "QC", ][, 4] <- "QC"
      sdata <- data[seq[, 1] %in% c("Sample", "QC")]
      sclass <- seq[seq[, 1] %in% c("Sample", "QC"), ][, 4]
      pca <- pcaplot(sdata, sclass)
      output$plotpca2 <- renderPlotly(pca)

      if (sum(seq$lab %in% "QC") > 0) {
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
      if (is.null(rv$si)) {
        p("No data")
      } else if (input$drift_select != "None" && nrow(rv$data[[rv$si]]) != nrow(rv$data[[which(rv$choices %in% input$drift_select)]])) {
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
      if (is.null(rv$si)) {
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
              data = rv$data[[rv$si]][input$dt_drift_panel_rows_selected[my_i], ],
              seq = rv$seq[[rv$si]]
            )
          })
        })
        if (input$drift_select != "None") {
          local({
            my_i <- i
            output[[paste0("driftplotoutput2", my_i)]] <- renderPlot({
              driftplot(
                data = rv$data[[which(rv$choices %in% input$drift_select)]][input$dt_drift_panel_rows_selected[my_i], ],
                seq = rv$seq[[rv$si]]
              )
            })
          })
        }
      }
    } else if (rv$drift_plot_select == 2) {
      output$cvscatterplot <- renderPlot({
        cvscatterplot(
          data = rv$data[[rv$si]],
          data2 = rv$data[[which(rv$choices %in% input$drift_select)]],
          seq = rv$seq[[rv$si]],
          name1 = names(rv$data)[rv$si],
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
              data = rv$data[[rv$si]][input$dt_boxplot_panel_rows_selected[my_i], ],
              seq = rv$seq[[rv$si]],
              log = input$bloxplot_log,
              ylog = input$bloxplot_ylog
            )
          })
        })
      }
    }
  })

  observe({
    if (!is.null(rv$si)) {
      seq <- rv$seq[[rv$si]]
      dat <- rv$data[[rv$si]]
      blank_mv <- sum(is.na(dat[seq[, 1] %in% "Blank"])) +
                    sum(dat[seq[, 1] %in% "Blank"] == 0, na.rm = TRUE)
      qc_mv <- sum(is.na(dat[seq[, 1] %in% "QC"])) +
                    sum(dat[seq[, 1] %in% "QC"] == 0, na.rm = TRUE)
      sample_mv <- sum(is.na(dat[seq[, 1] %in% "Sample"])) +
                    sum(dat[seq[, 1] %in% "Sample"] == 0, na.rm = TRUE)

      sdata <- dat[seq[, 1] %in% "Sample"]
      sclass <- seq[seq[, 1] %in% "Sample", ][, 4]
      if (sum(seq$lab %in% "QC") > 0) {
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
        HTML("<h2>", names(rv$data)[rv$si], "</h2>", nrow(dat) - 1, " features.<br>", ncol(dat[seq[, 1] %in% "Sample"]), " samples.<br>", ncol(dat[seq[, 1] %in% "QC"]), " QC samples.<br>", ncol(dat[seq[, 1] %in% "Blank"]), " Blank samples.<br>", "<br>", sample_mv, " missing values in Samples<br>", qc_mv, " missing values in QC samples<br>", blank_mv, " missing values in Blank samples<br><br>")
      })
      output$cvinfo_ui <- renderUI({
        HTML(text)
      })
    }
  })
}
# Run the application
shinyApp(ui = ui, server = server)