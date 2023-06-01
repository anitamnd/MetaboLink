library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
# library(spsComps)
library(DT)
library(dplyr)
library(plotly)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(impute)
library(randomForest)
library(writexl)
library(igraph)
library(stringi)
library(BiocManager)
options(repos = BiocManager::repositories())
source("functions.R")
source("plotfunctions.R")

dashboardPage(
  dashboardHeader(
    title = "JLspec",
    titleWidth = 400
  ),

  #### Sidebar ####
  
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
            column(12, fileInput("in_file", "Select file", accept = c("txt/csv", "text/comma-seperated-values, text/plain", ".csv"),
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
            column(12, checkboxInput("bfdiscard", "Discard blank", value = T, width = "100%"), style = "padding: 0px; margin-top: -25px; margin-bottom: -15px; margin-left: 5px;"),
            column(12, checkboxInput("bfkeepis", "Keep IS", value = T, width = "100%"), style = "padding: 0px; margin-top: -15px; margin-bottom: -15px; margin-left: 5px;"),
            column(12, checkboxInput("bfnewsave", "Save as new file", value = T, width = "100%"), style = "padding: 0px; margin-top: -15px; margin-bottom: -15px; margin-left: 5px;")
          ),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, bsButton("bf", "Blank filtrate", width = "100%"), style = "padding-left:0px; margin-top: 10px;"),
            column(6, bsButton("bfsave", "Save", width = "100%"), style = "padding-left:0px; margin-top: 10px;"),
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
            checkboxInput("mvf_newsave", "Save as new file", value = T, width = "100%")
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
            column(6, checkboxInput("isqc", "Normalize QC", value = T, width = "100%"), style = "padding: 0px; margin-top: -30px; margin-left: 10px; margin-right: -10px;"),
            column(6, checkboxInput("isnewsave", "Save as new file", value = T, width = "100%"), style = "padding: 0px; margin-top: -30px; margin-left: 10px; margin-right: -10px;")
          ),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, bsButton("is", "IS normalize", width = "100%"), style = "padding-left:0px;"),
            column(6, bsButton("is_optimize", "Optimize", width = "100%"), style = "padding-left:0px;")
          ),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, bsButton("isremove", "Remove IS", width = "100%"), style = "padding-left:0px;"),
            column(6, bsButton("issave", "Save", width = "100%"), style = "padding-left:0px;")          
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
            column(12, checkboxInput("dc_newsave", "Save as new file", value = T, width = "100%"), style = "padding: 0px; margin-top: -10px; margin-left: 10px; margin-right: -10px;"),
            column(6, bsButton("dc_run", "Run", width = "100%"), style = "padding-left:0px;"),
            column(6, bsButton("dc_save", "Save", width = "100%"), style = "padding-left:0px;")
          )
        ),
        bsCollapsePanel("Merge datasets",
          style = "primary",
          fluidRow(selectInput("md_select", "Select dataset to merge with", choices = NULL, width = "100%")),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, numericInput("md_ppm", "M/z tolerance ppm", min = 0, value = 10, width = "100%"), style = "padding-left:0px;"),
            column(6, numericInput("md_rt", "RT tolerance", min = 0, value = 0.1, step = 0.01, width = "100%"), style = "padding-left:0px;")
          ),
          fluidRow(
            style = "margin-right: 0px;",
            column(6, bsButton("md_rankings", "Edit priorities", width = "100%"), style = "padding-left:0px;"),
            column(6, bsButton("md_run", "Run", width = "100%"), style = "padding-left:0px;")
          )
        )
      )
    ),
    sidebarMenu()
  ),

  #### Main Body ####
  
  dashboardBody(
    tags$head(tags$style(".modal-sm{ width:300px}
                         .modal-lg{ width:1200px}")),
    tags$head(tags$script(src="CallShiny.js")),
    useShinyjs(),  # Include shinyjs
    extendShinyjs(script="CallShiny.js", functions=c("retrieve_results","send_message","run_button")),
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
      column(2, dropdownButton(
        inputId = "plot2", circle = FALSE, width = "100%",
        label = "Explore data",
        bsButton("pca_button", label = "PCA", icon = icon("thumbs-up"), block = TRUE),
        bsButton("drift_button", label = "Feature drift", block = TRUE),
        bsButton("feature_button", label = "Feature Viewer", block = TRUE),
        bsButton("info_button", label = "Dataset-Info", block = TRUE)
      )),
      column(2, bsButton("statistics_button",
        label = "Statistics",
        icon = icon("thumbs-up"),
        style = "default",
        block = T
      )),
      tags$style(type = "text/css", "#plot2 {width:100%}"),
      column(2, bsButton("export",
        label = "Export",
        icon = icon("thumbs-up"),
        style = "default",
        block = TRUE
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
                  h4("Group")
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
              width = NULL, title = "Group nicknames",
              actionButton("group_edit", "Edit", width = 100)
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
            box(width = NULL, DTOutput("dttable"))
    )))),
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
    )))),
    fluidRow(
      hidden(
        div(
          id = "statistics_panel",
          fluidPage(
            fluidRow(
              column(6, id="pr_c1",
                  h4("Data manipulation and adjustments"),
                  column(10,checkboxInput("norm_qc", "Normalization by a pooled sample (QC)", value=F)),
                  column(10,checkboxInput("logtrans", "Log-transform data", value=F)),
                  column(10,checkboxInput("mean_scale", "Mean scale", value=F)),                  
                  actionButton("adjust_button", "Transform data") #TODO save button      
              ),
              column(5, id="pr_c3",
                  h4("Summary"),
                  actionButton("send_polystest", "Send to PolySTest"),
                  span(textOutput("connection_polystest"), style="color:#33DD33;"),     
                  textInput("url_polystest",label="URL",value="http://computproteomics.bmb.sdu.dk:443/app_direct/PolySTest/"),
                  disabled(actionButton("retrieve_polystest", "Retrieve results from PolySTest"))              )
            ),
            br(),
            fluidRow(column(12, box(width = NULL, DTOutput("stats_table"))))
    )))),
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
    ))),
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
    ))),
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
    ))),
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
