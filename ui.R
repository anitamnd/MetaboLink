shinyUI(dashboardPage(
  dashboardHeader(
    title = "MetaboLink",
    titleWidth = 400,
    dropdownMenu(type = "notifications",
                 icon = icon("question-circle"),
                 badgeStatus = NULL,
                 headerText = "Help",
                 notificationItem("User manual", icon = icon("book"),
                                  href = "https://github.com/anitamnd/MetaboLink/wiki"),
                 notificationItem("Source code and installation", icon = icon("file"),
                                  href = "https://github.com/anitamnd/MetaboLink"),
                 notificationItem("Institution", icon = icon("university"),
                                  href = "https://www.sdu.dk/en")
    )
  ),
  
  # Sidebar 
  
  dashboardSidebar(
    width = "400",
    useShinyjs(),
    tags$style(HTML(".panel-primary {color: #000000;}")),
    tags$style(HTML("#removeIS {
        color: black;
        text-decoration: underline;
      }")),
    tags$style(".skin-blue .sidebar .norm a { color: #444; }"),
    tags$head(tags$script(src="CallShiny.js")),
    extendShinyjs(script="CallShiny.js", functions=c("retrieve_results","send_message","run_button")),
    fluidPage(
      fluidRow(
        selectizeInput("selectDataset", "Active dataset",
                       choices = NULL, width = "100%",
                       options = list(placeholder = "Please upload a file to start")
        ) %>% 
          bsTooltip("Switch between the different uploaded/saved datsets.",
                    placement = "right", trigger = "hover")
      ),
      bsCollapse(
        id = "menu", multiple = FALSE, open = "Data input",
        bsCollapsePanel("Data input",
                        style = "primary",
                        fluidRow(style = "padding: 0px;",
                                 selectInput("fileType", "Select file format",
                                             choices = c("Samples in columns", "Samples in rows"),
                                             selected = "Samples in columns",
                                             width = "100%"
                                 )
                        ),
                        fluidRow(style = "padding: 0px;",
                                 fileInput("inputFile", "Upload file (.txt or .csv)",
                                           accept = c("txt/csv", "text/comma-seperated-values, text/plain", ".csv"),
                                           width = "100%"
                                 )
                        ),
                        # TODO: might be releveant for future
                        # fluidRow(style = "padding: 0px;",
                        #          checkboxGroupInput("dataType", "",
                        #                             choices = c("Metabolomics" = "metabolomics",
                        #                                         "Lipidomics" = "lipidomics"),
                        #                             selected = NULL, 
                        #                             inline = TRUE)
                        # ),
                        fluidRow(
                          style = "margin-right: 0px;",
                          column(6, style = "padding-left:0px;",
                                 bsButton("upload", "Upload", width = "100%")
                          ),
                          column(6, style = "padding-left:0px;",
                                 bsButton("example", "Load example", width = "100%")  %>% 
                                   bsTooltip("Load example metabolomics datasets in positive and negative ion mode and respective metadata files. More examples available on GitHub.", placement = "right", trigger = "hover")
                          )
                        )
        ),
        bsCollapsePanel("Blank filtration",
                        style = "primary",
                        fluidRow(
                          style = "padding: 0px;",
                          column(12,
                                 sliderInput("signalStrength", "Signal strength above blank", 1, 10, 5, step = 0.1, width = "100%"),
                                 style = "padding: 0px"
                          )
                        ),
                        fluidRow(
                          style = "margin-right: 0px;",
                          column(12,
                                 checkboxInput("discardBlank", "Discard blank", value = T, width = "100%"),
                                 style = "padding: 0px; margin-top: -25px; margin-bottom: -15px; margin-left: 5px;"
                          ),
                          column(12,
                                 checkboxInput("keepIS", "Keep internal standards", value = T, width = "100%"),
                                 style = "padding: 0px; margin-top: -15px; margin-bottom: -15px; margin-left: 5px;"
                          ),
                          column(12,
                                 checkboxInput("newFileBF", "Save as new file", value = T, width = "100%"),
                                 style = "padding: 0px; margin-top: -15px; margin-bottom: -15px; margin-left: 5px;"
                          )
                        ),
                        fluidRow(
                          style = "margin-right: 0px;",
                          column(6, bsButton("blankFiltrate", "Blank filtrate", width = "100%"),
                                 style = "padding-left:0px; margin-top: 10px;"
                          ),
                          column(6, bsButton("saveBF", "Save", width = "100%"),
                                 style = "padding-left:0px; margin-top: 10px;"
                          )
                        )
        )  %>% 
          bsTooltip("Requires columns labeled 'Blank' and 'QC'.", placement = "right", trigger = "hover"),
        bsCollapsePanel("Missing value filtration",
                        style = "primary",
                        fluidRow(
                          style = "padding: 0px;",
                          column(12,
                                 sliderInput("cutoffNAs", "Minimum percentage of messured values", 0, 100, 80, step = 5, width = "100%"),
                                 style = "padding: 0px"
                          )
                        ),
                        fluidRow(
                          column(6,
                                 prettyCheckboxGroup("filterNAmethod", "", choices = c("in QC", "in group", "entire data"))
                          )),
                        fluidRow(
                          column(6,
                                 checkboxInput("mvf_newsave", "Save as new file", value = T, width = "100%")
                          )),
                        fluidRow(
                          style = "margin-right: 0px;",
                          column(6, bsButton("runFilterNA", "Run", width = "100%"), style = "padding-left:0px;"),
                          column(6, bsButton("saveFilterNA", "Save", width = "100%"), style = "padding-left:0px;")
                        )
        ),
        bsCollapsePanel("Imputation",
                        style = "primary",
                        fluidRow(selectInput("imputationMethod", "Imputation method", choices = c("KNN", "Min/X", "Median"), width = "100%")),
                        fluidRow(hidden(div(id = "imp_remaining_hide", selectInput("remainingNAs", "Remaining missing values", choices = c("zero", "Min/X", "Median"), width = "100%")))),
                        fluidRow(hidden(div(id = "imp_minx_hide", sliderInput("imputationMinX", "Divide min by", min = 1, max = 10, value = 1, step = 1, width = "100%")))),
                        fluidRow(
                          style = "margin-right: 0px;",
                          column(6, prettyCheckbox("imp_onlyQC", "Only impute QC", value = TRUE)),
                          column(6)
                        ),
                        fluidRow(
                          style = "margin-right: 0px;",
                          column(6,
                                 checkboxInput("newFileImp", "Save as new file", value = T, width = "100%"), 
                                 style = "padding: 0px; margin-top: -10px; margin-left: 10px; margin-right: -10px;")
                        ),
                        fluidRow(
                          style = "margin-right: 0px;",
                          column(6, bsButton("runImputation", "Run", width = "100%"), style = "padding-left:0px;"),
                          column(6, bsButton("saveImputation", "Save", width = "100%"), style = "padding-left:0px;")
                        )
        ),
        bsCollapsePanel("Normalization",
                        style = "primary",
                        p("Expand options below to see all the normalization methods."),
                        div(class = "norm",
                            bsCollapse(
                              id =  "norm2", multiple = FALSE, open = "Normalization",
                              bsCollapsePanel("Internal standards",
                                              fluidRow(
                                                selectInput("isMethod", "Method", choices = c("Nearest RT", "Same lipid structure"), selected = "Nearest RT", width = "100%")
                                              ),
                                              fluidRow(
                                                checkboxGroupInput("isChoose", NULL, choices = NULL, selected = NULL, inline = FALSE)
                                              ),
                                              fluidRow(
                                                style = "margin-right: 0px;",
                                                column(12, actionLink("removeIS", "Remove IS", width = "50%") %>% 
                                                         bsTooltip("Remove internal standards.", placement = "right", trigger = "hover")
                                                )),
                                              fluidRow(
                                                style = "margin-right: 0px;",
                                                column(6, checkboxInput("normalizeQC", "Normalize QC", value = T, width = "100%"), style = "padding: 0px; margin-top: 0px; margin-left: 10px; margin-right: -10px;"),
                                                column(6, checkboxInput("newFileIS", "Save as new file", value = T, width = "100%"), style = "padding: 0px; margin-top: 0px; margin-left: 10px; margin-right: -10px;")
                                              ),
                                              fluidRow(
                                                style = "margin-right: 0px;",
                                                column(6, bsButton("normalizeIS", "Normalize", width = "100%"), style = "padding-left:0px;"),
                                                column(6, bsButton("saveIS", "Save", width = "100%"), style = "padding-left:0px;")
                                              )
                              ),
                              bsCollapsePanel("Drift correction",
                                              fluidRow(
                                                selectInput("driftMethod", "Signal correction method", choices = c("QC-RFSC (random forest)", "QC-RLSC (robust LOESS)"), width = "100%")
                                              ),
                                              fluidRow(
                                                conditionalPanel(
                                                  condition = "input.driftMethod == 'QC-RFSC (random forest)'",
                                                  div(id = "dc_ntree_hide", 
                                                      sliderInput("driftTrees", "ntree", min = 100, max = 1000, value = 500, step = 100, width = "100%")
                                                  )
                                                ),
                                                conditionalPanel(
                                                  condition = "input.driftMethod == 'QC-RLSC (robust LOESS)'",
                                                  div(id = "dc_qcspan_hide", 
                                                      sliderInput("driftQCspan", "QCspan", min = 0.2, max = 0.75, value = 0.5, step = 0.05, width = "100%")
                                                  ),
                                                  div(id = "dc_degree_hide", 
                                                      sliderInput("driftDegree", "degree", min = 0, max = 2, value = 2, step = 1, width = "100%")
                                                  )
                                                )
                                              ),
                                              fluidRow(style = "margin-right: 0px;",
                                                       column(12, checkboxInput("newFileDrift", "Save as new file", value = T, width = "100%"), style = "padding: 0px; margin-top: -10px; margin-left: 10px; margin-right: -10px;"),
                                                       column(6, bsButton("runDrift", "Run", width = "100%"), style = "padding-left:0px;"),
                                                       column(6, bsButton("saveDrift", "Save", width = "100%"), style = "padding-left:0px;")
                                              )
                              ),
                              bsCollapsePanel("More",
                                              fluidRow(
                                                style = "margin-right: 0px;",
                                                column(12, selectInput("normMethod", "Select normalization method", choices = c("QC (PQN)", "Sum", "Median", "Sample amount"), width = "100%"), style = "padding-left:0px;")
                                              ),
                                              fluidRow(
                                                style = "margin-right: 0px;",
                                                column(12, checkboxInput("newFileNorm", "Save as new file", value = T, width = "100%"), style = "padding: 0px; margin-top: -10px; margin-left: 10px; margin-right: -10px;"),
                                                column(6, bsButton("normalize", "Run", width = "100%"), style = "padding-left:0px;"),
                                                column(6, bsButton("saveNormalization", "Save", width = "100%"), style = "padding-left:0px;")
                                              )
                              ) %>% 
                                bsTooltip("Press for more normalization options.", placement = "right", trigger = "hover")
                            ))
        ),
        bsCollapsePanel("Log transform and scaling",
                        style = "primary",
                        fluidRow(
                          style = "margin-right: 0px;",
                          column(6, style = "padding-left:0px;",
                                 selectInput("logTransform", "Log transform", choices = c("None", "log2", "log10", "ln"), width = "100%") %>%
                                   bsTooltip("Do not use log transformation for negative values.", placement = "right", trigger = "hover")
                          ),
                          column(6, selectInput("scaling", "Data scaling", choices = c("None", "Mean center", "Auto scale"), width = "100%"), style = "padding-left:0px;")
                        ),
                        fluidRow(
                          column(6,
                                 checkboxInput("newFileTransform", "Save as new file", value = T, width = "100%")
                          )),
                        fluidRow(
                          style = "margin-right: 0px;",
                          column(6, bsButton("transform", "Run", width = "100%"), style = "padding-left:0px;"),
                          column(6, bsButton("saveTransform", "Save", width = "100%"), style = "padding-left:0px;")
                        )
        ) %>% bsTooltip("Some features will be removed after transforming the data if Inf values are introduced.", placement = "right", trigger = "hover"),
        bsCollapsePanel("Merge datasets",
                        style = "primary",
                        fluidRow(selectInput("mergeFile", "Select dataset to merge with", choices = NULL, width = "100%")),
                        fluidRow(
                          style = "margin-right: 0px;",
                          column(6, numericInput("merge_ppm", "M/z tolerance ppm", min = 0, value = 10, width = "100%"), style = "padding-left:0px;"),
                          column(6, numericInput("merge_rt", "RT tolerance", min = 0, value = 0.1, step = 0.01, width = "100%"), style = "padding-left:0px;")
                        ),
                        
                        selectInput("annotation_column_merge", "Select merging Column", choices = NULL, width = "100%")
                        ,
                        fluidRow(
                          style = "margin-right: 0px;",
                          column(6, bsButton("editRankings", "Edit priorities", width = "100%"), style = "padding-left:0px;"),
                          column(6, bsButton("mergeDatasets", "Run", width = "100%"), style = "padding-left:0px;")
                        )
        ) %>% bsTooltip("Merge datasets with same samples and different ion mode. The datasets must have the same number of samples.", placement = "right", trigger = "hover"),
        bsCollapsePanel("Remove files",
                        style = "primary",
                        fluidRow(
                          style = "margin-right: 0px;",
                          column(12, checkboxGroupInput("filesToRemove", "Select files to remove", choices = NULL, selected = NULL), style = "padding-left:0px;"),
                          column(12, bsButton("removeFiles", "Remove", width = "50%"), style = "padding-left:0px;")
                        )
        )
      ),
      fluidRow(
        column(12, div(style = "float: right;",
                       a(icon("book"), "User manual",  href = "https://github.com/anitamnd/MetaboLink/wiki")
        ))
      )
    ),
    sidebarMenu()
  ),
  
  # Main Body
  
  dashboardBody(
    tags$head(tags$style(".modal-sm{ width:300px}
                         .modal-lg{ width:1200px}")),
    useShinyjs(),
    fluidRow(hidden(div(
      id = "buttons", style = "padding-bottom: 49px",
      column(3, bsButton("sequence",
                         label = "Sequence",
                         icon = icon("tags"),
                         style = "default",
                         block = T
      )),
      column(3, bsButton("explore",
                         label = "Explore data",
                         icon = icon("table"),
                         style = "default",
                         block = T
      )),
      column(3, bsButton("statistics_button",
                         label = "Statistics",
                         icon = icon("clipboard"),
                         style = "default",
                         block = T
      )),
      tags$style(type = "text/css", "#plot2 {width:100%}"),
      column(3, bsButton("export",
                         label = "Export",
                         icon = icon("download"),
                         style = "default",
                         block = TRUE
      )),
    ))),
    fluidRow(
      hidden(
        div(
          id = "sequence_panel",
          column(12, box(width = NULL, title = "Instructions", status = "primary", solidHeader = TRUE,
                         tagList(
                           list(
                             tags$li("Upload the sequence/metadata file."),
                             tags$li("The sample names in the metafile should match the sample names in the data file."),
                             tags$li("Make sure the columns are labeled correctly before proceeding."),
                             tags$li("If a sample (numeric) column is labeled '-', this usually means there are invalid characters in the column."),
                             tags$li("Labels cannot be edited in the app to avoid crashes. Please edit the file and re-upload.")
                           )
                         )
          )),
          column(
            width = 8,
            box(
              title = textOutput("diboxtitle"), width = NULL,
              DTOutput("seq_table") %>% withSpinner(color="steelblue")
            )
          ),
          column(
            width = 4,
            box(
              width = NULL, title = "Upload Sequence File", status = "danger",
              fileInput("inputSequence", "Select file", accept = c("txt/csv", "text/comma-seperated-values,text/plain", ".csv"), width = "100%"),
              column(6, style = "padding-left: 0px;", actionButton("updateSequence", label = "Update", width = "100%")), 
              column(6, style = "padding-right: 0px;", actionButton("reuseSequence", label = "Re-use sequence", width = "100%"))
            ),
            
            box(
              column(
                width = 6,
                selectInput("identifier_column_refmet", "Select Inchi Column", choices = NULL, width = "100%")
              ),
              checkboxInput("online_refmet", "Online lookup. ", value = FALSE, width = "100%"),
              width = NULL, title = "Add RefMet Information",
              actionButton("addRefmet", "Add", width = "50%")
            ),
            
            box(
              column(
                width = 6,
                selectInput("name_column_lipids", "Select Lipid Name Column", choices = NULL, width = "100%")
              ),
              width = NULL, title = "Clean Lipid Names",
              actionButton("cleanedLipidGroup", "Clean", width = "50%")
            ),
            
            box(
              column(
                width = 6,
                selectInput("name_column_annotate", "Select Annotation Column", choices = NULL, width = "100%")
              ),
              width = NULL, title = "Remove Unannotated Features",
              actionButton("RemoveUnannotated", "Remove", width = "50%")
            ),
            
            box(
              width = NULL, title = "Edit Data Columns",
              actionButton("editColumns", "Edit", width = "50%")
            ),
            box(
              width = NULL, title = "Group Nicknames",
              p("Only use letters and numbers."),
              actionButton("editGroups", "Edit", width = "50%")
            ),
            box(
              width = NULL, title = "Download Sequence File",
              downloadButton("downloadSequence", " Download")
            )
          )
        )
      )
    ),
    fluidRow(
      hidden(
        div(
          id = "datatable_panel",
          tabsetPanel(
            tabPanel("Data table",
                     fluidRow(
                       column(12, box(width = NULL, DTOutput("dttable") %>% withSpinner(color="steelblue")))
                     )
            ),
            tabPanel("Sample distribution",
                     #TODO add specific panel for comparison?
                     fluidRow(
                       column(12,
                              box(width = 12,
                                  title = "Median Across Samples",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  plotlyOutput("histogram", height = "600px") %>% withSpinner(color="steelblue")
                              )),
                       column(12,
                              box(width = 12, title = "Median Across QCs",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  uiOutput("histogram_qc", height = "600px") %>% withSpinner(color="steelblue")
                              )),
                       column(
                         width = 12,
                         box(
                           title = "Class Plot",
                           width = 12,
                           status = "primary",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           fluidRow(
                             column(
                               width = 3,
                               wellPanel(
                                 h4("Select which column plot to display:"),
                                 selectInput(
                                   inputId = "selected_column_class_plot",
                                   label = NULL,
                                   choices = c("Super class" = "super_class",
                                               "Main class" = "main_class",
                                               "Sub class" = "sub_class",
                                               "Lipid Class" = "Lipid.Abbreviation"),
                                   selected = "super_class"
                                 )
                               )
                             ),
                             column(
                               width = 9,
                               plotlyOutput("class_plot", height = "600px") %>% withSpinner(color = "steelblue")
                             )
                           )
                         )
                       ),
                       
                       column(12, 
                              box(
                                width = 12,
                                title = "Violin Plot",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotlyOutput("violin_plot", height = "600px") %>% withSpinner(color="steelblue")
                              )
                       )
                       ,
                       column(12,
                              box(width = 12,
                                  title = "Median Across Groups",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  column(6, selectInput("select_group", "Select group", choices = NULL, width = "100%")),
                                  column(6),
                                  plotlyOutput("histogram_group", height = "600px") %>% withSpinner(color="steelblue")
                              ))
                     )
            ),
            tabPanel("PCA", 
                     fluidRow(
                       column(12, 
                              box(
                                width = NULL, 
                                title = "Principal Component Analysis",
                                status = "info",
                                solidHeader = TRUE,
                                collapsible = FALSE,
                                tagList(
                                  tags$ul(
                                    tags$li("Check log-transfomed checkbox if data is already log-transformed.")
                                  )
                                )
                              )
                       )
                     ),
                     fluidRow(
                       # Left column: PCA 1 and Scree Plot 1 (stacked vertically)
                       column(6,
                              box(
                                width = NULL,
                                title = "PCA 1",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                selectInput("selectpca1", "", choices = NULL, width = "100%"),
                                checkboxInput("pca1_islog", "Data is log-transformed.", value = FALSE, width = "100%"),
                                actionButton("run_pca1", "Run PCA", width = "50%") %>%
                                  bsTooltip("Check box if the data is log-transformed!", placement = "right", trigger = "hover"),
                                plotlyOutput("plotpca1", width = "100%"),
                                br(),
                                htmlOutput("pca1Details")
                              ),
                              box(
                                width = NULL,
                                title = "Scree plot 1",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotlyOutput("plotscree1", width = "100%")
                              )
                       ),
                       # Right column: PCA 2 and Scree Plot 2 (stacked vertically)
                       column(6,
                              box(
                                width = NULL,
                                title = "PCA 2",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                selectInput("selectpca2", "", choices = NULL, width = "100%"),
                                checkboxInput("pca2_islog", "Data is log-transformed.", value = FALSE, width = "100%"),
                                actionButton("run_pca2", "Run PCA", width = "50%"),
                                plotlyOutput("plotpca2", width = "100%"),
                                br(),
                                htmlOutput("pca2Details")
                              ),
                              box(
                                width = NULL,
                                title = "Scree plot 2",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotlyOutput("plotscree2", width = "100%")
                              )
                       )
                     )
            ),
            tabPanel("Feature drift",
                     fluidRow(
                       column(3, box(width = NULL, DTOutput("dt_drift_panel"))),
                       column(9, 
                              box(
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
            ),
            tabPanel("Feature viewer",
                     fluidRow(
                       column(3, box(
                         width = NULL, 
                         title = "Select Feature", 
                         DTOutput("dt_boxplot_panel")
                       )),
                       column(9, box(width = NULL, title = "Settings",
                                     fluidRow(
                                       column(6,
                                              textInput("boxplot_title", "Title", value = NULL),
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
            ),
            tabPanel("Outlier Detection",
                     tabsetPanel(
                       # K-means Tab
                       tabPanel("K-means",
                                # Row 1: Information
                                fluidRow(
                                  column(
                                    width = 12,
                                    box(
                                      width = NULL,
                                      title = "K-means Analysis Information",
                                      status = "info",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      tagList(
                                        tags$ul(
                                          tags$li("Evaluate cluster validity using methods silhouette, WSS, or gap statistic."),
                                          tags$li("Optionally, restrict clustering to selected groups for enhanced analysis."),
                                          tags$li("Results are visualized with plots and an outlier table displaying potential anomalies.")
                                        )
                                      )
                                    )
                                  )
                                ),
                                # Row 2: Selection and Parameters
                                fluidRow(
                                  column(
                                    width = 6,
                                    box(
                                      width = 12,
                                      title = "Data & Evaluation Selection",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      selectInput("kmeans_pca", "Choose PCA Results:", choices = NULL),
                                      # Group Selection Checkbox
                                      checkboxInput(
                                        inputId = "select_groups_kmeans",
                                        label = "Select Specific Groups",
                                        value = FALSE
                                      ),
                                      uiOutput("group_selection_ui_kmeans"),
                                      selectInput("kmeans_eval_method", "Choose Evaluation Method:",
                                                  choices = c("Within Sum of Square (WSS)" = "wss",
                                                              "Silhouette" = "silhouette",
                                                              "Gap Statistic" = "gap_stat")),
                                      actionButton("compute_kmeans_eval", "Compute Evaluation", width = "100%")
                                    )
                                  ),
                                  column(
                                    width = 6,
                                    box(
                                      width = 12,
                                      title = "K-means Parameters",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      numericInput("num_clusters", "Number of Clusters (k):", value = 3, min = 1, step = 1),
                                      numericInput("percentile_threshold", "Percentile Threshold:", value = 95, min = 0, max = 100, step = 1),
                                      actionButton("run_kmeans", "Run K-means", width = "100%")
                                    )
                                  )
                                ),
                                # Row 3: Results
                                fluidRow(
                                  column(
                                    width = 12,
                                    box(
                                      width = NULL,
                                      title = "K-means Results",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      plotlyOutput("kmeans_eval_plot") %>% withSpinner(color="steelblue"),
                                      br(),
                                      plotlyOutput("kmeans_plot") %>% withSpinner(color="steelblue"),
                                      DTOutput("kmeans_outliers") %>% withSpinner(color="steelblue")
                                    )
                                  )
                                )
                       ),
                       
                       # Hierarchical Clustering Tab
                       tabPanel("Hierarchical",
                                # Row 1: Information
                                fluidRow(
                                  column(
                                    width = 12,
                                    box(
                                      width = NULL,
                                      title = "Hierarchical Clustering Analysis Information",
                                      status = "info",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      tagList(
                                        tags$ul(
                                          tags$li("Hierarchical clustering builds a dendrogram based on the distance between samples."),
                                          tags$li("Users can specify the clustering method, number of clusters, and dendrogram threshold."),
                                          tags$li("It supports group selection to focus on subsets of the data."),
                                          tags$li("Dendrogram and cluster plots along with outlier tables help identify unusual samples.")
                                        )
                                      )
                                    )
                                  )
                                ),
                                # Row 2: Data and Parameters
                                fluidRow(
                                  column(
                                    width = 6,
                                    box(
                                      width = 12,
                                      title = "Data Selection",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      selectInput("hierarchical_pca", "Choose PCA Results:", choices = NULL),
                                      # Group Selection Checkbox
                                      checkboxInput(
                                        inputId = "select_groups_hierarchical",
                                        label = "Select Specific Groups",
                                        value = FALSE
                                      ),
                                      uiOutput("group_selection_ui_hierarchical")
                                    )
                                  ),
                                  column(
                                    width = 6,
                                    box(
                                      width = 12,
                                      title = "Clustering Parameters",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      selectInput("clustering_method", "Select Clustering Method:",
                                                  choices = c("Single" = "single",
                                                              "Complete" = "complete",
                                                              "Average" = "average",
                                                              "Ward's D2" = "ward.D2")),
                                      numericInput("num_clusters_hierarchical", "Number of Clusters (k):", value = 3, min = 1),
                                      numericInput("threshold", "Dendrogram Threshold (Distance):", value = 5, min = 0),
                                      actionButton("run_hierarchical", "Run Hierarchical Clustering", width = "100%")
                                    )
                                  )
                                ),
                                # Row 3: Results
                                fluidRow(
                                  column(
                                    width = 12,
                                    box(
                                      width = NULL,
                                      title = "Hierarchical Results",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      plotlyOutput("hclust_plot") %>% withSpinner(color="steelblue"),
                                      # plotlyOutput("conf_matrix_plot") %>% withSpinner(color="steelblue"),
                                      plotlyOutput("dendrogram_plot") %>% withSpinner(color="steelblue"),
                                      DTOutput("hierarchical_outliers") %>% withSpinner(color="steelblue")
                                    )
                                  )
                                )
                       ),
                       
                       # DBSCAN Tab
                       tabPanel("DBSCAN",
                                # Row 1: Information
                                fluidRow(
                                  column(
                                    width = 12,
                                    box(
                                      width = NULL,
                                      title = "DBSCAN Analysis Information",
                                      status = "info",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      tagList(
                                        tags$ul(
                                          tags$li("DBSCAN is a density-based algorithm that groups samples based on local point density."),
                                          tags$li("Requires setting epsilon(eps) and minimum points (minPts) parameters to define cluster boundaries."),
                                          tags$li("Filters out samples with low density as potential outliers."),
                                        )
                                      )
                                    )
                                  )
                                ),
                                # Row 2: Data and Parameters
                                fluidRow(
                                  column(
                                    width = 6,
                                    box(
                                      width = 12,
                                      title = "Data Selection",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      selectInput("dbscan_pca", "Choose PCA Results:", choices = NULL),
                                      # Group Selection Checkbox
                                      checkboxInput(
                                        inputId = "select_groups_dbscan",
                                        label = "Select Specific Groups",
                                        value = FALSE
                                      ),
                                      uiOutput("group_selection_ui_dbscan"),
                                      numericInput("knn", "Choose k for kNN Distance Plot:", value = 5, min = 1, step = 1),
                                      actionButton("compute_knn", "Compute kNN Distance Plot", width = "100%")
                                    )
                                  ),
                                  column(
                                    width = 6,
                                    box(
                                      width = 12,
                                      title = "DBSCAN Parameters",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      numericInput("eps", "Choose epsilon for DBSCAN:", value = 3, min = 0.01, step = 0.1),
                                      numericInput("min_pts_dbscan", "Choose minPts for DBSCAN:", value = 5, min = 1),
                                      actionButton("run_dbscan", "Run DBSCAN", width = "100%")
                                    )
                                  )
                                ),
                                # Row 3: Results
                                fluidRow(
                                  column(
                                    width = 12,
                                    box(
                                      width = NULL,
                                      title = "DBSCAN Results",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      plotlyOutput("knn_plot") %>% withSpinner(color="steelblue"),
                                      plotlyOutput("dbscan_plot") %>% withSpinner(color="steelblue"),
                                      DTOutput("dbscan_outliers") %>% withSpinner(color="steelblue")
                                    )
                                  )
                                )
                       ),
                       
                       # HDBSCAN Tab
                       tabPanel("HDBSCAN",
                                # Row 1: Information
                                fluidRow(
                                  column(
                                    width = 12,
                                    box(
                                      width = NULL,
                                      title = "HDBSCAN Analysis Information",
                                      status = "info",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      tagList(
                                        tags$ul(
                                          tags$li("HDBSCAN extends DBSCAN by converting it into a hierarchical clustering approach."),
                                          tags$li("Uses a minimum points parameter and an outlier threshold to classify data points."),
                                          tags$li("Allows selection of groups to refine the clustering process."),
                                          tags$li("Plots and outlier tables provide a detailed view of anomalous samples.")
                                        )
                                      )
                                    )
                                  )
                                ),
                                # Row 2: Data and Parameters
                                fluidRow(
                                  column(
                                    width = 6,
                                    box(
                                      width = 12,
                                      title = "Data Selection",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      selectInput("hdbscan_pca", "Choose PCA Results:", choices = NULL),
                                      # Group Selection Checkbox
                                      checkboxInput(
                                        inputId = "select_groups_hdbscan",
                                        label = "Select Specific Groups",
                                        value = FALSE
                                      ),
                                      uiOutput("group_selection_ui_hdbscan")
                                    )
                                  ),
                                  column(
                                    width = 6,
                                    box(
                                      width = 12,
                                      title = "HDBSCAN Parameters",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      numericInput("min_pts_hdbscan", "Choose minPts for HDBSCAN:", value = 5, min = 1),
                                      numericInput("threshold_hdbscan", "Outlier Threshold for HDBSCAN:", value = 0.85, min = 0.01, max = 1),
                                      actionButton("run_hdbscan", "Run HDBSCAN", width = "100%")
                                    )
                                  )
                                ),
                                # Row 3: Results
                                fluidRow(
                                  column(
                                    width = 12,
                                    box(
                                      width = NULL,
                                      title = "HDBSCAN Results",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      plotlyOutput("hdbscan_plot") %>% withSpinner(color="steelblue"),
                                      DTOutput("hdbscan_outliers") %>% withSpinner(color="steelblue")
                                    )
                                  )
                                )
                       ),
                       
                       # OPTICS Tab
                       tabPanel("OPTICS",
                                # Row 1: Information
                                fluidRow(
                                  column(
                                    width = 12,
                                    box(
                                      width = NULL,
                                      title = "OPTICS Analysis Information",
                                      status = "info",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      tagList(
                                        tags$ul(
                                          tags$li("OPTICS orders samples based on reachability distances to reveal cluster structure."),
                                          tags$li("Generates reachability and threshold plots for visualizing cluster boundaries."),
                                          tags$li("Parameters such as eps, minPts, and eps cl can be adjusted to fine-tune analysis."),
                                          tags$li("The results help identify outliers and are displayed through multiple visualizations.")
                                        )
                                      )
                                    )
                                  )
                                ),
                                # Row 2: Data and Parameters
                                fluidRow(
                                  column(
                                    width = 6,
                                    box(
                                      width = 12,
                                      title = "Data Selection",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      selectInput("optics_pca", "Choose PCA Results:", choices = NULL),
                                      # Group Selection Checkbox
                                      checkboxInput(
                                        inputId = "select_groups_optics",
                                        label = "Select Specific Groups",
                                        value = FALSE
                                      ),
                                      uiOutput("group_selection_ui_optics"),
                                    )
                                  ),
                                  column(
                                    width = 6,
                                    box(
                                      width = 12,
                                      title = "OPTICS Parameters",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      numericInput("min_pts_optics", "Choose minPts for OPTICS:", value = 5, min = 1),
                                      numericInput("eps_optics", "Choose eps for OPTICS (optional):", value = NA, min = 0.1, step = 0.1),
                                      numericInput("eps_cl_optics", "Choose cutoff (eps_cl) for OPTICS:", value = 0.5, min = 0.1, step = 0.1)
                                    )
                                  )
                                ),
                                # Row 3: Results
                                fluidRow(
                                  column(
                                    width = 12,
                                    box(
                                      width = NULL,
                                      title = "OPTICS Results",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      plotOutput("optics_reachability_plot") %>% withSpinner(color="steelblue"),
                                      plotOutput("reachability_plot_threshold") %>% withSpinner(color="steelblue"),
                                      plotlyOutput("cluster_plot") %>% withSpinner(color="steelblue"),
                                      DTOutput("optics_outliers") %>% withSpinner(color="steelblue")
                                    )
                                  )
                                )
                       ),
                       
                       # LOF Tab
                       tabPanel("LOF",
                                # Row 1: Information
                                fluidRow(
                                  column(
                                    width = 12,
                                    box(
                                      width = NULL,
                                      title = "LOF Analysis Information",
                                      status = "info",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      tagList(
                                        tags$ul(
                                          tags$li("LOF measures the local density deviation of a sample relative to its neighbors."),
                                          tags$li("Uses a threshold and a k parameter to compute an outlier score for each sample."),
                                          tags$li("Highlights samples with significantly lower local density as outliers."),
                                          tags$li("Interactive LOF plots and a summary table help in interpreting and identifying anomalies.")
                                        )
                                      )
                                    )
                                  )
                                ),
                                # Row 2: Data and Parameters
                                fluidRow(
                                  column(
                                    width = 6,
                                    box(
                                      width = 12,
                                      title = "Data Selection",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      selectInput("lof_pca", "Choose PCA Results:", choices = NULL),
                                      # Group Selection Checkbox
                                      checkboxInput(
                                        inputId = "select_groups_lof",
                                        label = "Select Specific Groups",
                                        value = FALSE
                                      ),
                                      uiOutput("group_selection_ui_lof")
                                    )
                                  ),
                                  column(
                                    width = 6,
                                    box(
                                      width = 12,
                                      title = "LOF Parameters",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      numericInput("lof_threshold", "Threshold for LOF:", value = 1.5, min = 0, step = 0.1),
                                      numericInput("lof_k", "k for LOF:", value = 4, min = 1),
                                      actionButton("run_lof", "Run LOF", width = "100%")
                                    )
                                  )
                                ),
                                # Row 3: Results
                                fluidRow(
                                  column(
                                    width = 12,
                                    box(
                                      width = NULL,
                                      title = "LOF Results",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      plotlyOutput("lof_plot") %>% withSpinner(color="steelblue"),
                                      plotlyOutput("lof_od_plot") %>% withSpinner(color="steelblue"),
                                      DTOutput("lof_outliers") %>% withSpinner(color="steelblue")
                                    )
                                  )
                                )
                       )
                     )
            ),
            tabPanel("Visualization",
                     tabsetPanel(
                       tabPanel(
                         "Heatmap",
                         
                         # Row 1: Heatmap Analysis Information
                         fluidRow(
                           column(
                             width = 12,
                             box(
                               width = NULL,
                               title = "Heatmap Analysis",
                               status = "info",
                               solidHeader = TRUE,
                               collapsible = FALSE,
                               tagList(
                                 tags$ul(
                                   tags$li("Heatmaps provide a visual representation of high-dimensional data and clustering patterns."),
                                   tags$li("Select the desired dataset and, optionally, specific groups for comparison."),
                                   tags$li("Customize clustering, labeling, and display options to best reveal data patterns.")
                                 )
                               )
                             )
                           )
                         ),
                         
                         # Row 2: Controls & Customizations (side-by-side)
                         fluidRow(
                           # Left box: Heatmap Controls
                           column(
                             width = 6,
                             box(
                               width = 12,
                               title = "Heatmap Controls",
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = FALSE,
                               
                               selectInput(
                                 inputId = "select_heatmap_data",
                                 label = "Select Dataset for Heatmap:",
                                 choices = NULL, 
                                 width = "100%"
                               ),
                               selectInput(
                                 inputId = "heatmap_labels",
                                 label = "Select Label Column:",
                                 choices = NULL,
                                 width = "100%"
                               ),
                               checkboxInput(
                                 inputId = "enable_grouping_heatmap",
                                 label = "Select column for grouping features: ",
                                 value = FALSE
                               ),
                               uiOutput("grouping_column_ui"),
                               checkboxInput(
                                 inputId = "select_groups_heatmap",
                                 label = "Select Specific Groups",
                                 value = FALSE
                               ),
                               # Dynamic UI for Group Selection (appears when 'select_groups_heatmap' is TRUE)
                               uiOutput("group_selection_ui_heatmap"),
                               checkboxInput("heatmap_islog", "Data is log-transformed.", value = FALSE, width = "100%"),
                               helpText("Check if the data is log-transformed."),
                               # Top Features Selection
                               numericInput(
                                 inputId = "top_x",
                                 label = "Number of Top Features:",
                                 value = 25,
                                 min = 1,
                                 step = 1
                               ),
                               actionButton("run_heatmap", "Generate Heatmap", width = "100%")
                             )
                           ),
                           
                           # Right box: Heatmap Customizations
                           column(
                             width = 6,
                             box(
                               width = 12,
                               title = "Heatmap Customizations",
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = FALSE,
                               
                               textInput("heatmap_title", "Plot Title:", "My Heatmap"),
                               
                               # select input for clustering_distance_rows
                               selectInput("clustering_distance_rows", "Clustering Distance Metric (Rows):",
                                           choices = c("Euclidean" ="euclidean",
                                                       "Maximum" = "maximum",
                                                       "Manhattan" = "manhattan",
                                                       "Canberra" = "canberra",
                                                       "Binary" = "binary",
                                                       "Minkowski" = "minkowski",
                                                       "Pearson" =  "pearson",
                                                       "Spearman" = "spearman",
                                                       "Kendall" = "kendall"),
                                           selected = "euclidean"),
                               selectInput("clustering_method_rows", "Clustering Method (Rows):",
                                           choices = c("Ward D" = "ward.D",
                                                       "Ward D2" = "ward.D2",
                                                       "Single" = "single",
                                                       "Complete" = "complete",
                                                       "Average" = "average",
                                                       "McQuitty" = "mcquitty",
                                                       "Median" = "median",
                                                       "Centroid" = "centroid"),
                                           selected = "ward.D2"),
                               
                               checkboxInput("show_column_names", "Show Column Names:", FALSE),
                               helpText("Toggle to display or hide column names."),
                               checkboxInput("show_row_names", "Show Row Names:", FALSE),
                               helpText("Toggle to display or hide row names."),
                               checkboxInput("cluster_rows", "Cluster Rows:", TRUE),
                               helpText("Enable or disable hierarchical clustering of rows."),
                               checkboxInput("show_row_dend", "Show Row Dendrogram:", FALSE),
                               helpText("Toggle the visibility of the row dendrogram.")
                             )
                           )
                         ),
                         
                         # Row 3: Heatmap Plot & Results
                         fluidRow(
                          #   box(
                          #     width = NULL,
                          #     title = "Heatmap Plot",
                          #     status = "primary",
                          #     solidHeader = TRUE,
                          #     collapsible = TRUE,
                               
                          #     plotOutput("heatmap_plot", height = "600px") %>% withSpinner(color = "steelblue"),
                               
                          #  ),
                           box(
                              title = "Interactive Heatmap", width = 12, status = "primary", solidHeader = TRUE,
                              originalHeatmapOutput("heatmap_interactive")
                           ),
                           #box(
                           #   title = "Sub-heatmap", width = 12, solidHeader = TRUE, status = "primary",
                           #   subHeatmapOutput("heatmap_interactive", title = NULL)
                           #),
                           box(
                             title = "Ouput", width = 12, status = "primary", solidHeader = TRUE,
                             HeatmapInfoOutput("heatmap_interactive", title = NULL)
                           )
                        ),
                         fluidRow(
                           column(
                             width = 12,
                             box(
                               width = NULL,
                               title = "Heatmap Results",
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               DT::dataTableOutput("heatmap_table")
                             )
                           )
                         )
                       ),
                       tabPanel(
                         "Circular Barplot",
                         
                         # Information Box
                         fluidRow(
                           column(
                             width = 12,
                             box(
                               width = NULL,
                               title = "Circular Barplot Information",
                               status = "info",
                               solidHeader = TRUE,
                               collapsible = FALSE,
                               tagList(
                                 tags$ul(
                                   tags$li("Circular barplots display ranked features in a visually engaging, circular format."),
                                   tags$li("Select the top features and define groupings to compare differences effectively."),
                                   tags$li("Customize colors and labels to enhance the visual interpretation of the data.")
                                 )
                               )
                             )
                           )
                         ),
                         
                         # Data & Selection UI
                         fluidRow(
                           column(
                             width = 12,
                             box(
                               width = NULL,
                               title = "Circular Barplot Settings",
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = FALSE,
                               
                               selectInput(
                                 inputId = "select_data_circular_barplot",
                                 label = "Select Dataset for Circular Barplot",
                                 choices = NULL,  
                                 width = "100%"
                               ),
                               
                               fluidRow(
                                 column(
                                   width = 6,
                                   selectInput("group1_cirbar", "Group of Interest:", choices = NULL, width = "100%")
                                 ),
                                 column(
                                   width = 6,
                                   selectInput("group2_cirbar", "Reference Group:", choices = NULL, width = "100%")
                                 )
                               ),
                               
                               fluidRow(
                                 column(
                                   width = 6,
                                   selectInput("name_column_cirbar", "Select Naming Column", choices = NULL, width = "100%")
                                 ),
                                 column(
                                   width = 6,
                                   selectInput("group_column_cirbar", "Select Grouping Column", choices = NULL, width = "100%")
                                 )
                               ),
                               
                               fluidRow(
                                 column(
                                   width = 6,
                                   numericInput("top_x_cirbar", "Select Top Features:", value = 100, min = 1, step = 1, width = "100%")
                                 ),
                                 column(
                                   width = 6,
                                   selectInput("feature_cirbar", "Select Plotting Feature:", choices = NULL, width = "100%")
                                 )
                               ),
                               
                               # Run Plot Button
                               actionButton(
                                 inputId = "run_circular_barplot",
                                 label = "Run Plot",
                                 width = "100%"
                               )
                             )
                           )
                         ),
                         
                         # Circular Barplot Output
                         fluidRow(
                           column(
                             width = 12,
                             box(
                               width = NULL,
                               title = "Circular Barplot",
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               plotOutput("circular_barplot", height = "800px") %>% withSpinner(color = "steelblue")
                             )
                           )
                         )
                       )
                       ,
                       
                       tabPanel("Lipid Heatmap",
                                useShinyjs(),
                                tabsetPanel(
                                  
                                  tabPanel("Group selection",
                                           fluidRow(
                                             column(6, box(
                                               width = NULL,
                                               title = "User guide",
                                               tags$ul(
                                                 tags$li("Select the data frame to use."),
                                                 tags$li("Click 'Run Data Processing' to start the analysis."),
                                                 tags$li("After data processing select the Group of Interest and Reference Groups."),
                                                 tags$li("Go to the 'Lipid Visualization' tab set thresholds and see the results"),
                                                 tags$li("Have fun!"),
                                                 br(),
                                                 actionButton("df_explain", "What is the differnt data frames?"),
                                                 br(),
                                                 conditionalPanel(
                                                   condition = "input.run_process > 0", # Only display the following if data processing has been triggered.
                                                   br(),
                                                   tableOutput("groups_table"), # Shows grouped data of samples
                                                 )
                                               )
                                             )),
                                             column(6, box(
                                               width = NULL, 
                                               title = "Data Frame and group selection:", 
                                               radioButtons("selected_dataset", "Data frames",
                                                            choices = c("Original Data" = "original", "Merged Data" = "merged"),
                                                            selected = "original", width = "50%"),
                                               
                                               column(6, 
                                                      actionButton("run_process", "Run Data Processing"),
                                                      uiOutput("select_group_ui_heatmap"),
                                                      tableOutput("numerator_table"),
                                                      tableOutput("denominator_table"),
                                                      conditionalPanel(
                                                        condition = "input.run_process > 0", 
                                                        actionButton("show_lipid_info", label = "Lipid Summary", icon = icon("info-circle")),
                                                        actionButton("show_lipid_cal", label = "Calculation Summary", icon = icon("calculator")),
                                                        actionButton("show_lipid_remove", label = "Filtered Summary", icon = icon("filter")),
                                                        actionButton("lipid_contact", label = "Any questions?", icon = icon("question-circle"))
                                                        )
                                               )
                                             ))
                                           ),
                                           
                                           conditionalPanel(
                                             condition = "input.run_process > 0", 
                                             column(6, box(
                                               title = "Group of Interest Table",
                                               width = NULL,
                                               DT::dataTableOutput("numerator_group_table")  
                                             )),
                                             column(6, box(
                                               title = "Reference Group Table",
                                               width = NULL,
                                               DT::dataTableOutput("denominator_group_table")  
                                             ))
                                           )
                                  ),
                                  
                                  ####  VISUALIZATION TAB 
                                  
                                  tabPanel(
                                    "Lipid Visualization",
                                    conditionalPanel(
                                      condition = "input.run_process > 0",
                                      # First row with Lipid Selection and Plot Settings side by side
                                      fluidRow(
                                        # Column for Lipid Selection and Plot Settings
                                        column(
                                          width = 6,
                                          box(
                                            width = NULL,
                                            title = "Plot Settings", solidHeader = TRUE,
                                            uiOutput("select_lipid_ui"),
                                            tags$li("Lipids will show up if they are within the threshold of p-value and logFC."),
                                            tags$li("Default settings: lipids displayed: all, logFC: 0, p-value: 1, p-adj: 1, min. lipids: 1 or 2."),
                                            
                                            br(),
                                            br(),
                                            uiOutput("logFC_input_ui"),
                                            uiOutput("p_value_max_ui"),
                                            uiOutput("p_value_adj"),
                                            uiOutput("min_lipids_per_class_ui"),
                                            checkboxInput("split_screen", "Show Heatmap and Table side by side", value = FALSE),
                                            checkboxInput("show_grid", "Display Grid Lines", value = TRUE),
                                            
                                            br(),
                                            # Add the action button to open the modal dialog
                                            actionButton("download_heatmap_btn", "Download Heatmap Image"),
                                            br(),
                                            # Add the UI output for the warning message
                                            uiOutput("filteredDataWarning")
                                          )
                                        ),
                                        
                                        # Column for Color Settings
                                        column(
                                          width = 6,
                                          box(
                                            width = NULL,
                                            title = "Settings", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                            tabBox(
                                              width = NULL,
                                              tabPanel("Colors",
                                                       colourInput("low_color", "Negative logFC color", value = "#4575b4"),
                                                       colourInput("mid_color", "Mid logFC color", value = "white"),
                                                       colourInput("high_color", "Positive logFC color", value = "#d73027"),
                                                       colourInput("panel_bg_color", "Panel Background Color", value = "#D3D3D3"),
                                                       colourInput("strip_bg_color", "Strip Background Color", value = "#3483d1"),
                                                       colourInput("strip_text_color", "Strip Text Color", value = "black")
                                              ),
                                              tabPanel("Text Sizes",
                                                       numericInput("strip_text_size", "Strip Text Size", value = 16),
                                                       numericInput("axis_text_x_size", "X-axis Numbers Size", value = 12),
                                                       numericInput("axis_text_y_size", "Y-axis Numbers Size", value = 12),
                                                       numericInput("axis_title_size", "Axis Title Size", value = 20),
                                                       numericInput("legend_title_size", "Legend Title Size", value = 18),
                                                       numericInput("legend_text_size", "Legend Numbers Size", value = 14),
                                                       numericInput("axis_title_x_size", "X-axis Title Size", value = 20),
                                                       numericInput("axis_title_y_size", "Y-axis Title Size", value = 20),
                                                       numericInput("plot_title_size", "Plot Title Size", value = 22)
                                              ),
                                              tabPanel("Axis Settings",
                                                       numericInput("axis_text_x_angle", "X-axis Text Angle", value = 90, min = 0, max = 360),
                                              ),
                                              tabPanel("Legend Settings",
                                                       numericInput("barwidth", "LogFC Bar Width", value = 15),
                                                       numericInput("barheight", "LogFC Bar Height", value = 1)
                                              ),
                                              tabPanel("Labels",
                                                       textInput("x_axis_label", "X-axis Label", value = "Number of fatty-acid carbon atoms"),
                                                       textInput("y_axis_label", "Y-axis Label", value = "Number of fatty-acid double bonds"),
                                                       textInput("main_title", "Main Title", value = "")
                                              )
                                            )
                                          )
                                        ),
                                        
                                        column(
                                          width = 6,
                                          box(
                                            width = NULL,
                                            radioButtons(
                                              inputId = "selected_logfc_sclae_bar",
                                              label = "Select logFC scale bar:",
                                              choices = c("Manual" = "Manual", "Dynamic Range" = "dynamic"),
                                              selected = "Manual"
                                            ),
                                            conditionalPanel(
                                              condition = "input.selected_logfc_sclae_bar == 'Manual'",
                                              numericInput(
                                                inputId = "logFC_scale_manual",
                                                label = "Set Manual logFC Scale:",
                                                value = 2.5,  
                                                step = 0.1,
                                              ),
                                              tags$li(
                                                "Lipids in the heatmap with a '+' sign exceed the upper logFC scale, 
                                   while those with a '-' sign fall below the lower logFC scale. 
                                   Adjust the 'logFC scale bar input' to change this."
                                              )
                                            ),
                                            uiOutput("selected_groups_heatmap_text"),
                                          )
                                        )
                                        
                                        
                                      )
                                    ),
                                    
                                    # Output for the visualization
                                    uiOutput("visualization_ui"),
                                    tags$div(
                                      class = "alert-warning",
                                      uiOutput("table_message_2")
                                    )
                                  ),
                                  
                                  ####  HEATMAP TABLE TAB 
                                  
                                  tabPanel(
                                    "Table of Heatmap",
                                    column(width = 12,
                                           dataTableOutput("pValueTable")
                                    ),
                                    tags$div(
                                      class = "alert-warning",
                                      uiOutput("table_message_1")
                                    )
                                  ),
                                  
                                  ####  BUBBLE PLOT TAB
                                  
                                  #tabPanel(
                                  #"Bubble plot of data",
                                  
                                  #uiOutput("bubble_plot_ui"),
                                  #tags$div(
                                  #class = "alert-warning", # You can change this to 'alert-success', 'alert-warning', etc.
                                  #uiOutput("table_message")
                                  #)
                                  #)
                                  
                                )
                       ), #### End of Lipid Heatmap ####
                       
                       tabPanel(
                         "Volcano Plot",
                         fluidRow(
                           column(
                             width = 12,
                             box(
                               width = NULL,
                               title = "Volcano Plot Information",
                               status = "info",
                               solidHeader = TRUE,
                               collapsible = FALSE,
                               tagList(
                                 tags$ul(
                                   tags$li("Volcano plots highlight statistically significant changes by plotting fold change versus significance."),
                                   tags$li("Select a dataset, define two distinct groups, and set thresholds for p-values and fold changes."),
                                   tags$li("Customize colors to easily distinguish between upregulated, downregulated, and non-significant features.")
                                 )
                               )
                             )
                           )
                         ),
                         fluidRow(
                           # Left box: Data & group selection
                           column(
                             width = 6,  # half width, you can also use 4/8, etc.
                             box(
                               width = 12,
                               title = "Data & Group Selection",
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = FALSE,
                               
                               selectInput("select_volcano_data", "Select Dataset for volcano plot:", choices = NULL, width = "100%"),
                               selectInput("volcano_labels", "Select Label Column:", choices = NULL, width = "100%"),
                               
                               fluidRow(
                                 column(
                                   width = 6,
                                   selectInput("group1_vol", "Group of Interest:", choices = NULL)
                                 ),
                                 column(
                                   width = 6,
                                   selectInput("group2_vol", "Reference Group:", choices = NULL)
                                 )
                               ),
                               
                               fluidRow(
                                 column(
                                   width = 6,
                                   selectInput("pAdjustMethod_volcano", "P-Adjust Method:",
                                               choices = c("Holm" = "holm",
                                                           "Hochberg" = "hochberg",
                                                           "Hommel" = "hommel",
                                                           "Bonferroni" = "bonferroni",
                                                           "Benjamini & Hochberg" = "BH",
                                                           "Benjamini & Yekutieli" = "BY",
                                                           "FDR" = "fdr",
                                                           "None" = "none"),
                                               selected = "fdr"
                                   )
                                 ),
                                 column(width = 6,
                                        selectInput("pval_col_volcano",
                                                    "Display p-value column:",
                                                    choices = c("P-value" = "p.value",
                                                                "Adjusted P-value" = "p.adj"),
                                                    selected = "p.adj"
                                        )
                                 )
                               ),
                               
                               fluidRow(
                                 column(
                                   width = 6,
                                   numericInput("log2fc_threshold", "Log2 FC Threshold:", value = 2, min = 0)
                                 ),
                                 column(
                                   width = 6,
                                   numericInput("pval_threshold", "p-value Threshold:", value = 0.05, min = 0, max = 1, step = 0.01)
                                 )
                               ),
                               
                               actionButton("run_volcano_plot", "Generate Volcano Plot", width = "100%")
                             )
                           ),
                           
                           # Right box: Volcano Plot Customization
                           column(
                             width = 6,
                             box(
                               width = 12,
                               title = "Volcano Plot Customization",
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = FALSE,
                               
                               textInput("volcano_title", "Plot Title:", "My Volcano Plot"),
                               
                               # We create three fluidRows, each with two columns (6 color pickers total)
                               
                               # 1) Up-regulated colors
                               fluidRow(
                                 column(
                                   width = 6,
                                   colourInput("color_up_fill", "Up Fill:", value = "#FF0909")
                                 ),
                                 column(
                                   width = 6,
                                   colourInput("color_up_outline", "Up Outline:", value = "#B10E0E")
                                 )
                               ),
                               
                               # 2) Down-regulated colors
                               fluidRow(
                                 column(
                                   width = 6,
                                   colourInput("color_down_fill", "Down Fill:", value = "#0E66E2")
                                 ),
                                 column(
                                   width = 6,
                                   colourInput("color_down_outline", "Down Outline:", value = "#0A3E88")
                                 )
                               ),
                               
                               # 3) Non-significant colors
                               fluidRow(
                                 column(
                                   width = 6,
                                   colourInput("color_ns_fill", "NS Fill:", value = "#808080")
                                 ),
                                 column(
                                   width = 6,
                                   colourInput("color_ns_outline", "NS Outline:", value = "#000000")
                                 )
                               ),
                               
                               # Additional toggles, e.g. to show/hide legend
                               checkboxInput("show_legend_volcano", "Show Legend:", TRUE),
                               checkboxInput(
                                 inputId = "select_parameter_volcano",
                                 label = "Select axis parameters",
                                 value = FALSE
                               ),
                               # Dynamic UI for Group Selection (appears when 'select_groups_heatmap' is TRUE)
                               uiOutput("parameter_selection_ui_volcano"),
                             )
                           )
                         ),
                         
                         fluidRow(
                           column(
                             width = 6,
                             box(
                               width = 12,
                               title = "Individual Feature Selection",
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               
                               # Checkbox to enable/disable individual feature selection
                               checkboxInput("enable_feature_selection", "Enable Feature Selection", FALSE),
                               
                               # Dynamic UI: Feature selection (searchable list appears only if checked)
                               uiOutput("feature_selection_ui_volcano")
                             )
                           ),
                           
                           column(
                             width = 6,
                             box(
                               width = 12,
                               title = "Group Selection",
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               
                               # Checkbox to enable/disable group selection
                               checkboxInput("enable_group_selection", "Enable Group Selection", FALSE),
                               
                               # Dynamic UI: Group selection (searchable list appears only if checked)
                               uiOutput("group_selection_ui_volcano"),
                               uiOutput("group_color_ui")
                               
                             )
                           )
                         ),
                         
                         # Row 3: Volcano Plot & Results (full width)
                         fluidRow(
                           column(
                             width = 12,
                             box(
                               width = NULL,
                               title = "Volcano Plot & Results",
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = FALSE,
                               
                               plotlyOutput("volcano_plot", height = "600px") %>% withSpinner(color="steelblue"),
                               # br(),
                               # downloadButton("downloadPlot_volcano", "Download Volcano Plot"),
                               br(),
                               DTOutput("volcano_table")
                             )
                           )
                         )
                       ),
                       
                       tabPanel(title = "Odds Ratio Plot",
                                fluidRow(
                                  column(
                                    width = 12,
                                    box(
                                      width = NULL,
                                      title = "Odds Ratio plot Information",
                                      status = "info",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      tagList(
                                        tags$ul(
                                          tags$li("Odds ratio analysis quantifies the association between features and group differences."),
                                          tags$li("Transform the data and apply logistic regression to compute odds ratios with confidence intervals."),
                                          tags$li("Visualize the results with plots and tables to interpret significant associations clearly.")
                                        )
                                      )
                                    )
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 6,
                                    box(
                                      width = 12,
                                      title = "Data & Group Selection",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      
                                      selectInput("select_OR_data", "Select Dataset for Odds Ratio plot:", choices = NULL, width = "100%"),
                                      fluidRow(
                                        column(
                                          width = 6,
                                          selectInput("group1_OR", "Group of Interest:", choices = NULL)
                                        ),
                                        column(
                                          width = 6,
                                          selectInput("group2_OR", "Reference Group:", choices = NULL)
                                        ),
                                        column(
                                          width = 6,
                                          selectInput("OR_main_label", "Main group Column:", choices = NULL),
                                        ),
                                        column(
                                          width = 6,
                                          selectInput("OR_sub_label", "Sub group Column:", choices = NULL)
                                        ),
                                        column(
                                          width = 6,
                                          selectInput("OR_feature_label", "Feature name column:", choices = NULL)
                                        ),
                                        column(
                                          width = 6,
                                          selectInput("selected_groups_OR", 
                                                      "Select Groups:", 
                                                      choices = NULL, 
                                                      multiple = TRUE,
                                                      width = "100%")
                                        ),
                                      ),
                                      actionButton("run_OR_plot", "Odds Ratio Plot", width = "100%")
                                    )
                                  ),
                                  box(
                                    width = 12,
                                    title = "OR Plot",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    plotOutput("OR_plot", height = "1000px") %>% withSpinner(color="steelblue")
                                  ),
                                  box(width = 12,
                                      title = "OR Results Table",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      DTOutput("OR_table")  %>% withSpinner(color="steelblue"))
                                )
                       )
                     )
            ),
            # UI for Pathway Analysis
            tabPanel(
              "Pathway Analysis",
              tabsetPanel(
                # Over-Representation Analysis tab
                tabPanel(
                  "Over-Representation Analysis (ORA)",
                  
                  # Information Box
                  fluidRow(
                    column(
                      width = 12,
                      box(width = NULL, title = "Pathway Enrichment Information", status = "info",
                        solidHeader = TRUE, collapsible = FALSE,
                        tagList(
                          tags$ul(
                            tags$li("Pathway enrichment analysis identifies over-represented biological pathways in your data."),
                            tags$li("Set appropriate thresholds and select the correct groups to reveal significant pathways.")))))),
                  
                  # Data & Selection UI
                  fluidRow(column(width = 6,
                      box(width = 12, title = "Data & Identifier Selection", status = "primary",
                        solidHeader = TRUE, collapsible = FALSE,
                        
                        selectInput("select_data_for_enrichment", "Select Dataset for Identifier Gathering", choices = NULL, width = "100%"),
                        
                        fluidRow(
                          column(width = 6, selectInput("identifier_column", "Select InChI Column",
                                                        choices = NULL, width = "100%")),
                          column(width = 6, selectInput("compound_column", "Select Compound Column", 
                                                        choices = NULL, width = "100%"))
                        ),
                        
                        actionButton("run_gather_identifiers", "Gather Identifiers", width = "100%"),
                        
                        fluidRow(
                          column(
                            width = 6,
                            checkboxInput("gene_selected", "Pathway Enrichment", TRUE)
                          ),
                          column(
                            width = 6,
                            checkboxInput("module_selected", "Module Enrichment", FALSE)
                          )
                        ),
                        
                        fluidRow(
                          column(
                            width = 6,
                            selectInput("group1_enrichment", "Group of Interest", choices = NULL, width = "100%")
                          ),
                          column(
                            width = 6,
                            selectInput("group2_enrichment", "Reference Group", choices = NULL, width = "100%")
                          )
                        ),
                        
                        actionButton("run_enrichment_analysis", "Run ORA", width = "100%")
                      )
                    ),
                    
                    # Enrichment Settings & Network Customizations
                    column(
                      width = 6,
                      box(
                        title = "Enrichment Settings",
                        width = 12,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        fluidRow(
                          column(
                            width = 6,
                            numericInput("top_x_enrich", "Number of Top Features:", value = 20, min = 1, step = 1),
                            numericInput("p_value_threshold_enrich", "Adjusted P-value Threshold:", value = 0.05, min = 0, max = 1, step = 0.01)
                          ),
                          column(
                            width = 6,
                            selectInput("pAdjustMethod_enrich", "P-Adjust Method:",
                                        choices = c("Holm" = "holm",
                                                    "Hochberg" = "hochberg",
                                                    "Hommel" = "hommel",
                                                    "Bonferroni" = "bonferroni",
                                                    "Benjamini & Hochberg" = "BH",
                                                    "Benjamini & Yekutieli" = "BY",
                                                    "FDR" = "fdr",
                                                    "None" = "none"),
                                        selected = "fdr"
                            ),
                            selectInput("color_con_enrich", "Color based on Method:",
                                        choices = c("Adjusted P value" = "p.adjust",
                                                    "P-value" = "pvalue",
                                                    "Q-value" = "qvalue"),
                                        selected = "p.adjust"
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 6,
                            numericInput("minGSSize_enrich", "min GS Size Threshold:", value = 1, min = 0, max = 1e6, step = 1)
                          ),
                          column(
                            width = 6,
                            numericInput("maxGSSize_enrich", "max GS Size Threshold:", value = 500, min = 0, max = 1e6, step = 1)
                          )
                        ),
                        fluidRow(
                          column(
                            width = 6,
                            numericInput("qvalueCutoff_enrich", "Q-value Threshold:", value = 0.05, min = 0, max = 1, step = 0.01)
                          )
                        )
                      ),
                      box(
                        title = "Network Graph Customizations",
                        width = 12,
                        status = "info",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        fluidRow(
                          column(
                            width = 6,
                            selectInput("layout_option", "Choose Layout:",
                                        choices = c(
                                          "Nicely" = "nicely",
                                          "Kamada-Kawai" = "kk",
                                          "Fruchterman-Reingold" = "fr",
                                          "DrL Layout" = "drl",
                                          "Linear" = "linear",
                                          "Star" = "star",
                                          "Circular" = "circle",
                                          "Davidson-Harel" = "dh",
                                          "Graph Optimization" = "graphopt",
                                          "Grid Layout" = "grid",
                                          "Multidimensional Scaling" = "mds",
                                          "Random" = "randomly"
                                        ),
                                        selected = "nicely"
                            ),
                            sliderInput("node_size_mult", "Node Size Multiplier:", min = 0.5, max = 3, value = 1, step = 0.1)
                          ),
                          column(
                            width = 6,
                            sliderInput("node_text_size", "Node Text Size:", min = 1, max = 6, value = 3, step = 0.5),
                            sliderInput("edge_alpha", "Edge Transparency:", min = 0, max = 1, value = 0.5, step = 0.1)
                          )
                        ),
                        fluidRow(
                          column(
                            width = 6,
                            sliderInput("edge_width_scale", "Edge Width Scale:", min = 0.5, max = 5, value = 1, step = 0.1)
                          ),
                          column(
                            width = 6,
                            selectInput("node_color_by", "Color Nodes By:",
                                        choices = c("super_class", "main_class", "sub_class"),
                                        selected = "super_class"
                            )
                          )
                        )
                      )
                    ),
                    
                    # Identifiers Count
                    column(
                      width = 12,
                      box(
                        width = 12,
                        title = "Data summary",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        
                        textOutput("identifier_count_text"),
                        DTOutput("identifier_count_table")
                      )
                    )
                  ),
                  
                  # Enrichment Plots
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        title = "Enrichment Barplot and Dotplot",
                        width = NULL,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        fluidRow(
                          column(
                            width = 6,
                            plotOutput("enrichment_barplot", height = "600px") %>% withSpinner(color = "steelblue")
                          ),
                          column(
                            width = 6,
                            plotOutput("enrichment_dotplot", height = "600px") %>% withSpinner(color = "steelblue")
                          )
                        )
                      )
                    )
                  ),
                  
                  # Enrichment Network Graph
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        title = "Enrichment Network Graph",
                        width = NULL,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        fluidRow(
                          column(
                            width = 12,
                            plotOutput("enrichment_cnetplot", height = "700px") %>% withSpinner(color = "steelblue")
                          )
                        )
                      )
                    )
                  ),
                  
                  # Enrichment Table
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        width = NULL,
                        title = "Enrichment Table",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        DTOutput("enrichment_table")
                      )
                    )
                  )
                )
                
                # ,
                # TODO # Add tabs for other types of pathway enrichment analysis
                # # Functional Class Scoring tab
                # tabPanel(
                #   "FCS"
                #   # (FCS-specific UI components go here)
                # ),
                # 
                # # Topology-based Analysis tab
                # tabPanel(
                #   "TO"
                #   # (TO-specific UI components go here)
                # )
                
              )
            )
            
            ,
            
            # Summary tab
            tabPanel("Summary",
                     box(width = NULL,
                         fluidRow(
                           column(12,htmlOutput("title")),
                         ),
                         fluidRow(
                           column(6, uiOutput("info_ui")),
                           column(6, htmlOutput("cvinfo_ui"))
                         )
                     )
            )
          )
        )
      )
    ),
    fluidRow(
      hidden(
        div(
          id = "statistics_panel",
          fluidPage(
            fluidRow(
              column(12, box(width = NULL, title = "Guide", status = "primary", solidHeader = TRUE,
                             collapsible = TRUE, collapsed = TRUE,
                             strong("Local test"),
                             p("Start by selecting the test type:"),
                             tags$ul(
                               tags$li("2 groups (unpaired): compare the means of two independent or unrelated groups to determine if there is a statistically significant difference between them."),
                               tags$li("2 groups (paired): compare the means of two related groups to see if their average difference is significantly different from zero. Used when the same subjects are tested under two different conditions (e.g., before and after a treatment)."),
                               tags$li("2 groups with time (paired): used to analyze the changes within the same group over different times or conditions, assessing if there is a consistent effect across these points."),
                               tags$li("Compare to reference group: compare the mean of all groups against a reference group. Can be used to determine if the groups significantly differ from the expected performance or baseline.")
                             ),
                             br(),
                             strong("PolySTest"),
                             p("Usage of PolySTest is recommended for data with few replicates and high amounts of missing values."),
                             tagList(
                               list(
                                 tags$li("Select groups (required) and time (optional)."),
                                 tags$li("Options such as 'paired' analysis are available in PolySTest."),
                                 tags$li("To export the entire dataset to PolySTest, go to the Export panel.")
                               )
                             )
              )
              )
            ),
            fluidRow(
              column(6, box(width = NULL,
                            h4("Local test"),
                            fluidRow(
                              column(12,
                                     selectInput("testType", "Select test", width = "100%",
                                                 choices = c("2 groups (unpaired)" = "GroupsUnpaired",
                                                             "2 groups (paired)" = "GroupsPaired",
                                                             "2 groups with time (unpaired)" = "GroupsTimeUnpaired",
                                                             "2 groups with time (paired)" = "GroupsMultipleTime",
                                                             "Compare to reference group" = "CompareToReference"), selected = NULL
                                     ))
                            ),
                            conditionalPanel(
                              condition = "input.testType == 'GroupsUnpaired' || input.testType == 'GroupsPaired'",
                              fluidRow(
                                column(6, selectInput("group1", "Group 1", choices = NULL, width = "100%")),
                                column(6, selectInput("group2", "Group 2", choices = NULL, width = "100%"))
                              )
                            ),
                            conditionalPanel(
                              condition = "input.testType == 'GroupsTimeUnpaired'",
                              fluidRow(
                                column(6, selectInput("group1_time", "Group", choices = NULL, width = "100%")),
                                column(6, selectInput("time1_time", "Time", choices = NULL, width = "100%"))
                              ),
                              fluidRow(
                                column(6, selectInput("group2_time", "Group", choices = NULL, width = "100%")),
                                column(6, selectInput("time2_time", "Time", choices = NULL, width = "100%"))
                              ),
                            ),
                            conditionalPanel(
                              condition = "input.testType == 'CompareToReference'",
                              fluidRow(
                                column(12, selectInput("referenceGroup", "Select reference group", choices = NULL, width = "100%"))
                              )
                            ),
                            conditionalPanel(
                              condition = "input.testType == 'GroupsMultipleTime'",
                              fluidRow(
                                column(12, checkboxGroupInput("contrasts", "Select contrasts", choices = NULL, selected = NULL, inline = FALSE))
                              )
                            ),
                            fluidRow(
                              column(6, actionButton("selectTest", "Run test", width = "100%"))
                            )
              )),
              column(6, box(width = NULL, 
                            h4("Export to PolySTest"),
                            fluidRow(
                              column(6, selectInput("group1_polystest", "Group", choices = NULL, width = "100%")),
                              column(6, selectInput("time1_polystest", "Time", choices = NULL, width = "100%"))
                            ),
                            fluidRow(
                              column(6, selectInput("group2_polystest", "Group", choices = NULL, width = "100%")),
                              column(6, selectInput("time2_polystest", "Time", choices = NULL, width = "100%"))
                            ),
                            fluidRow( 
                              column(6, actionButton("export_polystest", "Send to PolySTest", width = "100%"))
                            )
              ))
            ),
            fluidRow(
              column(12, box(title = "Results", width = NULL,
                             uiOutput("results_ui")
              ))
            )
          )
        )
      )
    ),
    fluidRow(
      hidden(
        div(
          id = "export_panel",
          box(title = ".csv and .xlsx", status = "primary", solidHeader = TRUE, width = 6,
              column(12, 
                     h4(".csv"),
                     uiOutput("export_ui")
              ),
              column(12, style = "margin-top: 20px;",
                     h4(".xlsx"),
                     checkboxGroupInput("export_xml_list", "Choose sheets", choices = NULL, selected = NULL),
                     downloadButton("export_xml", "Export combined .xlsx")
              ),
              column(12, style = "margin-top: 20px;",
                     h4("Statistics results"),
                     uiOutput("export_stats")
              ),
              column(12, style = "margin-top: 20px;",
                     h4("Settings used in app"),
                     uiOutput("export_settings")
              ),
              column(12, style = "margin-top: 20px;",
                     h4("Export to MetaboAnalyst"),
                     downloadButton("downloadLipids", "Download Cleaned Lipid Names")
              )
              
          ),
          box(title = "Export to other apps", status = "primary", solidHeader = TRUE, width = 6,
              column(12, 
                     h4("Statistical testing"),
                     actionButton("send_polystest", "Send to PolySTest"),
                     span(textOutput("connection_polystest"), style="color:#33DD33;")
              ),
              column(12, style = "margin-top: 20px;",
                     h4("Clustering"),
                     actionButton("send_vsclust", "Send to VSClust"),
                     span(textOutput("connection_vsclust"), style="color:#33DD33;")
              ),
              column(12, style = "margin-top: 20px;",
                     h4(".csv for MetaboAnalyst"),
                     uiOutput("export_metabo")
              )
          )
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
)