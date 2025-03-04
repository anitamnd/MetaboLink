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
          bsTooltip("Switch between the different uploaded/saved datsets.", placement = "bottom", trigger = "hover")
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
                                   bsTooltip("Load example metabolomics datasets in positive and negative ion mode and respective metadata files. More examples available on GitHub.", placement = "bottom", trigger = "hover")
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
          bsTooltip("Requires columns labeled 'Blank' and 'QC'.", placement = "bottom", trigger = "hover"),
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
                                                         bsTooltip("Remove internal standards.", placement = "bottom", trigger = "hover")
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
                                                column(12, checkboxInput("newFileNorm", "Save as new file", value = F, width = "100%"), style = "padding: 0px; margin-top: -10px; margin-left: 10px; margin-right: -10px;"),
                                                column(6, bsButton("normalize", "Run", width = "100%"), style = "padding-left:0px;"),
                                                column(6, bsButton("saveNormalization", "Save", width = "100%"), style = "padding-left:0px;")
                                              )
                              ) %>% 
                                bsTooltip("Press for more normalization options.", placement = "bottom", trigger = "hover")
                            ))
        ),
        bsCollapsePanel("Log transform and scaling",
                        style = "primary",
                        fluidRow(
                          style = "margin-right: 0px;",
                          column(6, style = "padding-left:0px;",
                                 selectInput("logTransform", "Log transform", choices = c("None", "log2", "log10", "ln"), width = "100%") %>%
                                   bsTooltip("Do not use log transformation for negative values.", placement = "top", trigger = "hover")
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
        ) %>% bsTooltip("Some features will be removed after transforming the data if Inf values are introduced.", placement = "bottom", trigger = "hover"),
        bsCollapsePanel("Merge datasets",
                        style = "primary",
                        fluidRow(selectInput("mergeFile", "Select dataset to merge with", choices = NULL, width = "100%")),
                        fluidRow(
                          style = "margin-right: 0px;",
                          column(6, numericInput("merge_ppm", "M/z tolerance ppm", min = 0, value = 10, width = "100%"), style = "padding-left:0px;"),
                          column(6, numericInput("merge_rt", "RT tolerance", min = 0, value = 0.1, step = 0.01, width = "100%"), style = "padding-left:0px;")
                        ),
                        fluidRow(
                          style = "margin-right: 0px;",
                          column(6, bsButton("editRankings", "Edit priorities", width = "100%"), style = "padding-left:0px;"),
                          column(6, bsButton("mergeDatasets", "Run", width = "100%"), style = "padding-left:0px;")
                        )
        ) %>% bsTooltip("Merge datasets with same samples and different ion mode. The datasets must have the same number of samples.", placement = "bottom", trigger = "hover"),
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
              width = NULL, title = "Upload sequence file", status = "danger",
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
              width = NULL, title = "Add Refmet Information",
              actionButton("addRefmet", "Add", width = "50%")
            ),
            
            box(
              width = NULL, title = "Edit data columns",
              actionButton("editColumns", "Edit", width = "50%")
            ),
            box(
              width = NULL, title = "Group nicknames",
              p("Only use letters and numbers."),
              actionButton("editGroups", "Edit", width = "50%")
            ),
            box(
              width = NULL, title = "Download sequence file",
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
                              box(width = NULL, title = "Median across samples", 
                                  plotlyOutput("histogram") %>% withSpinner(color="steelblue")
                              )),
                       column(12,
                              box(width = NULL, title = "Median across QCs",
                                  uiOutput("histogram_qc") %>% withSpinner(color="steelblue")
                              )),
                       column(12,
                              box(width = NULL, title = "Median across groups",
                                  column(6, selectInput("select_group", "Select group", choices = NULL, width = "100%")),
                                  column(6),
                                  plotlyOutput("histogram_group") %>% withSpinner(color="steelblue")
                              ))
                     )
            ),
            tabPanel("PCA", 
                     #TODO small guide/tooltips
                     fluidRow(
                       column(12, box(width = NULL, title = "Principal Component Analysis",
                                      tagList(
                                        list(
                                          tags$li("Check log-transfomed checkbox if data is already log-transformed.")
                                        )
                                      )
                       ))
                     ),
                     fluidRow(
                       column(6, box(width = NULL,
                                     selectInput("selectpca1", "", choices = NULL, width = "100%"),
                                     checkboxInput("pca1_islog", "Data is log-transformed.", value = FALSE, width = "100%"),
                                     actionButton("run_pca1", "Run PCA", width = "50%") %>%
                                       bsTooltip("Check box if the data is log-transformed!", placement = "bottom", trigger = "hover"),
                                     plotlyOutput("plotpca1", width = "100%"), br(),
                                     htmlOutput("pca1Details")
                       )),
                       column(6, box(width = NULL,
                                     selectInput("selectpca2", "", choices = NULL, width = "100%"),
                                     checkboxInput("pca2_islog", "Data is log-transformed.", value = FALSE, width = "100%"),
                                     actionButton("run_pca2", "Run PCA", width = "50%"),
                                     plotlyOutput("plotpca2"), br(),
                                     htmlOutput("pca2Details")
                       )),
                       #TODO boxplots (see normalization)
                       column(6, box(width = NULL,
                                     plotlyOutput("plotscree1", width = "100%")
                       )),
                       column(6, box(width = NULL,
                                     plotlyOutput("plotscree2", width = "100%")
                       ))
                     ),
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
                         title = "Select feature", 
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
                       tabPanel("K-means",
                                fluidRow(
                                  column(12, box(width = NULL, title = "K-means Analysis",
                                                 tagList(
                                                   list(
                                                     tags$li("Add information regarding K-means analysis here.")))))),
                                # Make a dropdown to select the PCA results from the reactive values 
                                selectInput("kmeans_pca", "Choose PCA Results:", choices = NULL),
                                # With the selected PCA results, make a drowdown to select the evaluation method
                                selectInput("kmeans_eval_method", "Choose Evaluation Method:",  # Dropdown to select evaluation method for K-means
                                            choices = c("Within Sum of Square (WSS)" = "wss",
                                                        "Silhouette" = "silhouette",
                                                        "Gap Statistic" = "gap_stat")),
                                actionButton("compute_kmeans_eval", "Compute Evaluation"),      # Button to compute K-means evaluation
                                plotlyOutput("kmeans_eval_plot"),                                # Output for K-means evaluation plot
                                numericInput("num_clusters", "Number of Clusters (k):", value = 3, min = 1, step = 1),  # Input for number of clusters
                                numericInput("percentile_threshold", "Percentile Threshold:", value = 95, min = 0, max = 100, step = 1), # Input for percentile threshold
                                actionButton("run_kmeans", "Run K-means"),                      # Button to run K-means
                                plotlyOutput("kmeans_plot"),                                    # Output for K-means plot
                                DTOutput("kmeans_outliers")                                    # Output for K-means outliers table
                       ),
                       tabPanel("Hierarchical",
                                fluidRow(
                                  column(12, box(width = NULL, title = "Hierarchical Clustering Analysis",
                                                 tagList(
                                                   list(
                                                     tags$li("Add information regarding Hierarchical analysis here.")))))),
                                # Make a dropdown to select the PCA results from the reactive values 
                                selectInput("hierarchical_pca", "Choose PCA Results:", choices = NULL),
                                selectInput("clustering_method", "Select Clustering Method:",  # Dropdown to select clustering method
                                            choices = c("Single" = "single",
                                                        "Complete" = "complete",
                                                        "Average" = "average",
                                                        "Ward's D2" = "ward.D2")),
                                numericInput("num_clusters_hierarchical", "Number of Clusters (k):", value = 3, min = 1),  # Input for number of clusters
                                numericInput("threshold", "Dendrogram Threshold (Distance):", value = 5, min = 0),  # Input for dendrogram threshold
                                actionButton("run_hierarchical", "Run Hierarchical Clustering"),  # Button to run hierarchical clustering
                                plotlyOutput("hclust_plot"),                                      # Output for hierarchical clustering plot
                                plotlyOutput("conf_matrix_plot"),                                 # Output for confusion matrix plot
                                plotlyOutput("dendrogram_plot"),                                  # Output for dendrogram plot
                                DTOutput("hierarchical_outliers")                                # Output for hierarchical outliers table
                       ),
                       tabPanel("DBSCAN",
                                fluidRow(
                                  column(12, box(width = NULL, title = "Density-Based Spatial Clustering of Applications with Noise Analysis",
                                                 tagList(
                                                   list(
                                                     tags$li("Add information regarding DBSCAN analysis here.")))))),
                                # Make a dropdown to select the PCA results from the reactive values 
                                selectInput("dbscan_pca", "Choose PCA Results:", choices = NULL),
                                numericInput("knn", "Choose k for kNN Distance Plot:", value = 5, min = 1, step = 1),  # Input for k in kNN
                                actionButton("compute_knn", "Compute kNN Distance Plot"),                   # Button to compute kNN distance plot
                                plotlyOutput("knn_plot"),                                                    # Output for kNN plot
                                numericInput("eps", "Choose epsilon for DBSCAN:", value = 3, min = 0.01, step = 0.1),  # Input for epsilon in DBSCAN
                                numericInput("min_pts_dbscan", "Choose minPts for DBSCAN:", value = 5, min = 1),  # Input for minPts in DBSCAN
                                actionButton("run_dbscan", "Run DBSCAN"),                                   # Button to run DBSCAN
                                plotlyOutput("dbscan_plot"),                                                # Output for DBSCAN plot
                                DTOutput("dbscan_outliers")                                                # Output for DBSCAN outliers table
                       ),
                       tabPanel("HDBSCAN",
                                fluidRow(
                                  column(12, box(width = NULL, title = "Hierarchical Density-Based Spatial Clustering of Applications with Noise (HDBSCAN)",
                                                 tagList(
                                                   list(
                                                     tags$li("Add information regarding HDBSCAN analysis here.")))))),
                                # Make a dropdown to select the PCA results from the reactive values 
                                selectInput("hdbscan_pca", "Choose PCA Results:", choices = NULL),
                                numericInput("min_pts_hdbscan", "Choose minPts for HDBSCAN:", value = 5, min = 1),  # Input for minPts in HDBSCAN
                                numericInput("threshold_hdbscan", "Outlier Threshold for HDBSCAN:", value = 0.85, min = 0.01, max = 1),  # Input for outlier threshold in HDBSCAN
                                actionButton("run_hdbscan", "Run HDBSCAN"),                                          # Button to run HDBSCAN
                                plotlyOutput("hdbscan_plot"),                                                        # Output for HDBSCAN plot
                                DTOutput("hdbscan_outliers")                                                        # Output for HDBSCAN outliers table
                       ),
                       tabPanel("OPTICS",
                                fluidRow(
                                  column(12, box(width = NULL, title = "Ordering Points To Identify the Clustering Structure",
                                                 tagList(
                                                   list(
                                                     tags$li("Add information regarding OPTICS analysis here.")))))),
                                # Make a dropdown to select the PCA results from the reactive values 
                                selectInput("optics_pca", "Choose PCA Results:", choices = NULL),
                                numericInput("min_pts_optics", "Choose minPts for OPTICS:", value = 5, min = 1),  # Input for minPts in OPTICS
                                numericInput("eps_optics", "Choose eps for OPTICS (optional):", value = NA, min = 0.1, step = 0.1),  # Input for eps in OPTICS
                                numericInput("eps_cl_optics", "Choose cutoff (eps_cl) for OPTICS:", value = 0.5, min = 0.1, step = 0.1),  # Input for eps_cl in OPTICS
                                # actionButton("run_optics", "Run OPTICS"),                                     # Button to run OPTICS
                                plotOutput("optics_reachability_plot"),                                      # Output for OPTICS reachability plot
                                plotOutput("reachability_plot_threshold"),                                   # Output for reachability plot threshold
                                plotlyOutput("cluster_plot"),                                                # Output for cluster plot
                                DTOutput("optics_outliers")                                                 # Output for OPTICS outliers table
                       ),
                       tabPanel("LOF",
                                fluidRow(
                                  column(12, box(width = NULL, title = "Local Outlier Factor (LOF) Analysis",
                                                 tagList(
                                                   list(
                                                     tags$li("Add information regarding LOF analysis here.")))))),
                                # Make a dropdown to select the PCA results from the reactive values 
                                selectInput("lof_pca", "Choose PCA Results:", choices = NULL),
                                numericInput("lof_threshold", "Threshold for LOF:", value = 1.5, min = 0, step = 0.1),  # Input for threshold in LOF
                                numericInput("lof_k", "k for LOF:", value = 4, min = 1),                                 # Input for k in LOF
                                actionButton("run_lof", "Run LOF"),                                                    # Button to run LOF
                                plotlyOutput("lof_plot"),                                                             # Output for LOF plot
                                plotlyOutput("lof_od_plot"),                                                          # Output for LOF outlier detection plot
                                DTOutput("lof_outliers")                                                             # Output for LOF outliers table
                       )
                     )),
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
                                   tags$li("Heatmaps help visualize high-dimensional data and clustering of features."),
                                   tags$li("Select a dataset and, optionally, specific groups to compare."),
                                   tags$li("Customize display options to enhance the interpretation of patterns.")
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
                                 inputId = "select_groups",
                                 label = "Select Specific Groups: ",
                                 value = FALSE
                               ),
                               uiOutput("group_selection_ui"),
                               numericInput(
                                 inputId = "top_x",
                                 label = "Number of Top Features:",
                                 value = 20,
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
                                           choices = c("euclidean", "maximum", "manhattan", "canberra",
                                                       "binary", "minkowski","pearson","spearman","kendall"),
                                           selected = "euclidean"),
                               selectInput("clustering_method_rows", "Clustering Method (Rows):",
                                           choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
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
                           column(
                             width = 12,
                             box(
                               width = NULL,
                               title = "Heatmap Plot",
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               
                               plotOutput("heatmap_plot", height = "600px") %>% withSpinner(color = "steelblue"),
                             )
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
                       tabPanel("Wierd circular barplot",
                                fluidRow(
                                  column(12, box(width = NULL, title = "Wierd circular barplot",
                                                 tagList(
                                                   list(
                                                     tags$li("Add information regarding Wierd circular barplot analysis here.")
                                                   )
                                                 )
                                  ))
                                ),
                                fluidRow(
                                  column(12, box(width = NULL, title = "Wierd circular barplot",
                                                 selectInput(
                                                   inputId = "select_data_circular_barplot",
                                                   label = "Select Dataset for circular barplot",
                                                   choices = NULL,  # To be populated dynamically
                                                   width = "100%"
                                                 ),
                                                 fluidRow(
                                                   column(
                                                     width = 6,
                                                     selectInput("name_column_cirbar", "Select nameing column", choices = NULL, width = "100%")
                                                   ),
                                                   column(
                                                     width = 6,
                                                     selectInput("group_column_cirbar", "Select grouping column", choices = NULL, width = "100%")
                                                   ),
                                                   column(
                                                     width = 6,
                                                     numericInput("top_x_cirbar", "Select top features:", value = 100, min = 1, step = 1, width = "100%")
                                                   ),
                                                   column(
                                                     width = 6,
                                                     selectInput("feature_cirbar", "Select plotting feature:", choices = NULL, width = "100%")
                                                   ),
                                                   column(
                                                     width = 6,
                                                     selectInput("group1_cirbar", "Numerator (group):", choices = NULL)
                                                   ),
                                                   column(
                                                     width = 6,
                                                     selectInput("group2_cirbar", "Denominator (group):", choices = NULL)
                                                   )
                                                 ),
                                                 # Added action button to run the plot
                                                 actionButton(
                                                   inputId = "run_circular_barplot",
                                                   label = "Run Plot",
                                                   class = "btn-primary"
                                                 ),
                                                 plotOutput("circular_barplot", width = "800px", height = "800px") %>% withSpinner(color="steelblue")
                                  ))
                                )
                       ),
                       
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
                                                 tags$li("After data processing select the numerator and denominator groups."),
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
                                               title = "Numerator Group Table",
                                               width = NULL,
                                               DT::dataTableOutput("numerator_group_table")  
                                             )),
                                             column(6, box(
                                               title = "Denominator Group Table",
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
                                            uiOutput("selected_groups_text"),
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
                                   tags$li("Volcano plots help visualize differential expression data (e.g., metabolomics, proteomics)."),
                                   tags$li("Select two groups to compare, typically an experimental condition vs. a control."),
                                   tags$li("You can set thresholds for log2 fold change and p-value to highlight significantly changed features.")
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
                                   selectInput("group1_vol", "Numerator (group):", choices = NULL)
                                 ),
                                 column(
                                   width = 6,
                                   selectInput("group2_vol", "Denominator (group):", choices = NULL)
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
                               checkboxInput("show_legend", "Show Legend:", TRUE),
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
                               
                               plotlyOutput("volcano_plot", height = "500px") %>% withSpinner(color="steelblue"),
                               br(),
                               DTOutput("volcano_table")
                             )
                           )
                         )
                       )
                     )
            ),
            # UI for Pathway Enrichment Analysis
            tabPanel(
              "Pathway Enrichment",
              fluidRow(
                column(
                  12,
                  box(
                    width = NULL, title = "Pathway Enrichment Analysis",
                    tagList(
                      list(
                        tags$li("Select a dataset and the column containing compound identifiers. Click 'Run Gather Identifiers' to first update InChI from cached or online sources, then gather chemical identifiers for enrichment analysis."),
                        tags$li("Once identifiers are gathered, click 'Run Enrichment Analysis' to perform the pathway enrichment.")
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
                    selectInput("select_data_for_enrichment", "Select Dataset for Identifier gathering", choices = NULL, width = "100%"),
                    
                    fluidRow(
                      column(
                        width = 6,
                        selectInput("identifier_column", "Select Identifier Column", choices = NULL, width = "100%")
                      ),
                      column(
                        width = 6,
                        selectInput("compound_column", "Select Compound Column", choices = NULL, width = "100%")
                      )
                    ),
                    
                    actionButton("run_gather_identifiers", "Gather Identifiers", width = "50%"),
                    
                    checkboxInput("showDT", "Display Data Table", value = FALSE),
                    
                    # Only show search bar and DT if checkbox is checked
                    conditionalPanel(
                      condition = "input.showDT == true",
                      DTOutput("dt_table_path")
                    ),
                    
                    checkboxInput("gene_selected", "Run Gene Enrichment", TRUE),
                    checkboxInput("module_selected", "Run Module Enrichment", FALSE),
                    # select group for enrichment analysis 
                    selectInput("group_enrichment", "Select Group for Enrichment Analysis", choices = NULL, width = "100%"),
                    numericInput(
                      inputId = "top_x_enrich",
                      label = "Number of Top Features:",
                      value = 20,
                      min = 1,
                      step = 1
                    ),
                    actionButton("run_enrichment_analysis", "Enrichment Analysis", width = "50%")
                  )
                ),
                column(
                  6,
                  box(
                    width = NULL,
                    h4("Number of Identifiers Gathered"),
                    textOutput("identifier_count_text"),
                    uiOutput("identifier_count_table")
                  )
                ),
                column(
                  12,
                  box(
                    title = "Enrichment Barplot and Dotplot",
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("enrichment_barplot") %>% withSpinner(color="steelblue"),
                    plotOutput("enrichment_dotplot") %>% withSpinner(color="steelblue")
                  )
                ),
                column(
                  12,
                  box(
                    title = "Enrichment Cnetplot",  # Adds a title to the box
                    width = NULL,
                    status = "primary",             # Sets a color style (requires shinydashboard)
                    solidHeader = TRUE,             # Gives the header a solid background
                    collapsible = TRUE,             # Allows the user to collapse the box if needed
                    plotOutput("enrichment_cnetplot", height = "500px") %>% withSpinner(color="steelblue"  # Sets a fixed height
                    )
                  )), 
                fluidRow(
                  # Left column with radio buttons (width = 3)
                  column(
                    12,
                    box(
                      title = "Enrichment Class Plot",  # Title for the class plot section
                      width = NULL,
                      status = "primary",               # Sets a primary color style (requires shinydashboard)
                      solidHeader = TRUE,               # Gives the header a solid background
                      collapsible = TRUE,               # Allows the user to collapse the box if needed
                      fluidRow(
                        # Left column for the radio buttons (width = 3)
                        column(
                          width = 3,
                          wellPanel(
                            h4("Select which class plot to display:"),
                            radioButtons(
                              inputId  = "classChoice",
                              label    = NULL,  # Using the h4 above for a title
                              choices  = c("Super Class" = "super",
                                           "Main Class"  = "main",
                                           "Sub Class"   = "sub"),
                              selected = "super"  # Default selection
                            )
                          )
                        ),
                        # Right column for the plots (width = 9)
                        column(
                          width = 9,
                          conditionalPanel(
                            condition = "input.classChoice == 'super'",
                            plotlyOutput("super_class_plot") %>% withSpinner(color = "steelblue")
                          ),
                          conditionalPanel(
                            condition = "input.classChoice == 'main'",
                            plotlyOutput("main_class_plot") %>% withSpinner(color = "steelblue")
                          ),
                          conditionalPanel(
                            condition = "input.classChoice == 'sub'",
                            plotlyOutput("sub_class_plot") %>% withSpinner(color = "steelblue")
                          )
                        )
                      )
                    )
                  )
                )
                ,
                column(
                  12,
                  box(
                    width = NULL,
                    DTOutput("identifier_table")
                  )
                )
              )
            ),
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