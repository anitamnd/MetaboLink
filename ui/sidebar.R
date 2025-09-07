sidebar <- dashboardSidebar(
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
)