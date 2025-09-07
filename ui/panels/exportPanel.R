exportPanel <- fluidRow(
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
  )