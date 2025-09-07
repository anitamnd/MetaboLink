sequencePanel <- fluidRow(
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
  )