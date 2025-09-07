statisticsPanel <-  fluidRow(
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
  )