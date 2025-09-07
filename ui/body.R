# Load all panels
rFiles <- list.files("ui/panels", pattern = "\\.R$", full.names = TRUE)
for (file in rFiles) {
  source(file)
}

body <- dashboardBody(
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

  sequencePanel,
  datatablePanel,
  statisticsPanel,
  exportPanel,
  fluidRow(
    div(
      id = "welcome_panel",
      column(12, box(width = NULL, includeHTML("intro_text.html")))
    )
  )
)