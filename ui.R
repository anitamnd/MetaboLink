source('ui/sidebar.R')
source('ui/body.R')

dashboardPage(
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
  sidebar,
  body
)