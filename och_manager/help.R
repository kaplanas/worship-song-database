overview.panel = tabPanel(
  "Overview",
  tags$div(class = "helpPanel", includeHTML("help/overview.html"))
)

participate.panel = tabPanel(
  "How to participate",
  tags$div(class = "helpPanel", includeHTML("help/participate.html"))
)

profile.panel = tabPanel(
  "Congregation profile",
  tags$div(class = "helpPanel", includeHTML("help/profile.html"))
)

upload.panel = tabPanel(
  "Uploading data",
  tags$div(class = "helpPanel", includeHTML("help/upload.html"))
)

enter.panel = tabPanel(
  "Entering data",
  tags$div(class = "helpPanel", includeHTML("help/enter.html"))
)

report.panel = tabPanel(
  "Summaries and reports",
  tags$div(class = "helpPanel", includeHTML("help/reports.html"))
)

help.page = tabPanel("Help",
                     navlistPanel(
                       overview.panel,
                       participate.panel,
                       profile.panel,
                       upload.panel,
                       enter.panel,
                       report.panel,
                       well = F,
                       widths = c(2, 10)
                     ))
