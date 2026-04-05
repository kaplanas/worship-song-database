upload.page = tabPanel(
  "Upload worship history",
  tags$div(tags$p(tags$b("Both file types have strict formatting requirements and will fail if they aren't exactly right.  See the help tab for more info."))),
  selectizeInput("wh.file.type", label = "File type:",
                 choices = c("Spreadsheet", "Bulletin")),
  fileInput("wh.file", label = "", multiple = T, width = "500px",
            accept = c(".xls", ".xlsx", ".csv", ".pdf"),
            buttonLabel = "Choose file..."),
  checkboxInput("wh.file.overwrite",
                label = "Overwrite dates that have already been entered?"),
  actionButton("upload.wh.file", label = "Save file")
)
