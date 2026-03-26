upload.page = tabPanel(
  "Upload worship history",
  uiOutput("wh.file.congregation.id"),
  selectizeInput("wh.file.type", label = "File type:",
                 choices = c("Spreadsheet", "Bulletin")),
  fileInput("wh.file", label = "", multiple = T, width = "500px",
            accept = c(".xls", ".xlsx", ".csv", ".pdf"),
            buttonLabel = "Choose file..."),
  checkboxInput("wh.file.overwrite",
                label = "Overwrite dates that have already been entered?"),
  actionButton("upload.wh.file", label = "Save file")
)
