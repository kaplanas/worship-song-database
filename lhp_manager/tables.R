manage.hymnologists.info = list(
  table = "lhp.hymnologists",
  columns = data.frame(
    column.name = c("HymnologistID", "LastName", "FirstName", "FileName",
                    "Created", "Updated"),
    editable = c(F, T, T, F, F, F)
  ),
  sort = c("LastName", "FirstName"),
  key = "HymnologistID"
)

manage.hymnologists = tabPanel(
  "Manage hymnologists",
  rHandsontableOutput("hymnologists"),
  actionButton("save_hymnologists", label = "Save changes")
)

tables.page = tabPanel("Manage tables",
                       navlistPanel(
                         manage.hymnologists,
                         well = F,
                         widths = c(3, 9)
                       ))
