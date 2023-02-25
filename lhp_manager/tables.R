manage.hymnologists.info = list(
  table = "lhp.hymnologists",
  columns = data.frame(
    column.name = c("HymnologistID", "LastName", "FirstName", "FileName",
                    "Created", "Updated"),
    editable = c(F, T, T, F, F, F)
  ),
  sort = list(list(1, "asc"), list(2, "asc")),
  key = "HymnologistID"
)

manage.hymnologists = tabPanel(
  "Manage hymnologists",
  DTOutput("manage_hymnologists"),
  actionButton("add_hymnologists", label = "Add hymnologist",
               icon = icon("plus")),
  actionButton("delete_hymnologists", label = "Delete hymnologist"),
  actionButton("save_hymnologists", label = "Save changes")
)

tables.page = tabPanel("Manage tables",
                       navlistPanel(
                         manage.hymnologists,
                         well = F,
                         widths = c(3, 9)
                       ))
