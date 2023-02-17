manage.hymnologists = tabPanel(
  "Manage hymnologists",
  DTOutput("manage_hymnologists"),
  actionButton("add_hymnologist", label = "Add hymnologist",
               icon = icon("plus")),
  actionButton("save_hymnologists", label = "Save changes")
)