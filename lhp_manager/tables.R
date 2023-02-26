manage.hymnologists.info = list(
  table = "lhp.hymnologists",
  columns = data.frame(
    column.name = c("HymnologistID", "LastName", "FirstName", "FileName",
                    "Created", "Updated"),
    editable = c(F, T, T, F, F, F),
    key.table = c(NA, NA, NA, NA, NA, NA)
  ),
  sort = c("LastName", "FirstName"),
  key = "HymnologistID"
)

manage.hymnologists = tabPanel(
  "Manage hymnologists",
  rHandsontableOutput("hymnologists"),
  actionButton("save.hymnologists", label = "Save changes")
)

manage.hymnologist.returns.info = list(
  table = "lhp.hymnologist_returns",
  columns = data.frame(
    column.name = c("HymnologistReturnID", "HymnologistID", "RawSongName",
                    "SongID", "Processed", "Created", "Updated"),
    editable = c(F, T, F, T, T, F, F),
    key.table = c(NA, "hymnologist.labels", NA, "song.labels", NA, NA, NA)
  ),
  sort = c("HymnologistID", "RawSongName"),
  key = "HymnologistReturnID"
)

manage.hymnologist.returns = tabPanel(
  "Manage returns",
  rHandsontableOutput("hymnologist.returns"),
  actionButton("save.hymnologist.returns", label = "Save changes")
)

tables.page = tabPanel("Manage tables",
                       navlistPanel(
                         manage.hymnologists,
                         manage.hymnologist.returns,
                         well = F,
                         widths = c(2, 10)
                       ))
