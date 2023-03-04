manage.hymnologists.info = list(
  table = "lhp.hymnologists",
  columns = data.frame(
    column.name = c("HymnologistID", "LastName", "FirstName", "FileName",
                    "Created", "Updated"),
    key.table = rep(NA_character_, 6),
    key.label = rep(NA_character_, 6),
    width = c(100, 100, 100, 200, 100, 100),
    type = c("numeric", "text", "text", "text", "date", "date"),
    editable = c(F, T, T, F, F, F),
    html = rep(F, 6),
    stringsAsFactors = F
  ),
  fixed.cols = 3,
  sort = c("LastName", "FirstName"),
  key = "HymnologistID",
  related.utility.tables = c("hymnologist.labels"),
  related.selectors = c("return.file.hymnologist",
                        "process.return.hymnologist"),
  related.management.tables = c("hymnologist.returns"),
  related.processing.table = F
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
    key.table = c(NA, "hymnologist.labels", NA, "song.labels", NA, NA, NA),
    key.label = c(NA, "HymnologistLabel", NA, "SongLabel", NA, NA, NA),
    width = c(150, 100, 300, 200, 70, 100, 100),
    type = c("numeric", "dropdown", "text", "dropdown", "checkbox", "date",
             "date"),
    editable = c(F, T, F, T, T, F, F),
    html = c(F, F, F, T, F, F, F),
    stringsAsFactors = F
  ),
  fixed.cols = 3,
  sort = c("HymnologistID", "RawSongName"),
  key = "HymnologistReturnID",
  related.utility.tables = c(),
  related.selectors = c(),
  related.management.tables = c(),
  related.processing.table = T
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