return.file.hymnologist.info = list(
  sql = "SELECT CONCAT(CASE WHEN FirstName IS NULL THEN ''
                            ELSE CONCAT(FirstName, ' ')
                       END,
                       CONCAT(LastName,
                              CASE WHEN FileName IS NULL THEN ''
                                   ELSE ' ✓'
                              END)) AS SelectorDisplay,
                       HymnologistID
         FROM lhp.hymnologists
         ORDER BY LastName, FirstName, HymnologistID",
  label = "Choose hymnologist:"
)

process.return.hymnologist.info = list(
  sql = "SELECT CONCAT(CASE WHEN FirstName IS NULL THEN ''
                            ELSE CONCAT(FirstName, ' ')
                       END, LastName) AS SelectorDisplay,
                       HymnologistID
         FROM lhp.hymnologists
         WHERE HymnologistID IN (SELECT DISTINCT HymnologistID
                                 FROM lhp.hymnologist_returns
                                 WHERE NOT Processed)
         ORDER BY LastName, FirstName, HymnologistID",
  label = "Choose hymnologist:"
)

process.return.info = list(
  select.sql = "SELECT hymnologist_returns.HymnologistReturnID,
                       hymnologist_returns.RawSongName, song_labels.SongLabel,
                       hymnologist_returns.Processed
                FROM lhp.hymnologist_returns
                     LEFT JOIN lhp.song_labels
                     ON hymnologist_returns.SongID = song_labels.SongID
                WHERE hymnologist_returns.HymnologistID = {process.return.hymnologist}
                      AND NOT Processed",
  columns = data.frame(
    column.name = c("HymnologistReturnID", "RawSongName", "SongLabel",
                    "Processed"),
    displayed = c(F, T, T, T),
    editable = c(F, F, T, T),
    width = c(NA, 300, 300, 70),
    stringsAsFactors = F
  ),
  update.sql = "UPDATE lhp.hymnologist_returns
                SET SongID = {SongID}, Processed = {Processed}
                WHERE HymnologistReturnID = {HymnologistReturnID}",
  insert.sql = "INSERT INTO lhp.hymnologist_returns
                (HymnologistID, SongID, Processed)
                VALUES
                ({HymnologistID}, {SongID}, {Processed})",
  delete.sql = "DELETE FROM lhp.hymnologist_returns
                WHERE HymnologistID = {input$process.return.hymnologist}
                      AND NOT Processed
                      AND HymnologistReturnID NOT IN ({keys*})"
)

upload.returns = tabPanel(
  "Upload returns",
  selectInput("return.file.hymnologist", "Choose hymnologist:",
              choices = list()),
  fileInput("return.file", label = "", multiple = F,
            accept = c(".csv", ".xls", ".xlsx"),
            buttonLabel = "Choose file..."),
  actionButton("upload.return.file", label = "Save file")
)

process.returns = tabPanel(
  "Process returns",
  actionButton("refresh.process.return.hymnologist",
               label = "Refresh available returns"),
  selectInput("process.return.hymnologist", "Choose hymnologist:",
              choices = list()),
  rHandsontableOutput("process.return"),
  actionButton("save.return", label = "Save changes")
)

returns.page = tabPanel("Upload and process returns",
                        navlistPanel(
                          upload.returns,
                          process.returns,
                          well = F,
                          widths = c(2, 10)
                        ))
