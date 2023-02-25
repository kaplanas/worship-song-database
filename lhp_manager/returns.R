return.file.hymnologist.info = list(
  sql = "SELECT CONCAT(CASE WHEN FirstName IS NULL THEN ''
                            ELSE CONCAT(FirstName, ' ')
                       END, LastName) AS SelectorDisplay,
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
  select_sql = "SELECT hymnologist_returns.HymnologistReturnID,
                       hymnologist_returns.RawSongName, song_labels.Label,
                       hymnologist_returns.Processed
                FROM lhp.hymnologist_returns
                     LEFT JOIN lhp.song_labels
                     ON hymnologist_returns.SongID = song_labels.SongID
                WHERE hymnologist_returns.HymnologistID = {input$process_return_hymnologist}
                      AND NOT Processed",
  displayed.cols = c(2, 3, 4),
  editable.cols = c(3, 4),
  update_sql = "UPDATE lhp.hymnologist_returns
                SET SongID = {SongID}, Processed = {Processed}
                WHERE HymnologistReturnID = {HymnologistReturnID}"
)

song.labels.info = list(
  sql = "SELECT SongID, Label
         FROM lhp.song_labels
         ORDER BY REGEXP_REPLACE(Label, '[^A-Za-z0-9 ]', '')"
)

upload.returns = tabPanel(
  "Upload returns",
  selectInput("return_file_hymnologist", "Choose hymnologist:",
              choices = list()),
  fileInput("return_file", label = "", multiple = F,
            accept = c(".csv", ".xls", ".xlsx"),
            buttonLabel = "Choose file..."),
  actionButton("upload_return_file", label = "Save file")
)

process.returns = tabPanel(
  "Process returns",
  selectInput("process_return_hymnologist", "Choose hymnologist:",
              choices = list()),
  rHandsontableOutput("process_return"),
  actionButton("save_return", label = "Save changes")
)

returns.page = tabPanel("Manage returns",
                        navlistPanel(
                          upload.returns,
                          process.returns,
                          well = F,
                          widths = c(3, 9)
                        ))
