hymnologist.labels.info = list(
  sql = "SELECT HymnologistID,
                CONCAT(CASE WHEN FirstName IS NULL THEN ''
                            ELSE CONCAT(FirstName, ' ')
                       END,
                       LastName) AS Label
         FROM lhp.hymnologists
         ORDER BY LastName, FirstName"
)

song.labels.info = list(
  sql = "SELECT SongID, Label
         FROM lhp.song_labels
         ORDER BY REGEXP_REPLACE(Label, '[^A-Za-z0-9 ]', '')"
)