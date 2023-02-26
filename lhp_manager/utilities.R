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
  sql = "SELECT NULL AS SongID, '' AS Label
         FROM dual
         UNION ALL
         SELECT SongID, Label
         FROM lhp.song_labels
         ORDER BY Label"
)