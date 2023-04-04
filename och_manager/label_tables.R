#### Label tables ####

congregation.labels.sql = "SELECT CongregationID, CongregationLabel
                           FROM och.congregation_labels"

song.instance.labels.sql = "SELECT SongInstanceID, SongID, SongInstanceLabel
                            FROM och.songinstance_labels"

song.labels.sql = "SELECT SongID, SongLabel
                   FROM wsdb.song_labels"

ambiguous.song.labels.sql = "SELECT SongName
                             FROM wsdb.songs
                             GROUP BY SongName
                             HAVING COUNT(*) > 1"

#### Combined info ####

label.table.sql = list(
  congregation.labels = congregation.labels.sql,
  song.instance.labels = song.instance.labels.sql,
  song.labels = song.labels.sql,
  ambiguous.song.labels = ambiguous.song.labels.sql
)

#### Useful functions ####

# Populate the table
populate.label.table = function(label.table, db.con) {
  if(!is.null(db.con)) {
    label.sql = label.table.sql[[label.table]]
    tryCatch(
      {
        return(dbGetQuery(db.con, label.sql))
      },
      error = function(err) {
        print(err)
        return(NULL)
      }
    )
  } else {
    return(NULL)
  }
}
