#### Counts by songbook ####

songbook.count.info = "SELECT SongbookName, SongbookVolume,
                              COUNT(*) AS TotalSongs
                       FROM wsdb.songbookentries
                            JOIN wsdb.songbooks
                            ON songbookentries.SongbookID = songbooks.SongbookID
                            LEFT JOIN wsdb.songbookvolumes
                            ON songbookentries.SongbookVolumeID = songbookvolumes.SongbookVolumeID
                       GROUP BY SongbookName, SongbookVolume
                       ORDER BY SongbookName, SongbookVolume"

songbook.count.table = tabPanel(
  "Table of song counts",
  DTOutput("songbook.counts")
)

#### Combined info ####

summary.table.sql = list(
  songbook.counts = songbook.count.info
)

#### Viewing page ####

summary.page = tabPanel("Summary",
                        navlistPanel(
                          songbook.count.table,
                          well = F,
                          widths = c(2, 10)
                        ))

#### Useful functions ####

# Populate the table
populate.summary.table = function(summary.table, db.con) {
  if(!is.null(db.con)) {
    tryCatch(
      {
        return(dbGetQuery(db.con, summary.table.sql[[summary.table]]))
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