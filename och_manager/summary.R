#### Counts by congregation ####

congregation.count.info = "SELECT CongregationLabel,
                                  COUNT(DISTINCT WorshipDate) AS TotalDates,
                                  COUNT(DISTINCT CONCAT(WorshipDate, SongID)) AS TotalSongs,
                                  COUNT(DISTINCT SongID) AS TotalUniqueSongs
                           FROM och.worshiphistory
                                JOIN och.congregation_labels
                                ON worshiphistory.CongregationID = congregation_labels.CongregationID
                           WHERE Processed
                           GROUP BY CongregationLabel
                           ORDER BY CongregationLabel"

congregation.count.table = tabPanel(
  "Table of counts by congregation",
  DTOutput("congregation.counts")
)

#### Counts by song ####

song.count.info = "SELECT SongLabel,
                          COUNT(DISTINCT CONCAT(WorshipDate, CongregationID)) AS TotalSingings,
                          COUNT(DISTINCT CongregationID) AS TotalCongregations
                   FROM och.worshiphistory
                        JOIN wsdb.song_labels
                        ON worshiphistory.SongID = song_labels.SongID
                   WHERE Processed
                   GROUP BY SongLabel
                   ORDER BY COUNT(DISTINCT CongregationID) DESC,
                            COUNT(DISTINCT CONCAT(WorshipDate, CongregationID)) DESC,
                            SongLabel"

song.count.table = tabPanel(
  "Table of counts by song",
  DTOutput("song.counts")
)

#### Combined info ####

summary.table.sql = list(
  congregation.counts = congregation.count.info,
  song.counts = song.count.info
)

#### Viewing page ####

summary.page = tabPanel("Summary",
                        navlistPanel(
                          congregation.count.table,
                          song.count.table,
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