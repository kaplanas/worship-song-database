#### Counts by congregation ####

congregation.count.info = list(
  type = "table",
  sql = "SELECT CongregationLabel,
                COUNT(DISTINCT WorshipDate) AS TotalDates,
                MIN(WorshipDate) AS FirstDate,
                MAX(WorshipDate) AS LastDate,
                COUNT(DISTINCT CONCAT(WorshipDate, SongID)) AS TotalSongs,
                COUNT(DISTINCT SongID) AS TotalUniqueSongs
         FROM och.worshiphistory
              JOIN och.congregation_labels
              ON worshiphistory.CongregationID = congregation_labels.CongregationID
         WHERE Processed
         GROUP BY CongregationLabel
         ORDER BY CongregationLabel",
  hidden.columns = c()
)

congregation.count.table = tabPanel(
  "Table of counts by congregation",
  DTOutput("congregation.counts")
)

#### Counts by song ####

song.count.info = list(
  type = "table",
  sql = "SELECT SongLabel,
                COUNT(DISTINCT CONCAT(WorshipDate, CongregationID)) AS TotalSingings,
                COUNT(DISTINCT CongregationID) AS TotalCongregations,
                CASE WHEN restoration_songs.SongID IS NULL THEN 'N'
                     ELSE 'Y'
                END AS Restoration
         FROM och.worshiphistory
              JOIN wsdb.song_labels
              ON worshiphistory.SongID = song_labels.SongID
              LEFT JOIN och.restoration_songs
              ON worshiphistory.SongID = restoration_songs.SongID
         WHERE Processed
         GROUP BY SongLabel,
                  CASE WHEN restoration_songs.SongID IS NULL THEN 'N'
                       ELSE 'Y'
                  END
         ORDER BY COUNT(DISTINCT CongregationID) DESC,
                  COUNT(DISTINCT CONCAT(WorshipDate, CongregationID)) DESC,
                  SongLabel",
  hidden.columns = c(3)
)

song.count.table = tabPanel(
  "Table of counts by song",
  DTOutput("song.counts")
)

#### Counts by date ####

date.count.info = list(
  type = "graph",
  sql = "SELECT WorshipDate,
                COUNT(DISTINCT CongregationID) AS TotalCongregations,
                COUNT(DISTINCT CONCAT(CongregationID, SongID)) AS TotalSongs
         FROM och.worshiphistory
         WHERE Processed
         GROUP BY WorshipDate",
  hidden.columns = c()
)

date.count.graph = tabPanel(
  "Graph of counts by date",
  plotOutput("date.counts", height = "1000px")
)

#### Map ####

song.count.map.info = list(
  type = "graph",
  sql = "SELECT worshiphistory.CongregationID, Latitude, Longitude,
                COUNT(DISTINCT CONCAT(WorshipDate)) AS TotalDates
         FROM och.worshiphistory
              JOIN och.congregations
              ON worshiphistory.CongregationID = congregations.CongregationID
         WHERE Processed
         GROUP BY worshiphistory.CongregationID",
  hidden.columns = c()
)

song.count.map = tabPanel(
  "Map of counts by congregation",
  plotOutput("song.counts.map", height = "600px")
)

#### Combined info ####

summary.table.info = list(
  congregation.counts = congregation.count.info,
  song.counts = song.count.info,
  date.counts = date.count.info,
  song.counts.map = song.count.map.info
)

#### Viewing page ####

summary.page = tabPanel("Summary",
                        navlistPanel(
                          congregation.count.table,
                          date.count.graph,
                          song.count.table,
                          song.count.map,
                          well = F,
                          widths = c(2, 10)
                        ))

#### Useful functions ####

# Populate the table
populate.summary.table = function(summary.table, db.con) {
  if(!is.null(db.con)) {
    tryCatch(
      {
        return(dbGetQuery(db.con, summary.table.info[[summary.table]]$sql))
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