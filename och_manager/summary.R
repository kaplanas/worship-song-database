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
               AND (congregation_labels.UseData
                    OR CURRENT_USER() LIKE 'abby_kaplan%')
         GROUP BY CongregationLabel
         ORDER BY CongregationLabel",
  hidden.columns = c(),
  input.dependencies = c()
)

congregation.count.table = tabPanel(
  "Table of counts by congregation",
  DTOutput("congregation.counts")
)

#### Counts by congregation and year ####

congregation.year.count.info = list(
  type = "table",
  sql = "SELECT CongregationLabel, YEAR(WorshipDate) AS Year,
                CONCAT(COUNT(DISTINCT CASE WHEN Processed THEN WorshipDate
                                           ELSE NULL
                                      END),
                       ' / ',
                       COUNT(DISTINCT WorshipDate)) AS TotalDatesUploadedProcessed
         FROM och.worshiphistory
              JOIN och.congregation_labels
              ON worshiphistory.CongregationID = congregation_labels.CongregationID
         WHERE congregation_labels.UseData
               OR CURRENT_USER() LIKE 'abby_kaplan%'
         GROUP BY CongregationLabel, YEAR(WorshipDate)",
  hidden.columns = c(),
  input.dependencies = c()
)

congregation.year.count.table = tabPanel(
  "Table of counts by congregation and year",
  DTOutput("congregation.year.counts")
)

#### Map ####

congregation.count.map.info = list(
  type = "graph",
  sql = "SELECT worshiphistory.CongregationID, Latitude, Longitude,
                COUNT(DISTINCT CONCAT(WorshipDate)) AS TotalDates
         FROM och.worshiphistory
              JOIN och.congregations
              ON worshiphistory.CongregationID = congregations.CongregationID
         WHERE Processed
               AND (congregations.UseData
                    OR CURRENT_USER() LIKE 'abby_kaplan%')
         GROUP BY worshiphistory.CongregationID",
  hidden.columns = c(),
  input.dependencies = c()
)

congregation.count.map = tabPanel(
  "Map of counts by congregation",
  plotOutput("congregation.counts.map", height = "600px")
)

#### Counts by date ####

date.count.info = list(
  type = "graph",
  sql = "SELECT WorshipDate,
                COUNT(DISTINCT worshiphistory.CongregationID) AS TotalCongregations,
                COUNT(DISTINCT CONCAT(worshiphistory.CongregationID, SongID)) AS TotalSongs
         FROM och.worshiphistory
              JOIN och.congregations
              ON worshiphistory.CongregationID = congregations.CongregationID
         WHERE Processed
               AND (congregations.UseData
                    OR CURRENT_USER() LIKE 'abby_kaplan%')
         GROUP BY WorshipDate",
  hidden.columns = c(),
  input.dependencies = c()
)

date.count.graph = tabPanel(
  "Graph of counts by date",
  plotOutput("date.counts", height = "1000px")
)

#### Counts by song ####

song.count.info = list(
  type = "table",
  sql = "SELECT SongLabel,
                COUNT(DISTINCT CONCAT(WorshipDate, worshiphistory.CongregationID)) AS TotalSingings,
                COUNT(DISTINCT worshiphistory.CongregationID) AS TotalCongregations,
                CASE WHEN restoration_songs.SongID IS NULL THEN 'N'
                     ELSE 'Y'
                END AS Restoration
         FROM och.worshiphistory
              JOIN wsdb.song_labels
              ON worshiphistory.SongID = song_labels.SongID
              LEFT JOIN och.restoration_songs
              ON worshiphistory.SongID = restoration_songs.SongID
              JOIN och.congregations
              ON worshiphistory.CongregationID = congregations.CongregationID
         WHERE Processed
               AND (YEAR(WorshipDate) = {song.count.time}
                    OR {song.count.time} = '-1')
               AND (congregations.UseData
                    OR CURRENT_USER() LIKE 'abby_kaplan%')
         GROUP BY SongLabel,
                  CASE WHEN restoration_songs.SongID IS NULL THEN 'N'
                       ELSE 'Y'
                  END
         ORDER BY COUNT(DISTINCT worshiphistory.CongregationID) DESC,
                  COUNT(DISTINCT CONCAT(WorshipDate, worshiphistory.CongregationID)) DESC,
                  SongLabel",
  hidden.columns = c(3),
  input.dependencies = c("song.count.time")
)

song.count.table = tabPanel(
  "Table of counts by song",
  uiOutput("song.count.time"),
  DTOutput("song.counts")
)

#### Combined info ####

summary.table.info = list(
  congregation.counts = congregation.count.info,
  congregation.year.counts = congregation.year.count.info,
  congregation.counts.map = congregation.count.map.info,
  song.counts = song.count.info,
  date.counts = date.count.info
)

#### Viewing page ####

summary.page = tabPanel("Summary",
                        navlistPanel(
                          congregation.count.table,
                          congregation.year.count.table,
                          congregation.count.map,
                          date.count.graph,
                          song.count.table,
                          well = F,
                          widths = c(2, 10)
                        ))

#### Useful functions ####

# Populate the table
populate.summary.table = function(summary.table, db.con, sql.variables) {
  if(!is.null(db.con)) {
    tryCatch(
      {
        song.count.time = sql.variables$song.count.time
        sql = glue_sql(summary.table.info[[summary.table]]$sql, .con = db.con)
        df = dbGetQuery(db.con, sql)
        if(summary.table == "congregation.year.counts") {
          df = df %>%
            pivot_wider(names_from = "Year", names_sort = T,
                        values_from = "TotalDatesUploadedProcessed")
        }
        return(df)
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