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

#### Combined info ####

summary.table.sql = list(
  congregation.counts = congregation.count.info
)

#### Viewing page ####

summary.page = tabPanel("Summary",
                        navlistPanel(
                          congregation.count.table,
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