#### Selectors ####

# Congregations for file upload
wh.file.congregation.id = list(
  type = "select",
  label = "Choose congregation:",
  width = "500px",
  sql = "SELECT CongregationID, CongregationLabel AS SelectorDisplay
         FROM och.congregation_labels
         WHERE UseData
               OR CURRENT_USER() LIKE 'abby_kaplan%'
         ORDER BY CongregationLabel",
  input.dependencies = c()
)

# Congregations for worship history processing
process.wh.congregation.id = list(
  type = "select",
  label = "Choose congregation:",
  width = "500px",
  sql = "SELECT congregation_labels.CongregationID,
                congregation_labels.CongregationLabel AS SelectorDisplay
         FROM och.congregation_labels
              LEFT JOIN (SELECT DISTINCT CongregationID
                         FROM och.worshiphistory
                         WHERE NOT Processed) not_processed
              ON congregation_labels.CongregationID = not_processed.CongregationID
         WHERE (not_processed.CongregationID IS NOT NULL
                OR {show.all.entered})
               AND (congregation_labels.UseData
                    OR CURRENT_USER() LIKE 'abby_kaplan%')
         ORDER BY congregation_labels.CongregationLabel",
  input.dependencies = c("process.wh.show.all.entered")
)

# Worship history dates
process.wh.date = list(
  type = "date",
  label = "Choose date:",
  sql = "SELECT DISTINCT WorshipDate
         FROM och.worshiphistory
         WHERE CongregationID = {congregation.id}
               AND (NOT Processed OR {show.all.entered})",
  input.dependencies = c("process.wh.congregation.id",
                         "process.wh.show.all.entered")
)

# Congregations for upload summary
year.date.congregation = list(
  type = "select",
  label = "Choose congregation:",
  width = "500px",
  sql = "SELECT CongregationID, CongregationLabel AS SelectorDisplay
         FROM och.congregation_labels
         WHERE UseData
               OR CURRENT_USER() LIKE 'abby_kaplan%'
         ORDER BY CongregationLabel",
  input.dependencies = c()
)

# Years for upload summary
year.date.year = list(
  type = "select",
  label = "Choose year:",
  width = "500px",
  sql = "SELECT YEAR(WorshipDate),
                CONCAT(ProblemYear,
                       CASE WHEN problems.NonSunday = 0
                                 AND problems.Unprocessed = 0
                                 AND problems.Ambiguous = 0
                                 THEN ''
                            ELSE ' ('
                       END,
                       CASE WHEN problems.NonSunday = 0 THEN ''
                            ELSE CONCAT(problems.NonSunday, ' non-Sunday')
                       END,
                       CASE WHEN problems.NonSunday > 0
                                 AND (problems.Unprocessed > 0
                                      OR problems.Ambiguous > 0)
                                 THEN ', '
                            ELSE ''
                       END,
                       CASE WHEN problems.Unprocessed = 0 THEN ''
                            ELSE CONCAT(problems.Unprocessed, ' unprocessed')
                       END,
                       CASE WHEN problems.Unprocessed > 0
                                 AND problems.Ambiguous > 0
                                 THEN ', '
                            ELSE ''
                       END,
                       CASE WHEN problems.Ambiguous = 0 THEN ''
                            ELSE CONCAT(problems.Ambiguous, ' ambiguous')
                       END,
                       CASE WHEN problems.NonSunday = 0
                                 AND problems.Unprocessed = 0
                                 AND problems.Ambiguous = 0
                                 THEN ''
                            ELSE ')'
                       END) AS SelectorDisplay
         FROM och.worshiphistory
              JOIN (SELECT CongregationID, YEAR(WorshipDate) AS ProblemYear,
                           SUM(CASE WHEN DAYOFWEEK(WorshipDate) <> 1 THEN 1
                                    ELSE 0
                               END) AS NonSunday,
                           SUM(CASE WHEN AmbiguousSong <> '' THEN 1
                                    ELSE 0
                               END) AS Ambiguous,
                           SUM(CASE WHEN NOT Processed THEN 1
                                    ELSE 0
                               END) AS Unprocessed
                    FROM och.worshiphistory
                    GROUP BY CongregationID, YEAR(WorshipDate)) problems
              ON worshiphistory.CongregationID = problems.CongregationID
                 AND YEAR(WorshipDate) = ProblemYear
         WHERE worshiphistory.CongregationID = {year.date.congregation}
         GROUP BY YEAR(WorshipDate), ProblemYear
         ORDER BY YEAR(WorshipDate)",
  input.dependencies = c("year.date.congregation")
)

# Worship history time periods
song.count.time = list(
  type = "select",
  label = "Choose year:",
  sql = "SELECT -1 AS Year, 'All' AS SelectorDisplay
         FROM dual
         UNION ALL
         SELECT Year, Year AS SelectorDisplay
         FROM (SELECT DISTINCT YEAR(WorshipDate) AS Year
               FROM och.worshiphistory
               WHERE Processed
               ORDER BY YEAR(WorshipDate)) years",
  input.dependencies = c()
)

#### Combined info ####

# List with everything
selector.info = list(
  wh.file.congregation.id = wh.file.congregation.id,
  process.wh.congregation.id = process.wh.congregation.id,
  process.wh.date = process.wh.date,
  year.date.congregation = year.date.congregation,
  year.date.year = year.date.year,
  song.count.time = song.count.time
)

#### Useful functions ####

# Update the selector list
update.selector.list = function(selector.name, db.con, sql.variables) {
  if(!is.null(db.con)) {
    
    # Get SQL
    sql = selector.info[[selector.name]]$sql
    congregation.id = sql.variables$process.wh.congregation.id
    show.all.entered = sql.variables$process.wh.show.all.entered
    year.date.congregation = sql.variables$year.date.congregation
    sql = glue_sql(sql, .con = db.con)
    sql = gsub("'(true|false)'", "\\1", sql)
    
    # Get list for regular selector
    if(selector.info[[selector.name]]$type == "select") {
      selector.list = list()
      tryCatch(
        {
          selector.list = dbGetQuery(db.con, sql) %>%
            column_to_rownames("SelectorDisplay") %>%
            t() %>%
            as.data.frame() %>%
            as.list()
        },
        error = function(err) {
          print(err)
          selector.list = list()
        }
      )
    }
    
    # Get list for date picker and update
    else if(selector.info[[selector.name]]$type == "date") {
      selector.list = c()
      tryCatch(
        {
          selector.list = ymd(dbGetQuery(db.con, sql)$WorshipDate)
        },
        error = function(err) {
          print(err)
          selector.list = c()
        }
      )
    }
    
    # Return list
    return(selector.list)
    
  }
}