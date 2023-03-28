#### Selectors ####

# Congregations
process.wh.congregation.id = list(
  type = "select",
  label = "Choose congregation:",
  sql = "SELECT congregation_labels.CongregationID,
                congregation_labels.CongregationLabel AS SelectorDisplay
         FROM och.congregation_labels
              LEFT JOIN (SELECT DISTINCT CongregationID
                         FROM och.worshiphistory
                         WHERE NOT Processed) not_processed
              ON congregation_labels.CongregationID = not_processed.CongregationID
         WHERE not_processed.CongregationID IS NOT NULL
               OR {show.all.entered}
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

#### Combined info ####

# List with everything
selector.info = list(
  # wh.file.congregation.id = selector.congregations,
  process.wh.congregation.id = process.wh.congregation.id,
  process.wh.date = process.wh.date
)

#### Useful functions ####

# Update the selector list
update.selector.list = function(selector.name, db.con, sql.variables) {
  if(!is.null(db.con)) {
    
    # Get SQL
    sql = selector.info[[selector.name]]$sql
    congregation.id = sql.variables$process.wh.congregation.id
    show.all.entered = sql.variables$process.wh.show.all.entered
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