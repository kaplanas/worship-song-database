#### Utility functions ####

populate.management.table = function(manage.table, db.con, reactive.tables) {
  if(!is.null(db.con)) {
    manage.info = manage.table.info[[manage.table]]
    tryCatch(
      {
        temp.df = dbGetQuery(db.con, manage.info$populate.sql)
        for(i in 1:nrow(manage.info$columns)) {
          col.info = manage.info$columns[i,]
          if(!is.na(col.info$key.table)) {
            temp.df = temp.df %>%
              left_join(reactive.tables[[col.info$key.table]],
                        by = col.info$column.name)
          }
          if(col.info$type == "checkbox") {
            temp.df[,i] = temp.df[,i] == 1
          }
        }
        temp.df = temp.df[,if_else(is.na(manage.info$columns$key.table),
                                   manage.info$columns$column.name,
                                   manage.info$columns$key.label)]
        reactive.tables[[manage.table]] = temp.df
      },
      error = function(err) {
        print(err)
        reactive.tables[[manage.table]] = NULL
      }
    )
  } else {
    reactive.tables[[manage.table]] = NULL
  }
}

populate.utility.table = function(utility.table, db.con, reactive.tables) {
  if(!is.null(db.con)) {
    utility.info = utility.table.info[[utility.table]]
    tryCatch(
      {
        reactive.tables[[utility.table]] = dbGetQuery(db.con, utility.info$sql)
      },
      error = function(err) {
        print(err)
        reactive.tables[[utility.table]] = NULL
      }
    )
  } else {
    reactive.tables[[utility.table]] = NULL
  }
}

update.selector = function(selector.name, db.con, session) {
  selector.list = list()
  tryCatch(
    {
      selector.list = dbGetQuery(db.con, selector.info[[selector.name]]$sql) %>%
        column_to_rownames("SelectorDisplay") %>%
        t() %>%
        as.data.frame() %>%
        as.list()
    },
    error = function(err) {
      print(err)
      selector.list = list()
    },
    finally = {
      updateSelectInput(session, selector.name, choices = selector.list)
    }
  )
}

populate.processing.table = function(db.con, reactive.tables,
                                     process.return.hymnologist) {
  if(!is.null(db.con)) {
    tryCatch(
      {
        sql = glue_sql(process.return.info$select.sql, .con = db.con)
        reactive.tables$process.return = dbGetQuery(db.con, sql)
      },
      error = function(err) {
        print(err)
        reactive.tables$process.return = NULL
      }
    )
  } else {
    reactive.tables$process.return = NULL
  }
}

populate.summary.table = function(db.con, reactive.tables) {
  if(!is.null(db.con)) {
    tryCatch(
      {
        reactive.tables$lhp.summary = dbGetQuery(db.con, summary.table.info$sql)
      },
      error = function(err) {
        print(err)
        reactive.tables$lhp.summary = NULL
      }
    )
  } else {
    reactive.tables$lhp.summary = NULL
  }
}

#### Utility tables ####

hymnologist.labels.info = list(
  sql = "SELECT HymnologistID, HymnologistLabel
         FROM lhp.hymnologist_labels
         ORDER BY LastName, FirstName"
)

song.labels.info = list(
  sql = "SELECT NULL AS SongID, '' AS SongLabel
         FROM dual
         UNION ALL
         SELECT SongID, SongLabel
         FROM lhp.song_labels
         ORDER BY SongLabel"
)