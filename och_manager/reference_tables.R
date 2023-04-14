#### Congregations ####

manage.congregations.info = list(
  table = "och.congregations",
  columns = data.frame(
    column.name = c("CongregationID", "CongregationName", "Street", "City",
                    "State", "ZIPCode", "EmailAddress", "Website", "Latitude",
                    "Longitude", "FolderName", "Created", "Updated"),
    key.table = rep(NA_character_, 13),
    key.label = rep(NA_character_, 13),
    width = c(150, 300, 200, 200, 50, 70, 200, 300, 100, 100, 300, 100, 100),
    type = c("numeric", "text", "text", "text", "text", "text", "text", "text",
             "numeric", "numeric", "text", "date", "date"),
    editable = c(F, T, T, T, T, T, T, T, F, F, F, F, F),
    html = rep(F, 13),
    stringsAsFactors = F
  ),
  fixed.cols = 2,
  sort = c("CongregationName"),
  key = "CongregationID",
  related.label.tables = c("congregation.labels"),
  related.selectors = c("wh.file.congregation.id",
                        "process.wh.congregation.id"),
  related.processing.table = F
)

manage.congregations = tabPanel(
  "Congregations",
  rHandsontableOutput("congregations"),
  actionButton("save.congregations", label = "Save changes")
)

#### Combined info ####

# List with everything
reference.table.info = list(
  congregations = manage.congregations.info
)

# Use data on columns to create SELECT, UPDATE, INSERT, and DELETE statements
for(reference.table in names(reference.table.info)) {
  reference.info = reference.table.info[[reference.table]]
  sql.cols = paste(reference.info$columns$column.name, collapse = ", ")
  sql = paste("SELECT ", sql.cols, " FROM ", reference.info$table, " ORDER BY ",
              paste(reference.info$sort, collapse = ", "), sep = "")
  reference.table.info[[reference.table]]$populate.sql = sql
  sql = reference.info$columns %>%
    filter(editable |
             (reference.table == "congregations" &
                column.name %in% c("Latitude", "Longitude"))) %>%
    mutate(update = paste(column.name, " = {", column.name, "}", sep = "")) %>%
    pull(update) %>%
    paste(collapse = ", ") %>%
    paste("UPDATE ", reference.info$table, " SET ", ., " WHERE ",
          reference.info$key, " = {", reference.info$key, "}", sep = "")
  reference.table.info[[reference.table]]$update.sql = sql
  sql = paste(
    "INSERT INTO ", reference.info$table, "(",
    reference.info$columns %>%
      filter(editable |
               (reference.table == "congregations" &
                  column.name %in% c("Latitude", "Longitude"))) %>%
      pull(column.name) %>%
      paste(collapse = ", "),
    ") VALUES (",
    reference.info$columns %>%
      filter(editable |
               (reference.table == "congregations" &
                  column.name %in% c("Latitude", "Longitude"))) %>%
      mutate(column.name = paste("{", column.name, "}", sep = "")) %>%
      pull(column.name) %>%
      paste(collapse = ", "),
    ")", sep = ""
  )
  reference.table.info[[reference.table]]$insert.sql = sql
  sql = paste("DELETE FROM ", reference.info$table, " WHERE ",
              reference.info$key, " NOT IN ({keys*})", sep = "")
  reference.table.info[[reference.table]]$delete.sql = sql
}

#### Management page ####

reference.tables.page = tabPanel("Manage reference tables",
                                 navlistPanel(
                                   manage.congregations,
                                   widths = c(2, 10)
                        ))

#### Useful functions ####

# Populate the table
populate.reference.table = function(reference.table, db.con,
                                    reactive.label.tables) {
  reference.info = reference.table.info[[reference.table]]
  if(!is.null(db.con)) {
    tryCatch(
      {
        temp.df = dbGetQuery(db.con, reference.info$populate.sql)
        for(i in 1:nrow(reference.info$columns)) {
          col.info = reference.info$columns[i,]
          if(!is.na(col.info$key.table)) {
            temp.df = temp.df %>%
              left_join(reactive.label.tables[[col.info$key.table]],
                        by = col.info$column.name)
          }
          if(col.info$type == "checkbox") {
            temp.df[,i] = temp.df[,i] == 1
          }
        }
        temp.df = temp.df[,if_else(is.na(reference.info$columns$key.table),
                                   reference.info$columns$column.name,
                                   reference.info$columns$key.label)]
      },
      error = function(err) {
        print(err)
        return(NULL)
      },
      finally = {
        if(reference.table == "congregations") {
          tryCatch(
            {
              current.folders = get_bucket("worship-song-database",
                                           prefix = "observing_congregational_hymnody/")
              current.folders = map_chr(current.folders, function(x) { x$Key })
              current.folders = current.folders[!grepl("./.", current.folders)]
              missing.folders = setdiff(temp.df$FolderName, current.folders)
              for(mf in missing.folders) {
                put_folder("worship-song-database",
                           folder = paste("observing_congregational_hymnody",
                                          mf, sep = "/"))
              }
            },
            error = function(err) {
              print(err)
            }
          )
        }
        return(temp.df)
      }
    )
  } else {
    return(NULL)
  }
}

# Create the rhandsontable
create.reference.hot = function(df, reference.table, reactive.label.tables,
                                window.width, window.height) {
  reference.info = reference.table.info[[reference.table]]
  temp.df = df
  if(is.null(temp.df)) {
    NULL
  } else {
    temp.table = rhandsontable(temp.df, rowHeaders = F, overflow = "visible",
                               width = window.width * 0.65,
                               height = window.height * 0.8) %>%
      hot_context_menu(allowColEdit = F) %>%
      hot_cols(colWidths = reference.info$columns$width,
               fixedColumnsLeft = reference.info$fixed.cols,
               columnSorting = F)
    for(i in 1:nrow(reference.info$columns)) {
      col.info = reference.info$columns[i,]
      temp.table = temp.table %>%
        hot_col(col = i, readOnly = !col.info$editable,
                type = col.info$type,
                halign = case_when(col.info$type == "checkbox" ~ "htCenter",
                                   col.info$type == "numeric" ~ "htRight",
                                   T ~ "htLeft"))
      if(col.info$html) {
        temp.table = temp.table %>%
          hot_col(col = i, format = "html",
                  renderer = htmlwidgets::JS("safeHtmlRenderer"))
      }
      if(col.info$type == "dropdown") {
        temp.table = temp.table %>%
          hot_col(col = i,
                  source = reactive.label.tables[[col.info$key.table]][[col.info$key.label]])
      }
    }
    temp.table
  }
}

# If the user makes a change in the interface, propagate the change to the
# reactive table
update.reference.table = function(reference.table, change,
                                  reactive.reference.tables,
                                  reactive.reference.changes) {
  
  # Get info for this table
  reference.info = reference.table.info[[reference.table]]
  
  # If it was an edit, update the edited cell
  if(change$event == "afterChange") {
    if(!is.null(change$changes)) {
      r = change$changes[[1]][[1]] + 1
      c = change$changes[[1]][[2]] + 1
      reactive.reference.tables[[reference.table]][r,c] = change$changes[[1]][[4]]
      reactive.reference.changes[[reference.table]]$edit =
        c(reactive.reference.changes[[reference.table]]$edit,
          reactive.reference.tables[[reference.table]][r,reference.info$key])
    }
  }
  
  # If it was a new row, insert an empty row
  else if(change$event == "afterCreateRow") {
    reactive.reference.tables[[reference.table]][nrow(reactive.reference.tables[[reference.table]]) + 1,] = NA
    reactive.reference.changes[[reference.table]]$insert = T
  }
  
  # If it was a deleted row, remove that row
  else if(change$event == "afterRemoveRow") {
    r = change$ind + 1
    reactive.reference.tables[[reference.table]] = reactive.reference.tables[[reference.table]][-r,]
    reactive.reference.changes[[reference.table]]$delete = T
  }
  
}

# If the user clicks "save", write the table to the database
save.reference.table = function(reference.table, db.con,
                                reactive.reference.tables,
                                reactive.label.tables,
                                reactive.reference.changes) {
  
  # Info about this table
  reference.info = reference.table.info[[reference.table]]
  
  # Get raw data to use for update (mapping labels back to IDs)
  temp.df = reactive.reference.tables[[reference.table]]
  for(i in 1:nrow(reference.info$columns)) {
    if(!is.na(reference.info$columns$key.table[i])) {
      temp.df = temp.df %>%
        left_join(reactive.label.tables[[reference.info$columns$key.table[i]]],
                  by = reference.info$columns$key.label[i])
    }
  }
  temp.df = temp.df[,reference.info$columns$column.name]
  
  # If there were edits, issue an UPDATE statement
  if(length(reactive.reference.changes[[reference.table]]$edit) > 0) {
    
    # Get changed rows
    changed.df = temp.df %>%
      filter(.data[[reference.info$key]] %in% reactive.reference.changes[[reference.table]]$edit)
    
    # Geocode addresses (always, just in case the address changed)
    if(reference.table == "congregations") {
      changed.df = changed.df %>%
        dplyr::select(-c(Latitude, Longitude)) %>%
        tidygeocoder::geocode(street = Street, city = City, state = State,
                              lat = Latitude, long = Longitude, limit = 1,
                              method = "census") %>%
        mutate(Latitude = round(Latitude, 4),
               Longitude = round(Longitude, 4))
    }
    
    # Create sql to update changed rows
    sql = changed.df %>%
      glue_data_sql(reference.info$update.sql, .con = db.con)
    
    # Attempt to update changed rows
    for(s in sql) {
      tryCatch(
        {
          dbGetQuery(db.con, s)
          reactive.reference.changes[[reference.table]]$edit = c()
          show.changes.saved(T,
                             db.table = gsub("^.*(och\\.[a-z_]+).*$", "\\1",
                                             s))
        },
        error = function(err) {
          print(err)
          show.changes.saved(F, err.msg = err)
        }
      )
    }
    
  }
  
  # If there were inserts, issue an INSERT statement
  if(reactive.reference.changes[[reference.table]]$insert) {
    
    # Attempt to insert new rows
    if(any(is.na(temp.df[[reference.info$key]]))) {
      
      # Get new rows
      new.df = temp.df %>%
        filter(is.na(.data[[reference.info$key]]))
      
      # Geocode addresses
      if(reference.table == "congregations") {
        new.df = new.df %>%
          dplyr::select(-c(Latitude, Longitude)) %>%
          tidygeocoder::geocode(street = Street, city = City, state = State,
                                lat = Latitude, long = Longitude, limit = 1,
                                method = "census") %>%
          mutate(Latitude = round(Latitude, 4),
                 Longitude = round(Longitude, 4))
      }
      
      # Create SQL
      sql = new.df %>%
        glue_data_sql(reference.info$insert.sql, .con = db.con)
      sql = gsub("NULL", "DEFAULT", sql)
      
      # Attempt to execute SQL
      for(s in sql) {
        tryCatch(
          {
            if(reference.table == "congregations") {
              dbGetQuery(db.con,
                         glue_sql("ANALYZE TABLE och.congregations",
                                  table.to.analyze = reference.info$table,
                                  .con = db.con))
            }
            dbGetQuery(db.con, s)
            reactive.reference.changes[[reference.table]]$insert = F
            show.changes.saved(T,
                               db.table = gsub("^.*(och\\.[a-z_]+).*$", "\\1",
                                               s))
          },
          error = function(err) {
            print(err)
            show.changes.saved(F, err.msg = err)
          }
        )
      }
      
    }
    
  }
  
  # If there were deletions, issue a DELETE statement
  if(reactive.reference.changes[[reference.table]]$delete) {
    
    # Create sql to delete rows
    sql = glue_sql(reference.info$delete.sql,
                   keys = temp.df[[reference.info$key]], .con = db.con)
    
    # Attempt to delete rows
    tryCatch(
      {
        dbGetQuery(db.con, sql)
        reactive.reference.changes[[reference.table]]$delete = F
        show.changes.saved(T,
                           db.table = gsub("^.*(och\\.[a-z_]+).*$", "\\1",
                                           sql))
      },
      error = function(err) {
        print(err)
        show.changes.saved(F, err.msg = err)
      }
    )
    
  }
  
}
