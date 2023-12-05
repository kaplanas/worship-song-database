#### Processing table info ####

process.songbook.info = list(
  select.sql = "SELECT songbookentries.SongbookEntryID,
                       songbook_labels.SongbookLabel,
                       songbookvolume_labels.SongbookVolumeLabel,
                       songinstance_labels.SongInstanceLabel,
                       songbookentries.EntryNumber
                FROM wsdb.songbookentries
                     LEFT JOIN wsdb.songbook_labels
                     ON songbookentries.SongbookID = songbook_labels.SongbookID
                     LEFT JOIN wsdb.songbookvolume_labels
                     ON songbookentries.SongbookVolumeID = songbookvolume_labels.SongbookVolumeID
                     LEFT JOIN wsdb.songinstance_labels
                     ON songbookentries.SongInstanceID = songinstance_labels.SongInstanceID
                WHERE songbookentries.SongbookID = {process.songbook.id}
                      AND songbookentries.SongbookVolumeID = {process.songbook.volume.id}
                ORDER BY SongbookEntryID",
  columns = data.frame(
    column.name = c("SongbookEntryID", "SongbookID", "SongbookVolumeID",
                    "EntryNumber", "SongInstanceID"),
    displayed = c(F, F, F, T, T),
    editable = c(F, F, F, T, T),
    width = c(NA, NA, NA, 400, 200),
    stringsAsFactors = F
  ),
  update.sql = "UPDATE wsdb.songbookentries
                SET EntryNumber = {EntryNumber},
                    SongInstanceID = {SongInstanceID}
                WHERE SongbookEntryID = {SongbookEntryID}",
  insert.sql = "INSERT INTO wsdb.songbookentries
                (SongbookID, SongbookVolumeID, EntryNumber, SongInstanceID)
                VALUES
                ({SongbookID}, {SongbookVolumeID}, {EntryNumber},
                 {SongInstanceID})",
  delete.sql = "DELETE FROM wsdb.songbookentries
                WHERE SongbookID = {process.songbook.id}
                      AND SongbookVolumeID = {process.songbook.volume.id}
                      AND SongbookEntryID NOT IN ({keys*})"
)

#### Processing page ####

process.songbooks = tabPanel(
  "Process songbooks",
  fluidRow(
    column(
      6,
      selectizeInput("process.songbook.id", "Choose songbook:",
                   choices = list())
    ),
    column(
      6,
      selectizeInput("process.songbook.volume.id", "Choose songbook volume:",
                     choices = list())
    )
  ),
  rHandsontableOutput("process.songbook"),
  actionButton("save.songbook", label = "Save changes")
)

songbooks.page = tabPanel("Process songbooks",
                          navlistPanel(
                            process.songbooks,
                            well = F,
                            widths = c(2, 10)
                          ))

#### Useful functions ####

# Populate the table
populate.processing.table = function(db.con, process.songbook.id,
                                     process.songbook.volume.id) {
  if(!is.null(db.con)) {
    tryCatch(
      {
        if(process.songbook.volume.id == -1) {
          process.songbook.volume.id = NA
        }
        sql = glue_sql(process.songbook.info$select.sql, .con = db.con)
        sql = gsub("= NULL", "IS NULL", sql)
        return(dbGetQuery(db.con, sql))
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

# Create the rhandsontable
create.songbook.hot = function(df, reactive.label.tables, window.width,
                               window.height) {
  if(is.null(df)) {
    NULL
  } else {
    temp.df = df[,process.songbook.info$columns$displayed]
    if(nrow(temp.df) == 0) {
      temp.df = temp.df %>% add_row()
    }
    rhandsontable(temp.df, width = window.width * 0.75,
                  height = window.height * 0.65, rowHeaders = NULL,
                  overflow = "visible") %>%
      hot_context_menu(allowColEdit = F) %>%
      hot_cols(colWidths = process.songbook.info$columns$width[process.songbook.info$columns$displayed],
               columnSorting = F) %>%
      hot_col(col = "SongInstanceLabel", type = "dropdown", strict = T,
              renderer = "html",
              source = reactive.label.tables$song.instance.labels$SongInstanceLabel) %>%
      hot_col(col = "SongInstanceLabel",
              renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
      hot_col(col = which(!process.songbook.info$columns$editable[process.songbook.info$columns$displayed]),
              readOnly = T)
  }
}

# When the user makes a change in the interface, propagate the change to the
# reactive table
update.songbook.hot = function(change, reactive.songbook.processing) {
    
  # If it was an edit, update the edited cell
  if(change$event == "afterChange") {
    if(!is.null(change$changes)) {
      r = change$changes[[1]][[1]] + 1
      c = which(process.songbook.info$columns$displayed)[change$changes[[1]][[2]] + 1]
      reactive.songbook.processing$table[r,c] = change$changes[[1]][[4]]
      reactive.songbook.processing$changes$edit =
        c(reactive.songbook.processing$changes$edit,
          reactive.songbook.processing$table[r,"SongbookEntryID"])
    }
  }
  
  # If it was a new row, insert an empty row
  else if(change$event == "afterCreateRow") {
    reactive.songbook.processing$table[nrow(reactive.songbook.processing$table) + 1,] = NA
    reactive.songbook.processing$changes$insert = T
  }
  
  # If it was a deleted row, remove that row
  else if(change$event == "afterRemoveRow") {
    r = change$ind + 1
    reactive.songbook.processing$table = reactive.songbook.processing$table[-r,]
    reactive.songbook.processing$changes$delete = T
  }
  
}

# If the user clicks "save", write the table to the database and update other
# tables accordingly
save.songbook.table = function(reactive.songbook.processing,
                               process.songbook.id, process.songbook.volume.id,
                               reactive.label.tables, db.con) {
  
  # Keep track of whether all updates were successful
  successful.updates = T
  
  # Get raw data to use for update (mapping labels back to IDs)
  temp.df = reactive.songbook.processing$table %>%
    left_join(reactive.label.tables$song.instance.labels,
              by = "SongInstanceLabel") %>%
    mutate(SongbookID = process.songbook.id,
           SongbookVolumeID = na_if(process.songbook.volume.id, "-1")) %>%
    dplyr::select(SongbookEntryID, SongbookID, SongbookVolumeID, SongInstanceID,
                  EntryNumber)
  
  # If there were edits, issue an UPDATE statement
  if(length(reactive.songbook.processing$changes$edit) > 0) {
    
    # Create sql to update changed rows
    sql = temp.df %>%
      filter(SongbookEntryID %in% reactive.songbook.processing$changes$edit) %>%
      glue_data_sql(process.songbook.info$update.sql, .con = db.con)
    sql = gsub("= NULL", "IS NULL", sql)
    
    # Attempt to update changed rows
    for(s in sql) {
      tryCatch(
        {
          dbGetQuery(db.con, s)
          reactive.songbook.processing$changes$edit = F
        },
        error = function(err) {
          print(err)
          successful.updates = F
        }
      )
    }
    
  }
  
  # If there were inserts, issue an INSERT statement
  if(reactive.songbook.processing$changes$insert ||
     (nrow(temp.df) == 1 & is.na(temp.df$SongbookEntryID))) {
    
    # Attempt to insert new rows
    if(any(is.na(temp.df$SongbookEntryID))) {
      sql = temp.df %>%
        filter(is.na(SongbookEntryID)) %>%
        dplyr::select(-SongbookEntryID) %>%
        glue_data_sql(process.songbook.info$insert.sql, .con = db.con)
      sql = gsub("= NULL", "IS NULL", sql)
      for(s in sql) {
        tryCatch(
          {
            dbGetQuery(db.con, s)
            reactive.songbook.processing$changes$insert = F
          },
          error = function(err) {
            print(err)
            successful.updates = F
          }
        )
      }
    }
    
  }
  
  # If there were deletions, issue a DELETE statement
  if(reactive.songbook.processing$changes$delete) {
    
    # Create sql to delete rows
    sql = glue_sql(process.songbook.info$delete.sql,
                   process.songbook.id = process.songbook.id,
                   process.songbook.volume.id = na_if(process.songbook.volume.id, "-1"),
                   keys = paste(temp.df$SongbookEntryID), .con = db.con)
    sql = gsub("= NULL", "IS NULL", sql)
    
    # Attempt to delete rows
    tryCatch(
      {
        dbGetQuery(db.con, sql)
        reactive.songbook.processing$changes$delete = F
      },
      error = function(err) {
        print(err)
        successful.updates = F
      }
    )
    
  }
  
  # If anything went wrong, display a notice
  if(successful.updates) {
    showNotification("Changes saved", type = "message")
  } else {
    showNotification("Some changes may not have been saved", type = "error")
  }
  
}
