#### Processing table info ####

process.worship.history.info = list(
  select.sql = "SELECT worshiphistory.WorshipHistoryID, worshiphistory.RawLine,
                       worshiphistory.Processed, song_labels.SongLabel,
                       songinstance_labels.SongInstanceLabel,
                       worshiphistory.AmbiguousSong, worshiphistory.FileName,
                       worshiphistory.Created, worshiphistory.Updated
                FROM och.worshiphistory
                     LEFT JOIN wsdb.song_labels
                     ON worshiphistory.SongID = song_labels.SongID
                     LEFT JOIN och.songinstance_labels
                     ON worshiphistory.SongInstanceID = songinstance_labels.SongInstanceID
                WHERE worshiphistory.CongregationID = {process.wh.congregation.id}
                      AND worshiphistory.WorshipDate = {process.wh.date}
                ORDER BY WorshipHistoryID",
  columns = data.frame(
    column.name = c("WorshipHistoryID", "RawLine", "Processed", "SongID",
                    "SongInstanceID", "AmbiguousSong", "FileName", "Created",
                    "Updated"),
    displayed = c(F, T, T, T, T, T, T, T, T),
    editable = c(F, F, T, T, T, T, F, F, F),
    width = c(NA, 200, 80, 300, 400, 300, 300, 100, 100),
    stringsAsFactors = F
  ),
  update.sql = "UPDATE och.worshiphistory
                SET Processed = {Processed},
                    SongID = {SongID},
                    SongInstanceID = {SongInstanceID},
                    AmbiguousSong = {AmbiguousSong}
                WHERE WorshipHistoryID = {WorshipHistoryID}",
  insert.sql = "INSERT INTO och.worshiphistory
                (CongregationID, WorshipDate, RawLine, Processed, SongID,
                 SongInstanceID, AmbiguousSong)
                VALUES
                ({CongregationID}, {WorshipDate}, {RawLine}, {Processed},
                 {SongID}, {SongInstanceID}, {AmbiguousSong})",
  delete.sql = "DELETE FROM och.worshiphistory
                WHERE CongregationID = {process.wh.congregation.id}
                      AND WorshipDate = {process.wh.date}
                      AND WorshipHistoryID NOT IN ({keys*})"
)

#### Processing pages ####

upload.worship.history = tabPanel(
  "Upload worship history",
  uiOutput("wh.file.congregation.id"),
  selectizeInput("wh.file.type", label = "File type:",
                 choices = c("Spreadsheet", "Bulletin")),
  fileInput("wh.file", label = "", multiple = T, width = "500px",
            accept = c(".xls", ".xlsx", ".pdf"),
            buttonLabel = "Choose file..."),
  checkboxInput("wh.file.overwrite",
                label = "Overwrite dates that have already been entered?"),
  actionButton("upload.wh.file", label = "Save file")
)

process.worship.history = tabPanel(
  "Process worship history",
  fluidRow(
    column(
      2,
      fluidRow(
        actionButton("refresh.process.wh", label = "Refresh data")
      ),
      fluidRow(
        actionButton("mark.all.processed.wh", label = "Mark all as processed")
      )
    ),
    column(
      4,
      uiOutput("process.wh.congregation.id")
    ),
    column(
      3,
      checkboxInput("process.wh.show.all.entered", "Show all entered dates")
    ),
    column(
      3,
      uiOutput("process.wh.date")
    )
  ),
  rHandsontableOutput("process.worship.history"),
  actionButton("save.worship.history", label = "Save changes")
)

worship.history.page = tabPanel(
  "Upload and process worship history",
  navlistPanel(
    upload.worship.history,
    process.worship.history,
    well = F,
    widths = c(2, 10)
  )
)

#### Useful functions ####

# Populate the table
populate.processing.table = function(db.con, process.wh.congregation.id,
                                     process.wh.date) {
  if(!is.null(db.con)) {
    tryCatch(
      {
        sql = glue_sql(process.worship.history.info$select.sql, .con = db.con)
        return(dbGetQuery(db.con, sql) %>%
                 mutate(Processed = Processed == 1))
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
create.worship.history.hot = function(df, reactive.label.tables, window.width,
                                      window.height, selected.row) {
  if(is.null(df)) {
    NULL
  } else {
    temp.df = df[,process.worship.history.info$columns$displayed]
    if(nrow(temp.df) == 0) {
      temp.df = temp.df %>% add_row()
    }
    temp.hot = rhandsontable(temp.df, width = window.width * 0.8,
                             height = window.height * 0.6, rowHeaders = NULL,
                             overflow = "visible", selectCallback = T) %>%
      hot_context_menu(allowColEdit = F) %>%
      hot_cols(colWidths = process.worship.history.info$columns$width[process.worship.history.info$columns$displayed],
               columnSorting = F) %>%
      hot_col(col = "Processed", type = "checkbox") %>%
      hot_col(col = "SongLabel", type = "dropdown", strict = T,
              renderer = "html",
              source = c("", reactive.label.tables$song.labels$SongLabel)) %>%
      hot_col(col = "SongLabel",
              renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
      hot_col(col = "SongInstanceLabel", type = "dropdown", strict = F,
              renderer = "html", source = c()) %>%
      hot_col(col = "SongInstanceLabel",
              renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
      hot_col(col = "AmbiguousSong", type = "dropdown", strict = F,
              source = c("",
                         reactive.label.tables$ambiguous.song.labels$SongName)) %>%
      hot_col(col = which(!process.worship.history.info$columns$editable[process.worship.history.info$columns$displayed]),
              readOnly = T)
    if(!is.null(selected.row)) {
      if(!is.na(temp.df[selected.row,"SongLabel"]) & temp.df[selected.row,"SongLabel"] != "") {
        song.id = reactive.label.tables$song.labels %>%
          filter(SongLabel == temp.df[selected.row,"SongLabel"]) %>%
          pull(SongID)
        song.instance.options = reactive.label.tables$song.instance.labels %>%
          filter(SongID == song.id) %>%
          pull(SongInstanceLabel)
        song.instance.options = c("", song.instance.options)
        temp.hot = temp.hot %>%
          hot_col(col = "SongInstanceLabel", type = "dropdown", strict = F,
                  source = song.instance.options)
      }
    }
    temp.hot
  }
}

# When the user makes a change in the interface, propagate the change to the
# reactive table
update.worship.history.hot = function(change,
                                      reactive.worship.history.processing) {

  # If it was an edit, update the edited cell
  if(change$event == "afterChange") {
    if(!is.null(change$changes)) {
      r = change$changes[[1]][[1]] + 1
      c = which(process.worship.history.info$columns$displayed)[change$changes[[1]][[2]] + 1]
      reactive.worship.history.processing$table[r,c] = change$changes[[1]][[4]]
      reactive.worship.history.processing$changes$edit =
        c(reactive.worship.history.processing$changes$edit,
          reactive.worship.history.processing$table[r,"WorshipHistoryID"])
    }
  }

  # If it was a new row, insert an empty row
  else if(change$event == "afterCreateRow") {
    reactive.worship.history.processing$table[nrow(reactive.worship.history.processing$table) + 1,] = NA
    reactive.worship.history.processing$changes$insert = T
  }

  # If it was a deleted row, remove that row
  else if(change$event == "afterRemoveRow") {
    r = change$ind + 1
    reactive.worship.history.processing$table = reactive.worship.history.processing$table[-r,]
    reactive.worship.history.processing$changes$delete = T
  }

}

# If the user clicks "save", write the table to the database and update other
# tables accordingly
save.worship.history.table = function(reactive.worship.history.processing,
                                      process.wh.congregation.id,
                                      process.wh.date, reactive.label.tables,
                                      db.con) {

  # Keep track of whether all updates were successful
  successful.updates = T

  # Get raw data to use for update (mapping labels back to IDs)
  temp.df = reactive.worship.history.processing$table %>%
    left_join(reactive.label.tables$song.labels, by = "SongLabel") %>%
    left_join(reactive.label.tables$song.instance.labels %>%
                dplyr::select(SongInstanceID, SongInstanceLabel) %>%
                mutate(SongInstanceLabel = gsub("\n+$", "", SongInstanceLabel)),
              by = "SongInstanceLabel") %>%
    mutate(CongregationID = process.wh.congregation.id,
           WorshipDate = process.wh.date,
           Processed = coalesce(Processed, F)) %>%
    dplyr::select(WorshipHistoryID, CongregationID, WorshipDate, RawLine,
                  Processed, SongID, SongInstanceID, AmbiguousSong)

  # If there were edits, issue an UPDATE statement
  if(length(reactive.worship.history.processing$changes$edit) > 0) {

    # Create sql to update changed rows
    sql = temp.df %>%
      filter(WorshipHistoryID %in% reactive.worship.history.processing$changes$edit) %>%
      glue_data_sql(process.worship.history.info$update.sql, .con = db.con)

    # Attempt to update changed rows
    for(s in sql) {
      tryCatch(
        {
          dbGetQuery(db.con, s)
          reactive.worship.history.processing$changes$edit = F
        },
        error = function(err) {
          print(err)
          successful.updates = F
        }
      )
    }

  }

  # If there were inserts, issue an INSERT statement
  if(reactive.worship.history.processing$changes$insert ||
     (nrow(temp.df) == 1 & any(is.na(temp.df$WorshipHistoryID)))) {

    # Attempt to insert new rows
    if(any(is.na(temp.df$WorshipHistoryID))) {
      sql = temp.df %>%
        filter(is.na(WorshipHistoryID)) %>%
        dplyr::select(-WorshipHistoryID) %>%
        glue_data_sql(process.worship.history.info$insert.sql, .con = db.con)
      for(s in sql) {
        tryCatch(
          {
            dbGetQuery(db.con, s)
            reactive.worship.history.processing$changes$insert = F
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
  if(reactive.worship.history.processing$changes$delete) {

    # Create sql to delete rows
    sql = glue_sql(process.worship.history.info$delete.sql,
                   process.wh.congregation.id = process.wh.congregation.id,
                   process.wh.date = process.wh.date,
                   keys = paste(temp.df$WorshipHistoryID), .con = db.con)

    # Attempt to delete rows
    tryCatch(
      {
        dbGetQuery(db.con, sql)
        reactive.worship.history.processing$changes$delete = F
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
