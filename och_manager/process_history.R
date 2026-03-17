# Processing page
process.page = tabPanel(
  "Process worship history",
  fluidRow(
    column(
      2, align = "center",
      actionButton("refresh.process.wh", label = "Refresh data")
    ),
    column(
      2, align = "center",
      actionButton("mark.all.processed.wh", label = "Mark all as processed")
    ),
    column(
      2,
      selectInput("process.wh.filter", "Dates to show:",
                  c("All dates", "Dates with unprocessed records",
                    "Dates with notes"))
    ),
    column(
      3,
      uiOutput("process.wh.date")
    )
  ),
  rHandsontableOutput("process.worship.history"),
  actionButton("save.worship.history", label = "Save changes")
)

# Get dates we need to update the date picker
get.processing.dates = function(current.dates.df, date.picker.filter,
                                date.picker.current) {
  highlighted.dates = current.dates.df$worship.date
  min.date = NULL
  max.date = NULL
  selected.date = NULL
  if(length(current.dates.df) > 0 ) {
    if(date.picker.filter == "Dates with unprocessed records") {
      highlighted.dates = current.dates.df$worship.date[current.dates.df$AnyUnprocessed]
    } else if(date.picker.filter == "Dates with notes") {
      highlighted.dates = current.dates.df$worship.date[current.dates.df$AnyNotes]
    }
    if(date.picker.filter != "All dates") {
      min.date = min(highlighted.dates)
      max.date = max(highlighted.dates)
    }
  }
  if(!is.null(date.picker.current) &&
     date.picker.current %in% highlighted.dates) {
    selected.date = date.picker.current
  } else if(date.picker.filter == "All dates") {
    selected.date = as.Date(Sys.time() - (5 * 60 * 60))
  } else {
    selected.date = min.date
  }
  return(list(selected = selected.date, min = min.date, max = max.date,
              highlighted = highlighted.dates))
}

# Populate the table
populate.processing.table = function(db, username, process.wh.date) {
  if(!is.null(db)) {
    tryCatch(
      {
        min.history.id = as.numeric(paste(format(process.wh.date, "%Y%m%d"),
                                          "0000", sep = ""))
        max.history.id = as.numeric(paste(format(process.wh.date, "%Y%m%d"),
                                          "9999", sep = ""))
        history.df = query.dynamodb(db, table.name = "och_history",
                                    expression.attribute.values = list(`:c` = list(S = username),
                                                                       `:h1` = list(N = min.history.id),
                                                                       `:h2` = list(N = max.history.id)),
                                    key.condition.expression = "Congregation = :c AND HistoryID BETWEEN :h1 AND :h2",
                                    projection.expression = "WorshipDate, HistoryID, RawLine, SundayMorning, ProcessedRecord, SongID, SongInstanceID, Notes, NewSong, FileName")
        for(field in c("RawLine", "Notes", "NewSong", "FileName")) {
          if(!(field %in% colnames(history.df))) {
            history.df[[field]] = NA_character_
          }
        }
        for(field in c("SundayMorning", "ProcessedRecord", "SongID",
                       "SongInstanceID")) {
          if(!(field %in% colnames(history.df))) {
            history.df[[field]] = NA
          }
        }
        history.df = history.df %>%
           dplyr::select(WorshipDate, HistoryID, RawLine, SundayMorning,
	                 ProcessedRecord, SongID, SongInstanceID, Notes,
                         NewSong, FileName)
        return(history.df)
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
create.worship.history.hot = function(df, process.wh.date, song.labels,
                                      song.instance.labels, window.width,
                                      window.height, selected.row) {
  if(is.null(df) || nrow(df) == 0) {
    sunday.flag = F
    if(is.null(process.wh.date)) {
      sunday.flag = wday(Sys.Date(), label = T) == "Sun"
    } else {
      sunday.flag = wday(process.wh.date, label = T) == "Sun"
    }
    temp.df = data.frame(RawLine = NA_character_, SundayMorning = sunday.flag,
                         ProcessedRecord = F, SongID = NA,
                         SongLabel = NA_character_,
                         SongInstanceLabel = NA_character_,
                         Notes = NA_character_, NewSong = NA_character_)
  } else {
    temp.df = df %>%
      left_join(song.labels, by = "SongID") %>%
      left_join(song.instance.labels %>%
                  dplyr::select(-c("SongID")),
                by = "SongInstanceID") %>%
      dplyr::select(RawLine, SundayMorning, ProcessedRecord, SongID, SongLabel,
                    SongInstanceLabel, Notes, NewSong)
  }
  temp.hot = rhandsontable(temp.df %>% dplyr::select(-c("SongID")),
                           width = window.width * 0.95,
                           height = window.height * 0.7,
                           colHeaders = c("Raw text from bulletin", "Sun. AM",
                                          "Processed", "Song",
                                          "Song instance", "Notes",
                                          "New song needed"),
                           rowHeaders = NULL, overflow = "visible",
                           selectCallback = T) %>%
    hot_context_menu(allowColEdit = F) %>%
    hot_cols(colWidths = c(300, 80, 80, 300, 400, 300, 300), columnSorting = F) %>%
    hot_col(col = "Song", type = "dropdown", strict = T, renderer = "html",
            source = c("", song.labels$SongLabel)) %>%
    hot_col(col = "Song", renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
    hot_col(col = "Song instance", type = "dropdown", strict = F,
            renderer = "html", source = c()) %>%
    hot_col(col = "Song instance",
            renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
    hot_col(col = c("Raw text from bulletin"), readOnly = T)
  if(!is.null(selected.row)) {
    if(!is.na(temp.df[selected.row,"SongLabel"]) & temp.df[selected.row,"SongLabel"] != "") {
      song.id = song.labels %>%
        filter(SongLabel == as.character(temp.df[selected.row,"SongLabel"])) %>%
        pull(SongID)
      song.instance.options = song.instance.labels %>%
        filter(SongID == song.id) %>%
        pull(SongInstanceLabel)
      song.instance.options = c("", song.instance.options)
      temp.hot = temp.hot %>%
        hot_col(col = "Song instance", type = "dropdown", strict = F,
                source = song.instance.options)
    }
  }
  temp.hot
}

# When the user makes a change in the interface, propagate the change to the
# reactive table
update.worship.history.hot = function(change, process.wh.date,
                                      song.labels, song.instance.labels,
                                      reactive.worship.history.processing) {
  
  # Get the worship date
  numeric.date = format(process.wh.date, "%Y%m%d")

  # If it was an edit, update the edited cell
  if(change$event == "afterChange") {
    if(!is.null(change$changes)) {
      r = change$changes[[1]][[1]] + 1
      c = change$changes[[1]][[2]] + 3
      change.cell = change$changes[[1]][[4]]
      if(c == 6) {
        if(change.cell == "") {
          change.cell = NA
        } else {
          change.cell = song.labels %>%
            filter(SongLabel == as.character(change.cell)) %>%
            pull(SongID)
          if(length(change.cell) == 0) {
            change.cell = NA
          }
        }
      } else if(c == 7) {
        if(change.cell == "") {
          change.cell = NA
        } else {
          change.cell = song.instance.labels %>%
            filter(SongInstanceLabel == as.character(change.cell)) %>%
            pull(SongInstanceID)
          if(length(change.cell) == 0) {
            change.cell = NA
          }
        }
      }
      if(is.null(reactive.worship.history.processing$table)) {
        reactive.worship.history.processing$table =
          data.frame(HistoryID = as.numeric(paste(numeric.date, "0001",
                                            sep = "")),
                     WorshipDate = numeric.date,
                     RawLine = NA_character_,
                     SundayMorning = wday(process.wh.date, label = T) == "Sun",
                     ProcessedRecord = F, SongID = NA, SongInstanceID = NA,
                     Notes = NA_character_, NewSong = NA_character_,
                     FileName = NA_character_)
      }
      reactive.worship.history.processing$table[r,c] = change.cell
      reactive.worship.history.processing$changes$edit =
        c(reactive.worship.history.processing$changes$edit,
          reactive.worship.history.processing$table[r,"HistoryID"])
    }
  }

  # If it was a new row, insert an empty row
  else if(change$event == "afterCreateRow") {
    if(is.null(reactive.worship.history.processing$table)) {
      temp.df = data.frame(WorshipDate = as.numeric(numeric.date),
                           HistoryID = as.numeric(paste(numeric.date, "0001",
                                                        sep = "")),
                           RawLine = NA_character_,
                           SundayMorning = wday(process.wh.date, label = T) == "Sun",
                           ProcessedRecord = F, SongID = NA,
                           SongInstanceID = NA, Notes = NA_character_,
                           NewSong = NA_character_, FileName = NA_character_)
    } else {
      temp.df = reactive.worship.history.processing$table
    }
    reactive.worship.history.processing$table = temp.df %>%
      add_row() %>%
      mutate(HistoryID = coalesce(HistoryID, max(HistoryID, na.rm = T) + 1),
             SundayMorning = coalesce(SundayMorning,
                                      wday(process.wh.date, label = T) == "Sun"))
  }

  # If it was a deleted row, remove that row
  else if(change$event == "afterRemoveRow") {
    r = change$ind + 1
    if(change$ct > 1) {
      r = seq(r, length.out = change$ct)
    }
    history.id = reactive.worship.history.processing$table$HistoryID[r]
    reactive.worship.history.processing$table = reactive.worship.history.processing$table[-r,]
    reactive.worship.history.processing$changes$delete =
      c(reactive.worship.history.processing$changes$delete, history.id)
  }

}

# If the user clicks "save", write the table to the database and update other
# tables accordingly
save.worship.history.table = function(reactive.worship.history.processing,
                                      username, process.wh.date, db) {

  # Keep track of whether all updates were successful
  successful.updates = T

  # Numeric version of the current date
  numeric.date = as.numeric(format(process.wh.date, "%Y%m%d"))

  # Get raw data to use for update (mapping labels back to IDs)
  temp.df = reactive.worship.history.processing$table %>%
    mutate(Congregation = username,
           WorshipDate = numeric.date,
           ProcessedRecord = coalesce(ProcessedRecord, F)) %>%
    dplyr::select(Congregation, HistoryID, WorshipDate, RawLine,
                  ProcessedRecord, SundayMorning, SongID, SongInstanceID, Notes,
                  NewSong, FileName)
  
  # If there were deletions, delete the items
  if(length(reactive.worship.history.processing$changes$delete) > 0) {
    tryCatch(
      {
        for(history.id in reactive.worship.history.processing$changes$delete) {
          db$delete_item(TableName = "och_history",
                         Key = list(Congregation = list(S = username),
                                    HistoryID = list(N = history.id)))
        }
        reactive.worship.history.processing$changes$delete = c()
      },
      error = function(err) {
        print(err)
        successful.updates = F
      }
    )
  }
  
  # If there were edits or inserts, update those items
  if(length(reactive.worship.history.processing$changes$edit) > 0) {
    item.cols = sapply(temp.df, class)
    write.keys = unique(reactive.worship.history.processing$changes$edit)
    for(wk in write.keys) {
      tryCatch(
        {
          item.row = temp.df %>% filter(HistoryID == wk)
          item = list()
          for(field in names(item.cols)) {
            if(!is.na(item.row[[field]])) {
              if(item.cols[field] %in% c("integer", "numeric")) {
                item[[field]] = list(N = item.row[[field]])
              } else if(item.cols[field] == "character") {
                item[[field]] = list(S = item.row[[field]])
              } else if(item.cols[field] == "logical") {
                item[[field]] = list(BOOL = item.row[[field]])
              }
            }
          }
          db$put_item(TableName = "och_history", Item = item)
          reactive.worship.history.processing$changes$edit = c()
        },
        error = function(err) {
          print(err)
          successful.updates = F
        }
      )
    }
  }
  
  # Update the date entry (or delete it if we've deleted everything)
  tryCatch(
    {
      item = list(Congregation = list(S = username),
                  WorshipDate = list(N = numeric.date))
      if(nrow(temp.df) == 0) {
        db$delete_item(TableName = "och_dates", Key = item)
      } else {
        item[["AnyUnprocessed"]] = list(BOOL = any(!temp.df$ProcessedRecord))
        item[["AnyNotes"]] = list(BOOL = any(!is.na(temp.df$Notes) & temp.df$Notes != ""))
        db$put_item(TableName = "och_dates", Item = item)
      }
    },
    error = function(err) {
      print(err)
    }
  )

  # If anything went wrong, display a notice
  if(successful.updates) {
    showNotification("Changes saved", type = "message")
  } else {
    showNotification("Some changes may not have been saved", type = "error")
  }

}
