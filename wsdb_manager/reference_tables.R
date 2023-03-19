#### Genders ####

manage.genders.info = list(
  table = "wsdb.genders",
  columns = data.frame(
    column.name = c("GenderID", "GenderName", "Created", "Updated"),
    key.table = rep(NA_character_, 4),
    key.label = rep(NA_character_, 4),
    width = c(100, 100, 100, 100),
    type = c("numeric", "text", "date", "date"),
    editable = c(F, T, F, F),
    html = rep(F, 4),
    stringsAsFactors = F
  ),
  fixed.cols = 1,
  sort = c("GenderID"),
  key = "GenderID",
  related.label.tables = c("gender.labels"),
  related.selectors = c(),
  related.processing.table = F
)

manage.genders = tabPanel(
  "Genders",
  rHandsontableOutput("genders"),
  actionButton("save.genders", label = "Save changes")
)

#### Artists ####

manage.artists.info = list(
  table = "wsdb.artists",
  columns = data.frame(
    column.name = c("ArtistID", "LastName", "FirstName", "GenderID", "Created",
                    "Updated"),
    key.table = c(NA, NA, NA, "gender.labels", NA, NA),
    key.label = c(NA, NA, NA, "GenderLabel", NA, NA),
    width = c(150, 100, 100, 100, 100, 100),
    type = c("numeric", "text", "text", "dropdown", "date",
             "date"),
    editable = c(F, T, T, T, F, F),
    html = rep(F, 6),
    stringsAsFactors = F
  ),
  fixed.cols = 3,
  sort = c("LastName", "FirstName"),
  key = "ArtistID",
  related.label.tables = c("artist.labels"),
  related.selectors = c(),
  related.processing.table = T
)

manage.artists = tabPanel(
  "Artists",
  rHandsontableOutput("artists"),
  actionButton("save.artists", label = "Save changes")
)

#### Copyright administrators ####

manage.copyright.administrators.info = list(
  table = "wsdb.copyrightadministrators",
  columns = data.frame(
    column.name = c("CopyrightAdministratorID", "CopyrightAdministratorName",
                    "AddressLine1", "AddressLine2", "City", "State", "ZIPCode",
                    "EmailAddress", "PhoneNumber", "Created", "Updated"),
    key.table = rep(NA_character_, 11),
    key.label = rep(NA_character_, 11),
    width = c(200, 200, 100, 100, 100, 50, 70, 200, 200, 100, 100),
    type = c("numeric", "text", "text", "text", "text", "text", "text", "text",
             "text", "date", "date"),
    editable = c(F, T, T, T, T, T, T, T, T, F, F),
    html = rep(F, 11),
    stringsAsFactors = F
  ),
  fixed.cols = 2,
  sort = c("CopyrightAdministratorName"),
  key = "CopyrightAdministratorID",
  related.label.tables = c("copyright.administrator.labels"),
  related.selectors = c(),
  related.processing.table = F
)

manage.copyright.administrators = tabPanel(
  "Copyright administrators",
  rHandsontableOutput("copyright.administrators"),
  actionButton("save.copyright.administrators", label = "Save changes")
)

#### Copyright holders ####

manage.copyright.holders.info = list(
  table = "wsdb.copyrightholders",
  columns = data.frame(
    column.name = c("CopyrightHolderID", "CopyrightHolderName",
                    "CopyrightAdministratorID", "Created", "Updated"),
    key.table = c(NA, NA, "copyright.administrator.labels", NA, NA),
    key.label = c(NA, NA, "CopyrightAdministratorLabel", NA, NA),
    width = c(150, 200, 200, 100, 100),
    type = c("numeric", "text", "dropdown", "date", "date"),
    editable = c(F, T, T, F, F),
    html = rep(F, 5),
    stringsAsFactors = F
  ),
  fixed.cols = 2,
  sort = c("CopyrightHolderName"),
  key = "CopyrightHolderID",
  related.label.tables = c("copyright.holder.labels"),
  related.selectors = c(),
  related.processing.table = T
)

manage.copyright.holders = tabPanel(
  "Copyright holders",
  rHandsontableOutput("copyright.holders"),
  actionButton("save.copyright.holders", label = "Save changes")
)

#### Languages ####

manage.languages.info = list(
  table = "wsdb.languages",
  columns = data.frame(
    column.name = c("LanguageID", "LanguageName", "Created", "Updated"),
    key.table = rep(NA_character_, 4),
    key.label = rep(NA_character_, 4),
    width = c(100, 200, 100, 100),
    type = c("numeric", "text", "date", "date"),
    editable = c(F, T, F, F),
    html = rep(F, 4),
    stringsAsFactors = F
  ),
  fixed.cols = 2,
  sort = c("LanguageID"),
  key = "LanguageID",
  related.label.tables = c("language.labels"),
  related.selectors = c(),
  related.processing.table = F
)

manage.languages = tabPanel(
  "Languages",
  rHandsontableOutput("languages"),
  actionButton("save.languages", label = "Save changes")
)

#### Books of the bible ####

manage.books.of.the.bible.info = list(
  table = "wsdb.booksofthebible",
  columns = data.frame(
    column.name = c("BookID", "BookName", "BookAbbreviation", "Created",
                    "Updated"),
    key.table = rep(NA_character_, 5),
    key.label = rep(NA_character_, 5),
    width = c(100, 200, 200, 100, 100),
    type = c("numeric", "text", "text", "date", "date"),
    editable = c(F, T, T, F, F),
    html = rep(F, 5),
    stringsAsFactors = F
  ),
  fixed.cols = 2,
  sort = c("BookID"),
  key = "BookID",
  related.label.tables = c("book.of.the.bible.labels",
                           "scripture.reference.labels"),
  related.selectors = c(),
  related.processing.table = F
)

manage.books.of.the.bible = tabPanel(
  "Books of the Bible",
  rHandsontableOutput("books.of.the.bible"),
  actionButton("save.books.of.the.bible", label = "Save changes")
)

#### Scripture references ####

manage.scripture.references.info = list(
  table = "wsdb.scripturereferences",
  columns = data.frame(
    column.name = c("ScriptureReferenceID", "BookID", "Chapter", "Verse",
                    "Created", "Updated"),
    key.table = c(NA, "book.of.the.bible.labels", NA, NA, NA, NA),
    key.label = c(NA, "BookLabel", NA, NA, NA, NA),
    width = c(150, 100, 60, 60, 100, 100),
    type = c("numeric", "dropdown", "numeric", "numeric", "date", "date"),
    editable = c(F, T, T, T, F, F),
    html = rep(F, 6),
    stringsAsFactors = F
  ),
  fixed.cols = 1,
  sort = c("BookID", "Chapter", "Verse"),
  key = "ScriptureReferenceID",
  related.label.tables = c("scripture.reference.labels"),
  related.selectors = c(),
  related.processing.table = F
)

manage.scripture.references = tabPanel(
  "Scripture references",
  rHandsontableOutput("scripture.references"),
  actionButton("save.scripture.references", label = "Save changes")
)

#### Meters ####

manage.meters.info = list(
  table = "wsdb.meters",
  columns = data.frame(
    column.name = c("MeterID", "Meter", "Multiplier", "SortString", "Created",
                    "Updated"),
    key.table = rep(NA_character_, 6),
    key.label = rep(NA_character_, 6),
    width = c(100, 200, 200, 200, 100, 100),
    type = c("numeric", "text", "text", "text", "date", "date"),
    editable = c(F, T, T, F, F, F),
    html = rep(F, 6),
    stringsAsFactors = F
  ),
  fixed.cols = 3,
  sort = c("SortString"),
  key = "MeterID",
  related.label.tables = c("meter.labels"),
  related.selectors = c(),
  related.processing.table = F
)

manage.meters = tabPanel(
  "Meters",
  rHandsontableOutput("meters"),
  actionButton("save.meters", label = "Save changes")
)

#### Pitches ####

manage.pitches.info = list(
  table = "wsdb.pitches",
  columns = data.frame(
    column.name = c("PitchID", "PitchName", "Created", "Updated"),
    key.table = rep(NA_character_, 4),
    key.label = rep(NA_character_, 4),
    width = c(100, 100, 100, 100),
    type = c("numeric", "text", "date", "date"),
    editable = c(F, T, F, F),
    html = rep(F, 4),
    stringsAsFactors = F
  ),
  fixed.cols = 1,
  sort = c("PitchName"),
  key = "PitchID",
  related.label.tables = c("pitch.labels", "key.signature.labels"),
  related.selectors = c(),
  related.processing.table = F
)

manage.pitches = tabPanel(
  "Pitches",
  rHandsontableOutput("pitches"),
  actionButton("save.pitches", label = "Save changes")
)

#### Accidentals ####

manage.accidentals.info = list(
  table = "wsdb.accidentals",
  columns = data.frame(
    column.name = c("AccidentalID", "AccidentalSymbol", "Created", "Updated"),
    key.table = rep(NA_character_, 4),
    key.label = rep(NA_character_, 4),
    width = c(100, 150, 100, 100),
    type = c("numeric", "text", "date", "date"),
    editable = c(F, T, F, F),
    html = rep(F, 4),
    stringsAsFactors = F
  ),
  fixed.cols = 1,
  sort = c("AccidentalID"),
  key = "AccidentalID",
  related.label.tables = c("accidental.labels", "key.signature.labels"),
  related.selectors = c(),
  related.processing.table = F
)

manage.accidentals = tabPanel(
  "Accidentals",
  rHandsontableOutput("accidentals"),
  actionButton("save.accidentals", label = "Save changes")
)

#### Modes ####

manage.modes.info = list(
  table = "wsdb.modes",
  columns = data.frame(
    column.name = c("ModeID", "ModeName", "Created", "Updated"),
    key.table = rep(NA_character_, 4),
    key.label = rep(NA_character_, 4),
    width = c(100, 100, 100, 100),
    type = c("numeric", "text", "date", "date"),
    editable = c(F, T, F, F),
    html = rep(F, 4),
    stringsAsFactors = F
  ),
  fixed.cols = 1,
  sort = c("ModeID"),
  key = "ModeID",
  related.label.tables = c("mode.labels", "key.signature.labels"),
  related.selectors = c(),
  related.processing.table = F
)

manage.modes = tabPanel(
  "Modes",
  rHandsontableOutput("modes"),
  actionButton("save.modes", label = "Save changes")
)

#### Key signatures ####

manage.key.signatures.info = list(
  table = "wsdb.keysignatures",
  columns = data.frame(
    column.name = c("KeySignatureID", "PitchID", "AccidentalID", "ModeID",
                    "Created", "Updated"),
    key.table = c(NA, "pitch.labels", "accidental.labels", "mode.labels", NA,
                  NA),
    key.label = c(NA, "PitchLabel", "AccidentalLabel", "ModeLabel", NA, NA),
    width = c(150, 150, 150, 150, 100, 100),
    type = c("numeric", "dropdown", "dropdown", "dropdown", "date",
             "date"),
    editable = c(F, T, T, T, F, F),
    html = rep(F, 6),
    stringsAsFactors = F
  ),
  fixed.cols = 4,
  sort = c("PitchID", "AccidentalID", "ModeID"),
  key = "KeySignatureID",
  related.label.tables = c(),
  related.selectors = c(),
  related.processing.table = T
)

manage.key.signatures = tabPanel(
  "Key signatures",
  rHandsontableOutput("key.signatures"),
  actionButton("save.key.signatures", label = "Save changes")
)

#### Time signatures ####

manage.time.signatures.info = list(
  table = "wsdb.timesignatures",
  columns = data.frame(
    column.name = c("TimeSignatureID", "TimeSignatureBeat",
                    "TimeSignatureMeasure", "Created", "Updated"),
    key.table = rep(NA_character_, 5),
    key.label = rep(NA_character_, 5),
    width = c(150, 150, 150, 100, 100),
    type = c("numeric", "numeric", "numeric", "date", "date"),
    editable = c(F, T, T, F, F),
    html = rep(F, 5),
    stringsAsFactors = F
  ),
  fixed.cols = 3,
  sort = c("TimeSignatureMeasure", "TimeSignatureBeat"),
  key = "TimeSignatureID",
  related.label.tables = c(),
  related.selectors = c(),
  related.processing.table = T
)

manage.time.signatures = tabPanel(
  "Time signatures",
  rHandsontableOutput("time.signatures"),
  actionButton("save.time.signatures", label = "Save changes")
)

#### Arrangement types ####

manage.arrangement.types.info = list(
  table = "wsdb.arrangementtypes",
  columns = data.frame(
    column.name = c("ArrangementTypeID", "ArrangementType", "Created", "Updated"),
    key.table = rep(NA_character_, 4),
    key.label = rep(NA_character_, 4),
    width = c(100, 300, 100, 100),
    type = c("numeric", "text", "date", "date"),
    editable = c(F, T, F, F),
    html = rep(F, 4),
    stringsAsFactors = F
  ),
  fixed.cols = 1,
  sort = c("ArrangementType"),
  key = "ArrangementTypeID",
  related.label.tables = c("arrangement.type.labels"),
  related.selectors = c(),
  related.processing.table = F
)

manage.arrangement.types = tabPanel(
  "Arrangement types",
  rHandsontableOutput("arrangement.types"),
  actionButton("save.arrangement.types", label = "Save changes")
)

#### Topics ####

manage.topics.info = list(
  table = "wsdb.topics",
  columns = data.frame(
    column.name = c("TopicID", "TopicName", "Created", "Updated"),
    key.table = rep(NA_character_, 4),
    key.label = rep(NA_character_, 4),
    width = c(100, 300, 100, 100),
    type = c("numeric", "text", "date", "date"),
    editable = c(F, T, F, F),
    html = rep(F, 4),
    stringsAsFactors = F
  ),
  fixed.cols = 1,
  sort = c("TopicName"),
  key = "TopicID",
  related.label.tables = c("topic.labels"),
  related.selectors = c(),
  related.processing.table = F
)

manage.topics = tabPanel(
  "Topics",
  rHandsontableOutput("topics"),
  actionButton("save.topics", label = "Save changes")
)

#### Songbooks ####

manage.songbooks.info = list(
  table = "wsdb.songbooks",
  columns = data.frame(
    column.name = c("SongbookID", "SongbookName", "SongbookAbbreviation",
                    "IncludeInSearch", "Created", "Updated"),
    key.table = rep(NA_character_, 6),
    key.label = rep(NA_character_, 6),
    width = c(100, 500, 100, 100, 100, 100),
    type = c("numeric", "text", "text", "text", "date", "date"),
    editable = c(F, T, T, T, F, F),
    html = rep(F, 6),
    stringsAsFactors = F
  ),
  fixed.cols = 1,
  sort = c("SongbookName"),
  key = "SongbookID",
  related.label.tables = c(),
  related.selectors = c("process.songbook.id"),
  related.processing.table = T
)

manage.songbooks = tabPanel(
  "Songbooks",
  rHandsontableOutput("songbooks"),
  actionButton("save.songbooks", label = "Save changes")
)

#### Songbook volumes ####

manage.songbook.volumes.info = list(
  table = "wsdb.songbookvolumes",
  columns = data.frame(
    column.name = c("SongbookVolumeID", "SongbookVolume", "Created", "Updated"),
    key.table = rep(NA_character_, 4),
    key.label = rep(NA_character_, 4),
    width = c(100, 400, 100, 100),
    type = c("numeric", "text", "date", "date"),
    editable = c(F, T, F, F),
    html = rep(F, 4),
    stringsAsFactors = F
  ),
  fixed.cols = 1,
  sort = c("SongbookVolume"),
  key = "SongbookVolumeID",
  related.label.tables = c(),
  related.selectors = c("process.songbook.volume.id"),
  related.processing.table = T
)

manage.songbook.volumes = tabPanel(
  "Songbook volumes",
  rHandsontableOutput("songbook.volumes"),
  actionButton("save.songbook.volumes", label = "Save changes")
)

#### Combined info ####

# List with everything
reference.table.info = list(
  genders = manage.genders.info,
  artists = manage.artists.info,
  copyright.administrators = manage.copyright.administrators.info,
  copyright.holders = manage.copyright.holders.info,
  languages = manage.languages.info,
  books.of.the.bible = manage.books.of.the.bible.info,
  scripture.references = manage.scripture.references.info,
  meters = manage.meters.info,
  pitches = manage.pitches.info,
  accidentals = manage.accidentals.info,
  modes = manage.modes.info,
  key.signatures = manage.key.signatures.info,
  time.signatures = manage.time.signatures.info,
  topics = manage.topics.info,
  songbooks = manage.songbooks.info,
  songbook.volumes = manage.songbook.volumes.info
)

# Use data on columns to create SELECT, UPDATE, INSERT, and DELETE statements
for(reference.table in names(reference.table.info)) {
  reference.info = reference.table.info[[reference.table]]
  sql.cols = paste(reference.info$columns$column.name, collapse = ", ")
  sql = paste("SELECT ", sql.cols, " FROM ", reference.info$table, " ORDER BY ",
              paste(reference.info$sort, collapse = ", "), sep = "")
  reference.table.info[[reference.table]]$populate.sql = sql
  sql = reference.info$columns %>%
    filter(editable) %>%
    mutate(update = paste(column.name, " = {", column.name, "}", sep = "")) %>%
    pull(update) %>%
    paste(collapse = ", ") %>%
    paste("UPDATE ", reference.info$table, " SET ", ., " WHERE ",
          reference.info$key, " = {", reference.info$key, "}", sep = "")
  reference.table.info[[reference.table]]$update.sql = sql
  sql = paste(
    "INSERT INTO ", reference.info$table, "(",
    reference.info$columns %>%
      filter(editable) %>%
      pull(column.name) %>%
      paste(collapse = ", "),
    ") VALUES (",
    reference.info$columns %>%
      filter(editable) %>%
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
                                   manage.genders,
                                   manage.artists,
                                   manage.copyright.administrators,
                                   manage.copyright.holders,
                                   manage.languages,
                                   manage.books.of.the.bible,
                                   manage.scripture.references,
                                   manage.meters,
                                   manage.pitches,
                                   manage.accidentals,
                                   manage.modes,
                                   manage.key.signatures,
                                   manage.time.signatures,
                                   manage.topics,
                                   manage.songbooks,
                                   manage.songbook.volumes,
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
        return(temp.df)
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
    
    # Create sql to update changed rows
    sql = temp.df %>%
      filter(.data[[reference.info$key]] %in% reactive.reference.changes[[reference.table]]$edit) %>%
      glue_data_sql(reference.info$update.sql, .con = db.con)
    
    # Attempt to update changed rows
    for(s in sql) {
      tryCatch(
        {
          dbGetQuery(db.con, s)
          reactive.reference.changes[[reference.table]]$edit = c()
          show.changes.saved(T)
        },
        error = function(err) {
          print(err)
          show.changes.saved(F)
        }
      )
    }
    
  }
  
  # If there were inserts, issue an INSERT statement
  if(reactive.reference.changes[[reference.table]]$insert) {
    
    # Attempt to insert new rows
    if(any(is.na(temp.df[[reference.info$key]]))) {
      sql = temp.df %>%
        filter(is.na(.data[[reference.info$key]])) %>%
        glue_data_sql(reference.info$insert.sql, .con = db.con)
      sql = gsub("NULL", "DEFAULT", sql)
      for(s in sql) {
        tryCatch(
          {
            dbGetQuery(db.con, s)
            reactive.reference.changes[[reference.table]]$insert = F
            show.changes.saved(T)
          },
          error = function(err) {
            print(err)
            show.changes.saved(F)
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
        show.changes.saved(T)
      },
      error = function(err) {
        print(err)
        show.changes.saved(F)
      }
    )
    
  }
  
}
