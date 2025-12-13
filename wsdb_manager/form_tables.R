#### Songs ####

manage.songs.info = list(
  table = "wsdb.songs",
  select.label = "Choose song:",
  columns = data.frame(
    column.name = c("SongID", "SongName", "SongDisambiguator", "TopicID",
                    "SongTypeID", "SongTempoID", "PsalmNumberParaphrase",
                    "PsalmNumberQuotation", "PsalmNumberReference", "Created",
                    "Updated"),
    form.label = c("Song ID", "Song name", "Song disambiguator", "Topics",
                   "Song type", "Song tempo", "Psalm numbers (paraphrase)",
                   "Psalm numbers (quotation)", "Psalm number (reference)",
                   "Date created", "Date updated"),
    key.table = c(NA, NA, NA, "topic.labels", "song.type.labels",
                  "song.tempo.labels", "psalm.number.labels",
                  "psalm.number.labels", "psalm.number.labels", NA, NA),
    key.label = c(NA, NA, NA, "TopicLabel", "SongTypeLabel", "SongTempoLabel",
                  "PsalmNumberLabel", "PsalmNumberLabel", "PsalmNumberLabel",
                  NA, NA),
    multi.table = c(NA, NA, NA, "wsdb.songs_topics", NA, NA,
                    "(SELECT PsalmSongID AS PsalmSongIDParaphrase, SongID,PsalmNumber AS PsalmNumberParaphrase
                      FROM wsdb.psalmsongs
                      WHERE PsalmSongTypeID = 1) psalmsongs_paraphrase",
                    "(SELECT PsalmSongID AS PsalmSongIDQuotation, SongID,PsalmNumber AS PsalmNumberQuotation
                      FROM wsdb.psalmsongs
                      WHERE PsalmSongTypeID = 2) psalmsongs_quotation",
                    "(SELECT PsalmSongID AS PsalmSongIDReference, SongID,PsalmNumber AS PsalmNumberReference
                      FROM wsdb.psalmsongs
                      WHERE PsalmSongTypeID = 3) psalmsongs_reference",
                    NA, NA),
    type = c("numeric", "text", "text", "text", "text", "text", "text", "text",
             "text", "date", "date"),
    editable = c(F, T, T, T, T, T, T, T, T, F, F),
    stringsAsFactors = F
  ),
  sort = c("SongName", "SongDisambiguator"),
  key = "SongID",
  related.label.tables = c("song.labels"),
  related.selectors = c("manage.songs.id", "alt.by.song.id"),
  related.processing.table = F
)

#### Lyrics ####

manage.lyrics.info = list(
  table = "wsdb.lyrics",
  select.label = "Choose lyrics:",
  columns = data.frame(
    column.name = c("LyricsID", "FirstLine", "RefrainFirstLine", "ArtistID",
                    "TranslatedFromID", "CopyrightHolderID", "CopyrightYear",
                    "LanguageID", "MeterID", "ScriptureReferenceID", "FileName",
                    "Created", "Updated", "LyricsHTML"),
    form.label = c("Lyrics ID", "First line", "Refrain first line", "Artists",
                   "Translated from", "Copyright holders", "Year", "Language",
                   "Meters", "Scripture references", "File name",
                   "Date created", "Date updated", "Full lyrics"),
    key.table = c(NA, NA, NA, "artist.labels", "lyrics.labels",
                  "copyright.holder.labels", NA, "language.labels",
                  "meter.labels", "scripture.reference.labels", NA, NA, NA, NA),
    key.label = c(NA, NA, NA, "ArtistLabel", "LyricsLabel",
                  "CopyrightHolderLabel", NA, "LanguageLabel", "MeterLabel",
                  "ScriptureReferenceLabel", NA, NA, NA, NA),
    multi.table = c(NA, NA, NA, "wsdb.lyrics_artists",
                    "wsdb.lyrics_translations", "wsdb.lyrics_copyrightholders",
                    NA, NA, "wsdb.lyrics_meters",
                    "wsdb.lyrics_scripturereferences", NA, NA, NA, NA),
    type = c("numeric", "text", "text", "text", "text", "text", "numeric",
             "text", "text", "text", "file", "date", "date", "html"),
    editable = c(F, T, T, T, T, T, T, T, T, T, F, F, F, F),
    stringsAsFactors = F
  ),
  sort = c("FirstLine", "RefrainFirstLine"),
  key = "LyricsID",
  related.label.tables = c("lyrics.labels"),
  related.selectors = c("manage.lyrics.id"),
  related.processing.table = F
)

#### Tunes ####

manage.tunes.info = list(
  table = "wsdb.tunes",
  select.label = "Choose tune:",
  columns = data.frame(
    column.name = c("TuneID", "TuneName", "ArtistID", "CopyrightHolderID",
                    "CopyrightYear", "MeterID", "RealTuneName",
                    "SongID", "CanonicalSongName", "Created", "Updated"),
    form.label = c("Tune ID", "Tune name", "Artists", "Copyright holders",
                   "Year", "Meters", "Real tune name?", "CanonicalSongs",
                   "Canonical song name", "Date created", "Date updated"),
    key.table = c(NA, NA, "artist.labels", "copyright.holder.labels", NA,
                  "meter.labels", NA, "song.labels", NA, NA, NA),
    key.label = c(NA, NA, "ArtistLabel", "CopyrightHolderLabel", NA,
                  "MeterLabel", NA, "SongLabel", NA, NA, NA),
    multi.table = c(NA, NA, "wsdb.tunes_artists", "wsdb.tunes_copyrightholders",
                    NA, "wsdb.tunes_meters", NA, "wsdb.tunes_canonicalsongs",
                    NA, NA, NA),
    type = c("numeric", "text", "text", "text", "numeric", "text", "checkbox",
             "text", "text", "date", "date"),
    editable = c(F, T, T, T, T, T, T, T, T, F, F),
    stringsAsFactors = F
  ),
  sort = c("TuneName"),
  key = "TuneID",
  related.label.tables = c("tune.labels"),
  related.selectors = c("manage.tunes.id", "alt.by.tune.id"),
  related.processing.table = F
)

#### Arrangements ####

manage.arrangements.info = list(
  table = "wsdb.arrangements",
  select.label = "Choose arrangement:",
  columns = data.frame(
    column.name = c("ArrangementID", "ArrangementName", "TuneID", "ArtistID",
                    "ArrangementTypeID", "CopyrightHolderID", "CopyrightYear",
                    "Created", "Updated"),
    form.label = c("Arrangement ID", "Arrangement name", "Tunes", "Artists",
                   "Arrangement types", "Copyright holders", "Year",
                   "Date created", "Date updated"),
    key.table = c(NA, NA, "tune.labels", "artist.labels",
                  "arrangement.type.labels", "copyright.holder.labels", NA, NA,
                  NA),
    key.label = c(NA, NA, "TuneLabel", "ArtistLabel", "ArrangementTypeLabel",
                  "CopyrightHolderLabel", NA, NA, NA),
    multi.table = c(NA, NA, "wsdb.arrangements_tunes",
                    "wsdb.arrangements_artists",
                    "wsdb.arrangements_arrangementtypes",
                    "wsdb.arrangements_copyrightholders", NA, NA, NA),
    type = c("numeric", "text", "text", "text", "text", "text", "numeric",
             "date", "date"),
    editable = c(F, T, T, T, T, T, T, F, F),
    stringsAsFactors = F
  ),
  sort = c("ArrangementName"),
  key = "ArrangementID",
  related.label.tables = c("arrangement.labels"),
  related.selectors = c("manage.arrangements.id"),
  related.processing.table = F
)

#### Song instances ####

manage.song.instances.info = list(
  table = "wsdb.songinstances",
  select.label = "Choose song instance:",
  columns = data.frame(
    column.name = c("SongInstanceID", "SongInstance", "LyricsID", "TuneID",
                    "ArrangementID", "SongID", "KeySignatureID",
                    "TimeSignatureID", "Created", "Updated"),
    form.label = c("Song instance ID", "Song instance name", "Lyrics", "Tunes",
                   "Arrangement", "Song", "Key signatures", "Time signatures",
                   "Date created", "Date updated"),
    key.table = c(NA, NA, "lyrics.labels", "tune.labels", "arrangement.labels",
                  "song.labels", "key.signature.labels",
                  "time.signature.labels", NA, NA),
    key.label = c(NA, NA, "LyricsLabel", "TuneLabel", "ArrangementLabel",
                  "SongLabel", "KeySignatureLabel", "TimeSignatureLabel", NA,
                  NA),
    multi.table = c(NA, NA, "wsdb.songinstances_lyrics",
                    "wsdb.songinstances_tunes", NA, NA,
                    "wsdb.songinstances_keysignatures",
                    "wsdb.songinstances_timesignatures", NA, NA),
    type = c("numeric", "text", "text", "text", "text", "text", "text", "text",
             "date", "date"),
    editable = c(F, T, T, T, T, T, T, T, F, F),
    stringsAsFactors = F
  ),
  sort = c("SongInstance", "SongInstanceID"),
  key = "SongInstanceID",
  related.label.tables = c("song.instance.labels"),
  related.selectors = c("manage.song.instances.id"),
  related.processing.table = F,
  extra.delete.sql = "DELETE FROM wsdb.prettyscripturelists WHERE SongInstanceID = {SongInstanceID}"
)

#### Song instances ####

manage.song.instances.info = list(
  table = "wsdb.songinstances",
  select.label = "Choose song instance:",
  columns = data.frame(
    column.name = c("SongInstanceID", "SongInstance", "LyricsID", "TuneID",
                    "ArrangementID", "SongID", "KeySignatureID",
                    "TimeSignatureID", "Created", "Updated"),
    form.label = c("Song instance ID", "Song instance name", "Lyrics", "Tunes",
                   "Arrangement", "Song", "Key signatures", "Time signatures",
                   "Date created", "Date updated"),
    key.table = c(NA, NA, "lyrics.labels", "tune.labels", "arrangement.labels",
                  "song.labels", "key.signature.labels",
                  "time.signature.labels", NA, NA),
    key.label = c(NA, NA, "LyricsLabel", "TuneLabel", "ArrangementLabel",
                  "SongLabel", "KeySignatureLabel", "TimeSignatureLabel", NA,
                  NA),
    multi.table = c(NA, NA, "wsdb.songinstances_lyrics",
                    "wsdb.songinstances_tunes", NA, NA,
                    "wsdb.songinstances_keysignatures",
                    "wsdb.songinstances_timesignatures", NA, NA),
    type = c("numeric", "text", "text", "text", "text", "text", "text", "text",
             "date", "date"),
    editable = c(F, T, T, T, T, T, T, T, F, F),
    stringsAsFactors = F
  ),
  sort = c("SongInstance", "SongInstanceID"),
  key = "SongInstanceID",
  related.label.tables = c("song.instance.labels"),
  related.selectors = c("manage.song.instances.id"),
  related.processing.table = F,
  extra.delete.sql = "DELETE FROM wsdb.prettyscripturelists WHERE SongInstanceID = {SongInstanceID}"
)

#### Metrical psalms ####

manage.metrical.psalms.info = list(
  table = "wsdb.metricalpsalms",
  select.label = "Choose metrical psalm:",
  columns = data.frame(
    column.name = c("MetricalPsalmID", "PsalmNumber", "LyricsID", "Created",
                    "Updated"),
    form.label = c("Metrical psalm ID", "Psalm number", "Lyrics",
                   "Date created", "Date updated"),
    key.table = c(NA, "psalm.number.labels", "lyrics.labels", NA, NA),
    key.label = c(NA, "PsalmNumberLabel", "LyricsLabel", NA, NA),
    multi.table = c(NA, NA, "wsdb.metricalpsalms_lyrics", NA, NA),
    type = c("numeric", "text", "text", "date", "date"),
    editable = c(F, T, T, F, F),
    stringsAsFactors = F
  ),
  sort = c("PsalmNumber", "MetricalPsalmID"),
  key = "MetricalPsalmID",
  related.label.tables = c("metrical.psalm.labels"),
  related.selectors = c("manage.metrical.psalms.id"),
  related.processing.table = F,
  extra.delete.sql = "DELETE FROM wsdb.metricalpsalms_prettyscripturelists WHERE MetricalPsalmID = {MetricalPsalmID}"
)

#### Combined info ####

# List with everything
form.table.info = list(
  songs = manage.songs.info,
  lyrics = manage.lyrics.info,
  tunes = manage.tunes.info,
  arrangements = manage.arrangements.info,
  song.instances = manage.song.instances.info,
  metrical.psalms = manage.metrical.psalms.info
)

# Assign an element ID to each column in each table
for(form.table in names(form.table.info)) {
  form.table.info[[form.table]]$columns = form.table.info[[form.table]]$columns %>%
    mutate(element.id = paste(form.table, column.name, sep = "."))
}

# Use data on columns to create SELECT, UPDATE, INSERT, and DELETE statements
for(form.table in names(form.table.info)) {

  # Get info for this table
  form.info = form.table.info[[form.table]]

  # Create SELECT statement
  sql.cols = paste(form.info$columns$column.name[is.na(form.info$columns$multi.table)],
                   collapse = ", ")
  sql = paste("SELECT ", sql.cols, " FROM ", form.info$table, " ORDER BY ",
              paste(form.info$sort, collapse = ", "), sep = "")
  form.table.info[[form.table]]$populate.sql = sql
  
  # Create SELECT statements for many-to-many relationships
  form.table.info[[form.table]]$populate.multi.sql = list()
  for(i in 2:nrow(form.info$columns)) {
    col.info = form.info$columns[i,]
    if(!is.na(col.info$multi.table)) {
      sql = paste("SELECT ", form.info$key, ", ", col.info$column.name,
                  " FROM ", col.info$multi.table, sep = "")
      form.table.info[[form.table]]$populate.multi.sql[[col.info$element.id]] = sql
    }
  }

  # Create UPDATE statement for base table
  sql = form.info$columns %>%
    filter(editable | type == "file", is.na(multi.table)) %>%
    mutate(update = paste(column.name, " = {", column.name, "}", sep = "")) %>%
    pull(update) %>%
    paste(collapse = ", ") %>%
    paste("UPDATE ", form.info$table, " SET ", ., " WHERE ", form.info$key,
          " = {", form.info$key, "}", sep = "")
  form.table.info[[form.table]]$update.base.sql = sql
  
  # Create INSERT/DELETE statements for many-to-many tables
  form.table.info[[form.table]]$update.multi.sql = lapply(
    set_names(form.info$columns$column.name[!is.na(form.info$columns$multi.table)]),
    function(column.name) {
      col.info = form.info$columns[form.info$columns$column.name == column.name,]
      if(grepl("psalmsongs", col.info$multi.table)) {
        delete.sql = paste("DELETE FROM wsdb.psalmsongs WHERE SongID = {SongID} AND PsalmSongTypeID = ",
                           gsub("^.*PsalmSongTypeID = ([123]).*$", "\\1",
                                col.info$multi.table), sep = "")
        insert.sql = paste("INSERT INTO wsdb.psalmsongs (PsalmNumber, SongID, PsalmSongTypeID) VALUES ({",
                           gsub("^.*(PsalmNumber[A-Za-z]+).*$", "\\1",
                                col.info$multi.table),
                           "}, {SongID}, ",
                           gsub("^.*PsalmSongTypeID = ([123]).*$", "\\1",
                                col.info$multi.table), ")", sep = "")
      } else {
        delete.sql = paste("DELETE FROM ", col.info$multi.table, " WHERE ",
                           form.info$key, " = {", form.info$key, "}", sep = "")
        insert.sql = paste("INSERT INTO ", col.info$multi.table, " (",
                           form.info$key, ", ", col.info$column.name,
                           ") VALUES ({", form.info$key, "}, {",
                           col.info$column.name, "})", sep = "")
      }
      list(delete = delete.sql, insert = insert.sql)
    }
  )
  
  # Create DELETE statement
  sql = paste("DELETE FROM ", form.info$table, " WHERE ",
              form.info$key, " = {", form.info$key, "}", sep = "")
  form.table.info[[form.table]]$delete.sql = sql
  
  # Create INSERT statement
  sql = paste(
    "INSERT INTO ", form.info$table, "(",
    form.info$columns %>%
      filter(editable | type == "file", is.na(multi.table)) %>%
      pull(column.name) %>%
      paste(collapse = ", "),
    ") VALUES (",
    form.info$columns %>%
      filter(editable | type == "file", is.na(multi.table)) %>%
      mutate(column.name = paste("{", column.name, "}", sep = "")) %>%
      pull(column.name) %>%
      paste(collapse = ", "),
    ")", sep = ""
  )
  form.table.info[[form.table]]$insert.sql = sql

  # Create management page elements
  element.list = list()
  element.id = paste("manage", form.table, "id", sep = ".")
  element.list[[element.id]] = selectizeInput(element.id,
                                              form.info$select.label,
                                              choices = list(),
                                              selected = character(0),
                                              width = "100%",
                                              options = list(maxOptions = 100000,
                                                             render = I(selectize.html.render)))
  element.id = paste("save", form.table, sep = ".")
  element.list[[element.id]] = actionButton(element.id, label = "Save changes")
  element.id = paste("delete", form.table, sep = ".")
  element.list[[element.id]] = actionButton(element.id, label = "Delete record")
  for(i in 2:nrow(form.info$columns)) {
    col.info = form.info$columns[i,]
    new.element = div(col.info$column.name)
    if(is.na(col.info$key.table)) {
      if(col.info$editable) {
        if(col.info$type == "checkbox") {
          new.element = checkboxInput(col.info$element.id, col.info$form.label)
        } else {
          new.element = textInput(col.info$element.id, col.info$form.label,
                                  width = "100%")
        }
      } else {
        new.element = htmlOutput(col.info$element.id, width = "100%")
      }
    } else {
      new.element = selectizeInput(col.info$element.id, col.info$form.label,
                                   choices = list(), selected = c(),
                                   multiple = !is.na(col.info$multi.table),
                                   width = "100%",
                                   options = list(maxOptions = 100000,
                                                  render = I(selectize.html.render)))
    }
    element.list[[col.info$element.id]] = new.element
    if(col.info$type == "file") {
      element.list[["lyrics.new.file"]] = fileInput("lyrics.new.file", "",
                                                    accept = ".xml")
    }
  }
  form.table.info[[form.table]]$form.elements = element.list

}

#### Management pages ####

# Songs
form.table.info$songs$tab.panel = tabPanel(
  "Manage songs",
  fluidRow(
    column(5, form.table.info$songs$form.elements$manage.songs.id),
    column(4, form.table.info$songs$form.elements$save.songs,
           form.table.info$songs$form.elements$delete.songs,
           align = "center"),
    column(3, column(6, form.table.info$songs$form.elements$songs.Created),
           column(6, form.table.info$songs$form.elements$songs.Updated),
           align = "center")
  ),
  fluidRow(
    column(6, form.table.info$songs$form.elements$songs.SongName),
    column(6, form.table.info$songs$form.elements$songs.SongDisambiguator)
  ),
  fluidRow(
    column(6, form.table.info$songs$form.elements$songs.TopicID),
    column(3, form.table.info$songs$form.elements$songs.SongTypeID),
    column(3, form.table.info$songs$form.elements$songs.SongTempoID)
  ),
  fluidRow(
    column(4, form.table.info$songs$form.elements$songs.PsalmNumberParaphrase),
    column(4, form.table.info$songs$form.elements$songs.PsalmNumberQuotation),
    column(4, form.table.info$songs$form.elements$songs.PsalmNumberReference)
  )
)

# Lyrics
form.table.info$lyrics$tab.panel = tabPanel(
  "Manage lyrics",
  fluidRow(
    column(5, form.table.info$lyrics$form.elements$manage.lyrics.id),
    column(4, form.table.info$lyrics$form.elements$save.lyrics,
           form.table.info$lyrics$form.elements$delete.lyrics,
           align = "center"),
    column(3, column(6, form.table.info$lyrics$form.elements$lyrics.Created),
           column(6, form.table.info$lyrics$form.elements$lyrics.Updated),
           align = "center")
  ),
  fluidRow(
    column(4, form.table.info$lyrics$form.elements$lyrics.FirstLine),
    column(4, form.table.info$lyrics$form.elements$lyrics.RefrainFirstLine),
    column(4,
           fluidRow(
             form.table.info$lyrics$form.elements$lyrics.FileName,
             form.table.info$lyrics$form.elements$lyrics.new.file
           ))
  ),
  fluidRow(
    column(5, form.table.info$lyrics$form.elements$lyrics.ArtistID),
    column(3, form.table.info$lyrics$form.elements$lyrics.LanguageID),
    column(4, form.table.info$lyrics$form.elements$lyrics.TranslatedFromID)
  ),
  fluidRow(
    column(9, form.table.info$lyrics$form.elements$lyrics.CopyrightHolderID),
    column(3, form.table.info$lyrics$form.elements$lyrics.CopyrightYear)
  ),
  fluidRow(
    column(6, form.table.info$lyrics$form.elements$lyrics.MeterID),
    column(6, form.table.info$lyrics$form.elements$lyrics.ScriptureReferenceID)
  ),
  fluidRow(column(12, form.table.info$lyrics$form.elements$lyrics.LyricsHTML))
)

# Tunes
form.table.info$tunes$tab.panel = tabPanel(
  "Manage tunes",
  fluidRow(
    column(5, form.table.info$tunes$form.elements$manage.tunes.id),
    column(4, form.table.info$tunes$form.elements$save.tunes,
           form.table.info$tunes$form.elements$delete.tunes,
           align = "center"),
    column(3, column(6, form.table.info$tunes$form.elements$tunes.Created),
           column(6, form.table.info$tunes$form.elements$tunes.Updated),
           align = "center")
  ),
  fluidRow(
    column(4, form.table.info$tunes$form.elements$tunes.TuneName),
    column(2, form.table.info$tunes$form.elements$tunes.RealTuneName),
    column(6, form.table.info$tunes$form.elements$tunes.MeterID)
  ),
  fluidRow(
    column(5, form.table.info$tunes$form.elements$tunes.ArtistID),
    column(4, form.table.info$tunes$form.elements$tunes.SongID),
    column(3, form.table.info$tunes$form.elements$tunes.CanonicalSongName)
  ),
  fluidRow(
    column(9, form.table.info$tunes$form.elements$tunes.CopyrightHolderID),
    column(3, form.table.info$tunes$form.elements$tunes.CopyrightYear)
  )
)

# Arrangements
form.table.info$arrangements$tab.panel = tabPanel(
  "Manage arrangements",
  fluidRow(
    column(5, form.table.info$arrangements$form.elements$manage.arrangements.id),
    column(4, form.table.info$arrangements$form.elements$save.arrangements,
           form.table.info$arrangements$form.elements$delete.arrangements,
           align = "center"),
    column(3, column(6, form.table.info$arrangements$form.elements$arrangements.Created),
           column(6, form.table.info$arrangements$form.elements$arrangements.Updated),
           align = "center")
  ),
  fluidRow(
    column(4, form.table.info$arrangements$form.elements$arrangements.ArrangementName),
    column(3, form.table.info$arrangements$form.elements$arrangements.TuneID),
    column(5, form.table.info$arrangements$form.elements$arrangements.ArrangementTypeID)
  ),
  fluidRow(
    column(5, form.table.info$arrangements$form.elements$arrangements.ArtistID),
    column(4, form.table.info$arrangements$form.elements$arrangements.CopyrightHolderID),
    column(3, form.table.info$arrangements$form.elements$arrangements.CopyrightYear)
  )
)

# Song instances
form.table.info$song.instances$tab.panel = tabPanel(
  "Manage song instances",
  fluidRow(
    column(5, form.table.info$song.instances$form.elements$manage.song.instances.id),
    column(4, form.table.info$song.instances$form.elements$save.song.instances,
           form.table.info$song.instances$form.elements$delete.song.instances,
           align = "center"),
    column(3, column(6, form.table.info$song.instances$form.elements$song.instances.Created),
           column(6, form.table.info$song.instances$form.elements$song.instances.Updated),
           align = "center")
  ),
  fluidRow(
    column(6, form.table.info$song.instances$form.elements$song.instances.SongInstance),
    column(6, form.table.info$song.instances$form.elements$song.instances.SongID)
  ),
  fluidRow(
    column(5, form.table.info$song.instances$form.elements$song.instances.LyricsID),
    column(3, form.table.info$song.instances$form.elements$song.instances.TuneID),
    column(4, form.table.info$song.instances$form.elements$song.instances.ArrangementID)
  ),
  fluidRow(
    column(6, form.table.info$song.instances$form.elements$song.instances.KeySignatureID),
    column(6, form.table.info$song.instances$form.elements$song.instances.TimeSignatureID)
  )
)

# Metrical psalms
form.table.info$metrical.psalms$tab.panel = tabPanel(
  "Manage metrical psalms",
  fluidRow(
    column(5, form.table.info$metrical.psalms$form.elements$manage.metrical.psalms.id),
    column(4, form.table.info$metrical.psalms$form.elements$save.metrical.psalms,
           form.table.info$metrical.psalms$form.elements$delete.metrical.psalms,
           align = "center"),
    column(3, column(6, form.table.info$metrical.psalms$form.elements$metrical.psalms.Created),
           column(6, form.table.info$metrical.psalms$form.elements$metrical.psalms.Updated),
           align = "center")
  ),
  fluidRow(
    column(3, form.table.info$metrical.psalms$form.elements$metrical.psalms.PsalmNumber),
    column(9, form.table.info$metrical.psalms$form.elements$metrical.psalms.LyricsID)
  )
)

#### Useful functions ####

# Populate the table
populate.form.table = function(form.table, db.con, reactive.label.tables) {
  if(!is.null(db.con)) {
    form.info = form.table.info[[form.table]]
    tryCatch(
      {
        temp.df = dbGetQuery(db.con, form.info$populate.sql)
        for(i in 1:nrow(form.info$columns)) {
          col.info = form.info$columns[i,]
          if(!is.na(col.info$key.table)) {
            if(is.na(col.info$multi.table)) {
              temp.df = temp.df %>%
                left_join(reactive.label.tables[[col.info$key.table]],
                          by = col.info$column.name)
            } else {
              multi.df = dbGetQuery(db.con,
                                    form.info$populate.multi.sql[[col.info$element.id]])
              temp.df = temp.df %>%
                nest_join(multi.df, by = form.info$key,
                          name = col.info$column.name)
            }
          }
          if(col.info$type == "checkbox") {
            temp.df[,col.info$column.name] = temp.df[,col.info$column.name] == 1
          }
        }
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

# Get the row of data based on the user's selection
get.form.row = function(form.table, reactive.form.tables, reactive.id) {
  row = reactive.form.tables[[form.table]]
  id = reactive.id
  if(is.null(id)) {
    id = -99
  }
  if(!is.null(row)) {
    row = row %>%
      filter(.data[[form.table.info[[form.table]]$key]] == id)
    if(nrow(row) == 0) {
      row = row %>% add_row()
    }
  }
  return(row)
}

# Populate the management page based on the selected row
populate.form.page = function(form.table, row, session) {
  for(i in 2:nrow(form.table.info[[form.table]]$columns)) {
    col.info = form.table.info[[form.table]]$columns[i,]
    if(is.na(col.info$key.table)) {
      if(col.info$editable) {
        if(col.info$type == "checkbox") {
          updateCheckboxInput(session, col.info$element.id,
                              value = row[[col.info$column.name]])
        } else {
          updateTextInput(session, col.info$element.id,
                          value = row[[col.info$column.name]])
        }
      }
    } else {
      if(is.na(col.info$multi.table)) {
        updateSelectizeInput(session, col.info$element.id,
                             selected = row[[col.info$column.name]])
      } else {
        if(!is.null(row[[col.info$column.name]][[1]])) {
          updateSelectizeInput(session, col.info$element.id,
                               selected = row[[col.info$column.name]][[1]][[col.info$column.name]])
        } else {
          updateSelectizeInput(session, col.info$element.id,
                               selected = character(0))
        }
      }
    }
  }
}

# If the user clicks "save", write the row to the database
save.form.table = function(form.table, changes, manage.id, db.con,
                           reactive.label.tables, session) {
  
  # Info about this table
  form.info = form.table.info[[form.table]]
  
  # Track the relevant id
  id = manage.id
  
  # Convert input fields into a dataframe
  temp.df = data.frame(changes[form.info$columns$column.name[is.na(form.info$columns$multi.table) & (form.info$columns$editable | form.info$columns$type == "file")]])
  temp.df[[form.info$key]] = id
  
  # If this is a new record, create SQL to insert into the base table
  if(id == -1) {
    
    # Create INSERT statement
    sql = temp.df %>%
      glue_data_sql(form.info$insert.sql, .con = db.con)
    sql = gsub("[^\\]''", "NULL", sql)
    
    # Attempt to insert row
    tryCatch(
      {
        dbGetQuery(db.con, sql)
        id = dbGetQuery(db.con, "SELECT LAST_INSERT_ID() AS NEW_ID")$NEW_ID[1]
        show.changes.saved(T,
                           db.table = gsub("^.*(wsdb\\.[a-z_]+).*$", "\\1",
                                           sql))
      },
      error = function(err) {
        print(err)
        show.changes.saved(F, err.msg = err)
      }
    )
    
  }
  
  # If this is an existing record, create SQL to update the base table
  else {
    
    # Create UPDATE statement
    sql = temp.df %>%
      glue_data_sql(form.info$update.base.sql, .con = db.con)
    sql = gsub("[^\\]''", "NULL", sql)
    
    # Attempt to update row
    tryCatch(
      {
        dbGetQuery(db.con, sql)
        show.changes.saved(T,
                           db.table = gsub("^.*(wsdb\\.[a-z_]+).*$", "\\1",
                                           sql))
      },
      error = function(err) {
        print(err)
        show.changes.saved(F, err.msg = err)
      }
    )
    
  }
  
  # Create SQL to update many-to-many tables
  for(i in 1:nrow(form.info$columns)) {
    col.info = form.info$columns[i,]
    if(!is.na(col.info$multi.table)) {
      
      # Delete old rows
      if(manage.id != -1) {
        sql = data.frame(id)
        colnames(sql) = form.info$key
        sql = sql %>%
          glue_data_sql(form.info$update.multi.sql[[col.info$column.name]]$delete,
                        .con = db.con)
        tryCatch(
          {
            dbGetQuery(db.con, sql)
            show.changes.saved(T,
                               db.table = gsub("^.*(wsdb\\.[a-z_]+).*$", "\\1",
                                               sql))
          },
          error = function(err) {
            print(err)
            show.changes.saved(F, err.msg = err)
          }
        )
      }
      
      # Insert new rows
      if(length(changes[[col.info$column.name]]) > 0) {
        sql = data.frame(changes[[col.info$column.name]])
        colnames(sql) = col.info$column.name
        sql[[form.info$key]] = id
        sql = sql %>%
          glue_data_sql(form.info$update.multi.sql[[col.info$column.name]]$insert,
                        .con = db.con)
        for(s in sql) {
          tryCatch(
            {
              dbGetQuery(db.con, s)
              show.changes.saved(T,
                                 db.table = gsub("^.*(wsdb\\.[a-z_]+).*$",
                                                 "\\1", s))
            },
            error = function(err) {
              print(err)
              show.changes.saved(F, err.msg = err)
            }
          )
        }
      }
      
    }
  }
  
  # If the user provided a lyrics file, save it to the database and to S3
  if("FileContents" %in% names(changes)) {
    lyrics.xml = changes$FileContents
    lyrics.html = gsub("<lyrics>", paste("<lyrics id=\"", id, "\">", sep = ""),
                       lyrics.xml)
    lyrics.html = gsub("</lyrics>", "<XXX>", lyrics.html)
    lyrics.html = gsub("<([a-z0-9]*)>", "<p class=\"lyrics-\\1\">",
                       lyrics.html)
    lyrics.html = gsub("</[a-z0-9]*>", "</p>", lyrics.html)
    lyrics.html = gsub("<XXX>", "</lyrics>", lyrics.html)
    lyrics.html = gsub("([^>])(\\r|\\n)+", "\\1 <br/>", lyrics.html)
    sql = "UPDATE wsdb.lyrics SET LyricsText = {lyrics.xml} WHERE LyricsID = {id}"
    dbGetQuery(db.con, glue_sql(sql, .con = db.con))
    s3write_using(changes$FileContents, writeLines, bucket = "wsdb-lyrics-test",
                  object = changes$FileName)
  }
  
}

# If the user clicks "delete", delete the row from the database
delete.form.table = function(form.table, manage.id, db.con) {
  
  # Info about this table
  form.info = form.table.info[[form.table]]
  
  # Prepare info for SQL
  temp.df = data.frame(manage.id)
  colnames(temp.df) = form.table.info[[form.table]]$key
  
  # Create SQL to delete the row
  sql = temp.df %>%
    glue_data_sql(form.table.info[[form.table]]$delete.sql, .con = db.con)
  
  # Attempt to delete row
  tryCatch(
    {
      dbGetQuery(db.con, sql)
      show.changes.saved(T,
                         db.table = gsub("^.*(wsdb\\.[a-z_]+).*$", "\\1", sql))
    },
    error = function(err) {
      print(err)
      show.changes.saved(F, err.msg = err)
    }
  )
  
}
