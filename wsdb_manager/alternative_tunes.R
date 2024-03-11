#### Processing table info ####

alt.by.song.info = list(
  select.sql = "SELECT alternativetunes.AlternativeTuneID,
                       tune_song_labels.TuneLabel, alternativetunes.Notes,
                       alternativetunes.Created, alternativetunes.Updated
                FROM wsdb.alternativetunes
                     JOIN wsdb.tune_song_labels
                     ON alternativetunes.TuneID = tune_song_labels.TuneID
                WHERE alternativetunes.SongID = {alt.by.song.id}
                ORDER BY AlternativeTuneID",
  columns = data.frame(
    column.name = c("AlternativeTuneID", "TuneID", "Notes", "Created",
                    "Updated"),
    displayed = c(F, T, T, T, T),
    editable = c(F, T, T, F, F),
    width = c(NA, 250, 500, 200, 200),
    stringsAsFactors = F
  ),
  key = "AlternativeTuneID",
  suggest.sql = "SELECT CAST(NULL AS SIGNED) AS AlternativeTuneID,
                        tune_song_labels.TuneLabel,
                        CONCAT(CASE WHEN tune_song_labels.TuneLabel NOT IN
                                         (SELECT tune_song_labels.TuneLabel
                                          FROM wsdb.songinstances
                                               JOIN wsdb.songinstances_lyrics
                                               ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
                                               JOIN wsdb.lyrics_moods
                                               ON songinstances_lyrics.LyricsID = lyrics_moods.LyricsID
                                               JOIN wsdb.tunes
                                               ON lyrics_moods.MoodID = tunes.MoodID
                                               JOIN wsdb.tune_song_labels
                                               ON tunes.TuneID = tune_song_labels.TuneID
										                      WHERE songinstances.SongID = {alt.by.song.id}
										                            AND lyrics_moods.AssociationRule = 'Always')
                                         THEN '??? '
                                    ELSE ''
                               END, tune_notes.Notes) AS Notes
                 FROM wsdb.songinstances
                      JOIN (SELECT DISTINCT songinstances_lyrics.SongInstanceID,
                                   meters.Meter
                            FROM wsdb.songinstances_lyrics
                                 JOIN wsdb.lyrics_meters
                                 ON songinstances_lyrics.LyricsID = lyrics_meters.LyricsID
                                 JOIN wsdb.meters
                                 ON lyrics_meters.MeterID = meters.MeterID
                            WHERE meters.MeterID <> 1
                            UNION
                            SELECT DISTINCT songinstances_tunes.SongInstanceID,
                                   meters.Meter
                            FROM wsdb.songinstances_tunes
                                 JOIN wsdb.tunes_meters
                                 ON songinstances_tunes.TuneID = tunes_meters.TuneID
                                 JOIN wsdb.meters
                                 ON tunes_meters.MeterID = meters.MeterID
                            WHERE meters.MeterID <> 1) all_meters
                      ON songinstances.SongInstanceID = all_meters.SongInstanceID
                      LEFT JOIN wsdb.songinstances_lyrics
                      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
                      JOIN wsdb.meters
                      ON all_meters.Meter = meters.Meter
                      JOIN (SELECT DISTINCT TuneID, MeterID
                            FROM (SELECT TuneID, MeterID
                                  FROM wsdb.tunes_meters
                                  UNION ALL
                                  SELECT TuneID, MeterID
                                  FROM wsdb.songinstances_tunes
                                       JOIN wsdb.songinstances_lyrics
                                       ON songinstances_lyrics.SongInstanceID = songinstances_tunes.SongInstanceID
                                       JOIN wsdb.lyrics_meters
                                       ON songinstances_lyrics.LyricsID = lyrics_meters.LyricsID) tunes_meters_dup) tunes_meters
                      ON meters.MeterID = tunes_meters.MeterID
                      JOIN wsdb.tunes
                      ON tunes_meters.TuneID = tunes.TuneID
                      JOIN wsdb.tune_song_labels
                      ON tunes_meters.TuneID = tune_song_labels.TuneID
                      LEFT JOIN wsdb.alternativetunes existing_entries
                      ON tunes_meters.TuneID = existing_entries.TuneID
                         AND existing_entries.SongID = {alt.by.song.id}
					            LEFT JOIN (SELECT tunes_meters.TuneID,
					                              CONCAT(COALESCE(GROUP_CONCAT(DISTINCT
					                                                           lyrics.MeterPattern), ''),
					                                     ' ',
					                                     GROUP_CONCAT(DISTINCT
                                                            CONCAT(meters.Meter,
                                                                   CASE WHEN meters.Multiplier IS NULL
                                                                             THEN ''
                                                                        ELSE CONCAT(' ', meters.Multiplier)
														                                       END)
                                                            SEPARATOR ', ')) AS Notes
                                 FROM wsdb.tunes_meters
                                      JOIN wsdb.meters
                                      ON tunes_meters.MeterID = meters.MeterID
                                      LEFT JOIN wsdb.songinstances_tunes
                                      ON tunes_meters.TuneID = songinstances_tunes.TuneID
                                      LEFT JOIN wsdb.songinstances_lyrics
                                      ON songinstances_tunes.SongInstanceID = songinstances_lyrics.SongInstanceID
                                      LEFT JOIN wsdb.lyrics
                                      ON songinstances_lyrics.LyricsID = lyrics.LyricsID
								                 WHERE meters.MeterID <> 1
								                 GROUP BY tunes_meters.TuneID) tune_notes
                      ON tunes.TuneID = tune_notes.TuneID
                 WHERE songinstances.SongID = {alt.by.song.id}
                       AND tunes.TuneID NOT IN
                           (SELECT tunes.TuneID
                            FROM wsdb.songinstances
                                 JOIN wsdb.songinstances_lyrics
                                 ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
                                 JOIN wsdb.lyrics_moods
                                 ON songinstances_lyrics.LyricsID = lyrics_moods.LyricsID
                                 JOIN wsdb.tunes
                                 ON lyrics_moods.MoodID = tunes.MoodID
						                WHERE songinstances.SongID = {alt.by.song.id}
                                  AND lyrics_moods.AssociationRule = 'Never')
                       AND existing_entries.AlternativeTuneID IS NULL
                       AND tunes.TuneID NOT IN
                           (SELECT TuneID
                            FROM wsdb.songinstances_tunes
                                 JOIN wsdb.songinstances
                                 ON songinstances_tunes.SongInstanceID = songinstances.SongInstanceID
                            WHERE songinstances.SongID = {alt.by.song.id})
                 GROUP BY tune_song_labels.TuneLabel, tune_notes.Notes",
  update.sql = "UPDATE wsdb.alternativetunes
                SET SongID = {SongID},
                    TuneID = {TuneID},
                    SongOrMetricalPsalmID = {SongOrMetricalPsalmID},
                    Notes = {Notes}
                WHERE AlternativeTuneID = {AlternativeTuneID}",
  insert.sql = "INSERT INTO wsdb.alternativetunes
                (SongID, TuneID, SongOrMetricalPsalmID, Notes)
                VALUES
                ({SongID}, {TuneID}, {SongOrMetricalPsalmID}, {Notes})",
  delete.sql = "DELETE FROM wsdb.alternativetunes
                WHERE SongID = {alt.by.song.id}
                      AND AlternativeTuneID NOT IN ({keys*})"
)

alt.by.metrical.psalm.info = list(
  select.sql = "SELECT alternativetunes.AlternativeTuneID,
                       tune_song_labels.TuneLabel, alternativetunes.Notes,
                       alternativetunes.Created, alternativetunes.Updated
                FROM wsdb.alternativetunes
                     JOIN wsdb.tune_song_labels
                     ON alternativetunes.TuneID = tune_song_labels.TuneID
                WHERE alternativetunes.MetricalPsalmID = {alt.by.metrical.psalm.id}
                ORDER BY AlternativeTuneID",
  columns = data.frame(
    column.name = c("AlternativeTuneID", "TuneID", "Notes", "Created",
                    "Updated"),
    displayed = c(F, T, T, T, T),
    editable = c(F, T, T, F, F),
    width = c(NA, 250, 500, 200, 200),
    stringsAsFactors = F
  ),
  key = "AlternativeTuneID",
  suggest.sql = "SELECT CAST(NULL AS SIGNED) AS AlternativeTuneID,
                        tune_song_labels.TuneLabel,
                        CONCAT(CASE WHEN tune_song_labels.TuneLabel NOT IN
                                         (SELECT tune_song_labels.TuneLabel
                                          FROM wsdb.songinstances
                                               JOIN wsdb.songinstances_lyrics
                                               ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
                                               JOIN wsdb.lyrics_moods
                                               ON songinstances_lyrics.LyricsID = lyrics_moods.LyricsID
                                               JOIN wsdb.tunes
                                               ON lyrics_moods.MoodID = tunes.MoodID
                                               JOIN wsdb.tune_song_labels
                                               ON tunes.TuneID = tune_song_labels.TuneID
										                      WHERE songinstances.SongID = {alt.by.song.id}
										                            AND lyrics_moods.AssociationRule = 'Always')
                                         THEN '??? '
                                    ELSE ''
                               END, tune_notes.Notes) AS Notes
                 FROM wsdb.metricalpsalms
                      JOIN (SELECT DISTINCT metricalpsalms_lyrics.MetricalPsalmID,
                                   meters.Meter
                            FROM wsdb.metricalpsalms_lyrics
                                 JOIN wsdb.lyrics_meters
                                 ON metricalpsalms_lyrics.LyricsID = lyrics_meters.LyricsID
                                 JOIN wsdb.meters
                                 ON lyrics_meters.MeterID = meters.MeterID
                            WHERE meters.MeterID <> 1) all_meters
                      ON metricalpsalms.MetricalPsalmID = all_meters.MetricalPsalmID
                      LEFT JOIN wsdb.metricalpsalms_lyrics
                      ON metricalpsalms.MetricalPsalmID = metricalpsalms_lyrics.MetricalPsalmID
                      JOIN wsdb.meters
                      ON all_meters.Meter = meters.Meter
                      JOIN (SELECT DISTINCT TuneID, MeterID
                            FROM (SELECT TuneID, MeterID
                                  FROM wsdb.tunes_meters
                                  UNION ALL
                                  SELECT TuneID, MeterID
                                  FROM wsdb.songinstances_tunes
                                       JOIN wsdb.songinstances_lyrics
                                       ON songinstances_lyrics.SongInstanceID = songinstances_tunes.SongInstanceID
                                       JOIN wsdb.lyrics_meters
                                       ON songinstances_lyrics.LyricsID = lyrics_meters.LyricsID) tunes_meters_dup) tunes_meters
                      ON meters.MeterID = tunes_meters.MeterID
                      JOIN wsdb.tunes
                      ON tunes_meters.TuneID = tunes.TuneID
                      JOIN wsdb.tune_song_labels
                      ON tunes_meters.TuneID = tune_song_labels.TuneID
                      LEFT JOIN wsdb.alternativetunes existing_entries
                      ON tunes_meters.TuneID = existing_entries.TuneID
                         AND existing_entries.MetricalPsalmID = {alt.by.metrical.psalm.id}
					            LEFT JOIN (SELECT tunes_meters.TuneID,
                                        GROUP_CONCAT(DISTINCT
                                                     CONCAT(meters.Meter,
                                                            CASE WHEN meters.Multiplier IS NULL
                                                                      THEN ''
                                                                 ELSE CONCAT(' ', meters.Multiplier)
														                                END)
                                                     SEPARATOR ', ') AS Notes
                                 FROM wsdb.tunes_meters
                                      JOIN wsdb.meters
                                      ON tunes_meters.MeterID = meters.MeterID
								                 WHERE meters.MeterID <> 1
								                 GROUP BY tunes_meters.TuneID) tune_notes
                      ON tunes.TuneID = tune_notes.TuneID
                 WHERE metricalpsalms.MetricalPsalmID = {alt.by.metrical.psalm.id}
                       AND tunes.TuneID NOT IN
                           (SELECT tunes.TuneID
                            FROM wsdb.metricalpsalms_lyrics
                                 JOIN wsdb.lyrics_moods
                                 ON metricalpsalms_lyrics.LyricsID = lyrics_moods.LyricsID
                                 JOIN wsdb.tunes
                                 ON lyrics_moods.MoodID = tunes.MoodID
						                WHERE metricalpsalms.MetricalPsalmID = {alt.by.metrical.psalm.id}
                                  AND lyrics_moods.AssociationRule = 'Never')
                       AND existing_entries.AlternativeTuneID IS NULL
                 GROUP BY tune_song_labels.TuneLabel, tune_notes.Notes",
  update.sql = "UPDATE wsdb.alternativetunes
                SET MetricalPsalmID = {MetricalPsalmID},
                    TuneID = {TuneID},
                    SongOrMetricalPsalmID = {SongOrMetricalPsalmID},
                    Notes = {Notes}
                WHERE AlternativeTuneID = {AlternativeTuneID}",
  insert.sql = "INSERT INTO wsdb.alternativetunes
                (MetricalPsalmID, TuneID, SongOrMetricalPsalmID, Notes)
                VALUES
                ({MetricalPsalmID}, {TuneID}, {SongOrMetricalPsalmID}, {Notes})",
  delete.sql = "DELETE FROM wsdb.alternativetunes
                WHERE MetricalPsalmID = {alt.by.metrical.psalm.id}
                      AND AlternativeTuneID NOT IN ({keys*})"
)

alt.by.tune.info = list(
  select.sql = "SELECT alternativetunes.AlternativeTuneID,
                       song_labels.SongLabel,
                       metricalpsalm_labels.MetricalPsalmLabel,
                       alternativetunes.Notes, alternativetunes.Created,
                       alternativetunes.Updated
                FROM wsdb.alternativetunes
                     LEFT JOIN wsdb.song_labels
                     ON alternativetunes.SongID = song_labels.SongID
                     LEFT JOIN wsdb.metricalpsalm_labels
                     ON alternativetunes.MetricalPsalmID = metricalpsalm_labels.MetricalPsalmID
                WHERE alternativetunes.TuneID = {alt.by.tune.id}
                ORDER BY AlternativeTuneID",
  columns = data.frame(
    column.name = c("AlternativeTuneID", "SongID", "MetricalPsalmID", "Notes",
                    "Created", "Updated"),
    displayed = c(F, T, T, T, T, T),
    editable = c(F, T, T, T, F, F),
    width = c(NA, 400, 400, 500, 200, 200),
    stringsAsFactors = F
  ),
  key = "AlternativeTuneID",
  suggest.sql = "SELECT CAST(NULL AS SIGNED) AS AlternativeTuneID,
                        song_labels.SongLabel,
                        metricalpsalm_labels.MetricalPsalmLabel,
                        CONCAT(CASE WHEN song_labels.SongLabel NOT IN
                                         (SELECT song_labels.SongLabel
                                          FROM wsdb.songinstances
                                               JOIN wsdb.songinstances_lyrics
                                               ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
                                               JOIN wsdb.lyrics_moods
                                               ON songinstances_lyrics.LyricsID = lyrics_moods.LyricsID
                                               JOIN wsdb.tunes
                                               ON lyrics_moods.MoodID = tunes.MoodID
                                               JOIN wsdb.song_labels
                                               ON songinstances.SongID = song_labels.SongID
										                      WHERE tunes.TuneID = {alt.by.tune.id}
										                            AND lyrics_moods.AssociationRule = 'Always')
                                         THEN '??? '
                                    WHEN metricalpsalm_labels.MetricalPsalmLabel NOT IN
                                         (SELECT metricalpsalm_labels.MetricalPsalmLabel
                                          FROM wsdb.metricalpsalms_lyrics
                                               JOIN wsdb.lyrics_moods
                                               ON metricalpsalms_lyrics.LyricsID = lyrics_moods.LyricsID
                                               JOIN wsdb.tunes
                                               ON lyrics_moods.MoodID = tunes.MoodID
                                               JOIN wsdb.metricalpsalm_labels
                                               ON metricalpsalms_lyrics.MetricalPsalmID = metricalpsalm_labels.MetricalPsalmID
										                      WHERE tunes.TuneID = {alt.by.tune.id}
										                            AND lyrics_moods.AssociationRule = 'Always')
                                         THEN '??? '
                                    ELSE ''
                               END, lyrics_notes.Notes) AS Notes
                 FROM wsdb.tunes
                      JOIN wsdb.tune_song_labels
                      ON tunes.TuneID = tune_song_labels.TuneID
                      JOIN (SELECT DISTINCT TuneID, MeterID
                            FROM (SELECT TuneID, MeterID
                                  FROM wsdb.tunes_meters
                                  WHERE MeterID <> 1
                                  UNION ALL
                                  SELECT TuneID, MeterID
                                  FROM wsdb.songinstances_tunes
                                       JOIN wsdb.songinstances_lyrics
                                       ON songinstances_lyrics.SongInstanceID = songinstances_tunes.SongInstanceID
                                       JOIN wsdb.lyrics_meters
                                       ON songinstances_lyrics.LyricsID = lyrics_meters.LyricsID
                                  WHERE MeterID <> 1) tunes_meters_dup) tunes_meters
                      ON tunes.TuneID = tunes_meters.TuneID
                      JOIN wsdb.meters
                      ON tunes_meters.MeterID = meters.MeterID
                      JOIN (SELECT DISTINCT songinstances.SongID,
                                   NULL AS MetricalPsalmID, meters.Meter
                            FROM wsdb.songinstances
                                 JOIN wsdb.songinstances_lyrics
                                 ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
                                 JOIN wsdb.lyrics_meters
                                 ON songinstances_lyrics.LyricsID = lyrics_meters.LyricsID
                                 JOIN wsdb.meters
                                 ON lyrics_meters.MeterID = meters.MeterID
                            WHERE songinstances.SongID IN (SELECT SongID
                                                           FROM wsdb.psalmsongs)
                            UNION
                            SELECT DISTINCT songinstances.SongID,
                                   NULL AS MetricalPsalmID, meters.Meter
                            FROM wsdb.songinstances
                                 JOIN wsdb.songinstances_tunes
                                 ON songinstances.SongInstanceID = songinstances_tunes.SongInstanceID
                                 JOIN wsdb.tunes_meters
                                 ON songinstances_tunes.TuneID = tunes_meters.TuneID
                                 JOIN wsdb.meters
                                 ON tunes_meters.MeterID = meters.MeterID
                            WHERE songinstances.SongID IN (SELECT SongID
                                                           FROM wsdb.psalmsongs)
                            UNION
                            SELECT DISTINCT NULL AS SongID,
                                   metricalpsalms_lyrics.MetricalPsalmID,
                                   meters.Meter
                            FROM wsdb.metricalpsalms_lyrics
                                 JOIN wsdb.lyrics_meters
                                 ON metricalpsalms_lyrics.LyricsID = lyrics_meters.LyricsID
                                 JOIN wsdb.meters
                                 ON lyrics_meters.MeterID = meters.MeterID) all_meters
                      ON meters.Meter = all_meters.Meter
                      LEFT JOIN wsdb.song_labels
                      ON all_meters.SongID = song_labels.SongID
                      LEFT JOIN wsdb.metricalpsalm_labels
                      ON all_meters.MetricalPsalmID = metricalpsalm_labels.MetricalPsalmID
                      LEFT JOIN wsdb.alternativetunes existing_entries
                      ON (all_meters.SongID = existing_entries.SongID
                          OR all_meters.MetricalPsalmID = existing_entries.MetricalPsalmID)
                         AND existing_entries.TuneID = {alt.by.tune.id}
					            LEFT JOIN (SELECT songinstances.SongID, metricalpsalms_lyrics.MetricalPsalmID,
                                        GROUP_CONCAT(DISTINCT
                                                     COALESCE(lyrics.MeterPattern,
                                                     CONCAT(meters.Meter,
                                                            CASE WHEN meters.Multiplier IS NULL
                                                                      THEN ''
                                                                 ELSE CONCAT(' ', meters.Multiplier)
														                                END))
                                                     SEPARATOR ', ') AS Notes
                                 FROM wsdb.lyrics
                                      JOIN wsdb.lyrics_meters
                                      ON lyrics.LyricsID = lyrics_meters.LyricsID
                                      JOIN wsdb.meters
                                      ON lyrics_meters.MeterID = meters.MeterID
                                      LEFT JOIN wsdb.songinstances_lyrics
                                      ON lyrics_meters.LyricsID = songinstances_lyrics.LyricsID
                                      LEFT JOIN wsdb.songinstances
                                      ON songinstances_lyrics.SongInstanceID = songinstances.SongInstanceID
                                      LEFT JOIN wsdb.metricalpsalms_lyrics
                                      ON lyrics_meters.LyricsID = metricalpsalms_lyrics.LyricsID
								                 WHERE meters.MeterID <> 1
								                 GROUP BY songinstances.SongID, metricalpsalms_lyrics.MetricalPsalmID) lyrics_notes
                      ON all_meters.SongID = lyrics_notes.SongID
                         OR all_meters.MetricalPsalmID = lyrics_notes.MetricalPsalmID
                 WHERE tunes.TuneID = {alt.by.tune.id}
                       AND existing_entries.AlternativeTuneID IS NULL
                       AND COALESCE(all_meters.SongID, -1) NOT IN
                           (SELECT songinstances.SongID
                            FROM wsdb.tunes
								            JOIN wsdb.lyrics_moods
                                 ON tunes.MoodID = lyrics_moods.MoodID
                                 JOIN wsdb.songinstances_lyrics
                                 ON lyrics_moods.LyricsID = songinstances_lyrics.LyricsID
                                 JOIN wsdb.songinstances
                                 ON songinstances_lyrics.SongInstanceID = songinstances.SongInstanceID
						                WHERE tunes.TuneID = {alt.by.tune.id}
                                  AND lyrics_moods.AssociationRule = 'Never')
                       AND COALESCE(all_meters.MetricalPsalmID, -1) NOT IN
                           (SELECT metricalpsalms_lyrics.MetricalPsalmID
                            FROM wsdb.tunes
								                 JOIN wsdb.lyrics_moods
                                 ON tunes.MoodID = lyrics_moods.MoodID
                                 JOIN wsdb.metricalpsalms_lyrics
                                 ON lyrics_moods.LyricsID = metricalpsalms_lyrics.LyricsID
						                WHERE tunes.TuneID = {alt.by.tune.id}
                                  AND lyrics_moods.AssociationRule = 'Never')
					             AND COALESCE(all_meters.SongID, -1) NOT IN
                           (SELECT SongID
                            FROM wsdb.songinstances_tunes
                                 JOIN wsdb.songinstances
                                 ON songinstances_tunes.SongInstanceID = songinstances.SongInstanceID
                            WHERE songinstances_tunes.TuneID = {alt.by.tune.id})
                 GROUP BY song_labels.SongLabel,
                          metricalpsalm_labels.MetricalPsalmLabel,
                          lyrics_notes.Notes",
  update.sql = "UPDATE wsdb.alternativetunes
                SET SongID = {SongID},
                    MetricalPsalmID = {MetricalPsalmID},
                    SongOrMetricalPsalmID = {SongOrMetricalPsalmID},
                    Notes = {Notes}
                WHERE AlternativeTuneID = {AlternativeTuneID}",
  insert.sql = "INSERT INTO wsdb.alternativetunes
                (SongID, MetricalPsalmID, TuneID, SongOrMetricalPsalmID, Notes)
                VALUES
                ({SongID}, {MetricalPsalmID}, {TuneID}, {SongOrMetricalPsalmID},
                 {Notes})",
  delete.sql = "DELETE FROM wsdb.alternativetunes
                WHERE TuneID = {alt.by.tune.id}
                      AND AlternativeTuneID NOT IN ({keys*})"
)

#### Combined info ####

# List with everything
alt.table.info = list(
  alt.by.song = alt.by.song.info,
  alt.by.metrical.psalm = alt.by.metrical.psalm.info,
  alt.by.tune = alt.by.tune.info
)

#### Processing page ####

alt.by.song = tabPanel(
  "By song",
  fluidRow(
    column(9,
           selectizeInput("alt.by.song.id", "Choose song:", choices = list(),
                          options = list(maxOptions = 100000,
                                         render = I(selectize.html.render)))),
    column(3,
           actionButton("suggest.alt.by.song",
                        label = "Suggest alternative tunes"))
  ),
  rHandsontableOutput("alt.by.song"),
  actionButton("save.alt.by.song", label = "Save changes")
)

alt.by.metrical.psalm = tabPanel(
  "By metrical psalm",
  fluidRow(
    column(9,
           selectizeInput("alt.by.metrical.psalm.id", "Choose metrical psalm:",
                          choices = list(),
                          options = list(maxOptions = 100000,
                                         render = I(selectize.html.render)))),
    column(3,
           actionButton("suggest.alt.by.metrical.psalm",
                        label = "Suggest alternative tunes"))
  ),
  rHandsontableOutput("alt.by.metrical.psalm"),
  actionButton("save.alt.by.metrical.psalm", label = "Save changes")
)

alt.by.tune = tabPanel(
  "By tune",
  fluidRow(
    column(9,
           selectizeInput("alt.by.tune.id", "Choose tune:", choices = list(),
                          options = list(maxOptions = 100000,
                                         render = I(selectize.html.render)))),
    column(3,
           actionButton("suggest.alt.by.tune",
                        label = "Suggest alternative tunes"))
  ),
  rHandsontableOutput("alt.by.tune"),
  actionButton("save.alt.by.tune", label = "Save changes")
)

alternative.tunes.page = tabPanel("Catalogue alternative tunes",
                                  navlistPanel(
                                    alt.by.song,
                                    alt.by.metrical.psalm,
                                    alt.by.tune,
                                    well = F,
                                    widths = c(2, 10)
                                  ))

#### Useful functions ####

# Populate the table
populate.alternative.tunes.table = function(alt.table, db.con, alt.by.song.id,
                                            alt.by.metrical.psalm.id,
                                            alt.by.tune.id) {
  alt.info = alt.table.info[[alt.table]]
  if(!is.null(db.con)) {
    tryCatch(
      {
        sql = glue_sql(alt.info$select.sql, .con = db.con)
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
create.alternative.tunes.hot = function(df, alt.table, reactive.label.tables,
                                        window.width, window.height) {
  alt.info = alt.table.info[[alt.table]]
  if(is.null(df)) {
    NULL
  } else {
    temp.df = df[,alt.info$columns$displayed]
    if(nrow(temp.df) == 0) {
      temp.df = temp.df %>% add_row()
    }
    temp.hot = rhandsontable(temp.df, width = window.width * 0.75,
                             height = window.height * 0.65, rowHeaders = NULL,
                             overflow = "visible") %>%
      hot_context_menu(allowColEdit = F) %>%
      hot_cols(colWidths = alt.info$columns$width[alt.info$columns$displayed],
               columnSorting = F) %>%
      hot_col(col = which(!alt.info$columns$editable[alt.info$columns$displayed]),
              readOnly = T)
    if("TuneID" %in% alt.info$columns$column.name) {
      temp.hot = temp.hot %>%
        hot_col(col = "TuneLabel", type = "dropdown", strict = T,
                renderer = "html",
                source = c("", reactive.label.tables$tune.song.labels$TuneLabel)) %>%
        hot_col(col = "TuneLabel",
                renderer = htmlwidgets::JS("safeHtmlRenderer"))
    }
    if("SongID" %in% alt.info$columns$column.name) {
      temp.hot = temp.hot %>%
        hot_col(col = "SongLabel", type = "dropdown", strict = T,
                renderer = "html",
                source = c("", reactive.label.tables$song.labels$SongLabel)) %>%
        hot_col(col = "SongLabel",
                renderer = htmlwidgets::JS("safeHtmlRenderer"))
    }
    if("MetricalPsalmID" %in% alt.info$columns$column.name) {
      temp.hot = temp.hot %>%
        hot_col(col = "MetricalPsalmLabel", type = "dropdown", strict = T,
                renderer = "html",
                source = c("", reactive.label.tables$metrical.psalm.labels$MetricalPsalmLabel)) %>%
        hot_col(col = "MetricalPsalmLabel",
                renderer = htmlwidgets::JS("safeHtmlRenderer"))
    }
    temp.hot
  }
}

# When the user makes a change in the interface, propagate the change to the
# reactive table
update.alternative.tunes.hot = function(alt.table, change, reactive.alt.tables,
                                        reactive.alt.changes) {
  
  # Get info for this table
  alt.info = alt.table.info[[alt.table]]
  
  # If it was an edit, update the edited cell
  if(change$event == "afterChange") {
    if(!is.null(change$changes)) {
      r = change$changes[[1]][[1]] + 1
      c = change$changes[[1]][[2]] + 2
      reactive.alt.tables[[alt.table]][r,c] = change$changes[[1]][[4]]
      reactive.alt.changes[[alt.table]]$edit =
        c(reactive.alt.changes[[alt.table]]$edit,
          reactive.alt.tables[[alt.table]][r,alt.info$key])
    }
  }
  
  # If it was a new row, insert an empty row
  else if(change$event == "afterCreateRow") {
    reactive.alt.tables[[alt.table]][nrow(reactive.alt.tables[[alt.table]]) + 1,] = NA
    reactive.alt.changes[[alt.table]]$insert = T
  }
  
  # If it was a deleted row, remove that row
  else if(change$event == "afterRemoveRow") {
    r = seq(change$ind + 1, length.out = change$ct)
    reactive.alt.tables[[alt.table]] = reactive.alt.tables[[alt.table]][-r,]
    reactive.alt.changes[[alt.table]]$delete = T
  }
  
}

# If the user requests suggestions, retrieve them
suggest.alternative.tunes = function(alt.table, db.con, alt.by.song.id,
                                     alt.by.metrical.psalm.id, alt.by.tune.id) {
  alt.info = alt.table.info[[alt.table]]
  if(!is.null(db.con)) {
    tryCatch(
      {
        sql = glue_sql(alt.info$suggest.sql, .con = db.con)
        return(dbGetQuery(db.con, sql) %>%
                 mutate(AlternativeTuneID = NA_integer_))
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

# If the user clicks "save", write the table to the database
save.alternative.tunes.table = function(alt.table, db.con, reactive.alt.tables,
                                        reactive.label.tables,
                                        reactive.alt.changes, alt.by.song.id,
                                        alt.by.metrical.psalm.id,
                                        alt.by.tune.id) {
  
  # Info about this table
  alt.info = alt.table.info[[alt.table]]
  
  # Get raw data to use for update (mapping labels back to IDs)
  temp.df = reactive.alt.tables[[alt.table]]
  if(alt.table == "alt.by.song") {
    temp.df = temp.df %>%
      left_join(reactive.label.tables$tune.song.labels, by = "TuneLabel") %>%
      mutate(SongID = alt.by.song.id,
             MetricalPsalmID = NA)
  } else if(alt.table == "alt.by.metrical.psalm") {
    temp.df = temp.df %>%
      left_join(reactive.label.tables$tune.song.labels, by = "TuneLabel") %>%
      mutate(SongID = NA,
             MetricalPsalmID = alt.by.metrical.psalm.id)
  } else if(alt.table == "alt.by.tune") {
    temp.df = temp.df %>%
      left_join(reactive.label.tables$song.labels, by = "SongLabel") %>%
      left_join(reactive.label.tables$metrical.psalm.labels, by = "MetricalPsalmLabel") %>%
      mutate(TuneID = alt.by.tune.id)
  }
  temp.df = temp.df %>%
    dplyr::select(AlternativeTuneID, SongID, MetricalPsalmID, TuneID, Notes) %>%
    mutate(SongOrMetricalPsalmID = if_else(!is.na(SongID),
                                           paste("ps", SongID, sep = ""),
                                           paste("mp", MetricalPsalmID, sep = "")))
  
  # If there were edits, issue an UPDATE statement
  if(length(reactive.alt.changes[[alt.table]]$edit) > 0) {
    
    # Create sql to update changed rows
    sql = temp.df %>%
      filter(.data[[alt.info$key]] %in% reactive.alt.changes[[alt.table]]$edit) %>%
      glue_data_sql(alt.info$update.sql, .con = db.con)
    
    # Attempt to update changed rows
    for(s in sql) {
      tryCatch(
        {
          dbGetQuery(db.con, s)
          reactive.alt.changes[[alt.table]]$edit = c()
          show.changes.saved(T,
                             db.table = gsub("^.*(wsdb\\.[a-z_]+).*$", "\\1",
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
  if(reactive.alt.changes[[alt.table]]$insert) {
    
    # Attempt to insert new rows
    if(any(is.na(temp.df$AlternativeTuneID))) {
      sql = temp.df %>%
        filter(is.na(AlternativeTuneID)) %>%
        glue_data_sql(alt.info$insert.sql, .con = db.con)
      sql = gsub("NULL", "DEFAULT", sql)
      for(s in sql) {
        tryCatch(
          {
            dbGetQuery(db.con, s)
            reactive.alt.changes[[alt.table]]$insert = F
            show.changes.saved(T,
                               db.table = gsub("^.*(wsdb\\.[a-z_]+).*$", "\\1",
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
  if(reactive.alt.changes[[alt.table]]$delete) {
    
    # Create sql to delete rows
    sql = glue_sql(alt.info$delete.sql, keys = temp.df$AlternativeTuneID,
                   .con = db.con)
    
    # Attempt to delete rows
    tryCatch(
      {
        dbGetQuery(db.con, sql)
        reactive.alt.changes[[alt.table]]$delete = F
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
  
}
