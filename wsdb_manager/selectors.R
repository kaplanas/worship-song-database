#### Selectors ####

# Lyrics
selector.lyrics.sql = "SELECT -1 AS LyricsID, '[new record]' AS SelectorDisplay
                       FROM dual
                       UNION ALL
                       SELECT LyricsID, LyricsLabel AS SelectorDisplay
                       FROM wsdb.lyrics_labels
                       ORDER BY CASE WHEN LyricsID = -1 THEN 1
                                     ELSE 2
                                END,
                                SelectorDisplay"

# Tunes
selector.tunes.sql = "SELECT -1 AS TuneID, '[new record]' AS SelectorDisplay
                      FROM dual
                      UNION ALL
                      SELECT TuneID, TuneLabel AS SelectorDisplay
                      FROM wsdb.tune_labels
                      ORDER BY CASE WHEN TuneID = -1 THEN 1
                                    ELSE 2
                               END,
                               SelectorDisplay"

# Arrangements
selector.arrangements.sql = "SELECT -1 AS ArrangementID,
                                    '[new record]' AS SelectorDisplay
                             FROM dual
                             UNION ALL
                             SELECT ArrangementID,
                                    ArrangementLabel AS SelectorDisplay
                             FROM wsdb.arrangement_labels
                             ORDER BY CASE WHEN ArrangementID = -1 THEN 1
                                           ELSE 2
                                      END,
                                      SelectorDisplay"

# Songs
selector.songs.sql = "SELECT -1 AS SongID, '[new record]' AS SelectorDisplay
                      FROM dual
                      UNION ALL
                      SELECT SongID, SongLabel AS SelectorDisplay
                      FROM wsdb.song_labels
                      ORDER BY CASE WHEN SongID = -1 THEN 1
                                    ELSE 2
                               END,
                               SelectorDisplay"

# Song instances
selector.song.instances.sql = "SELECT -1 AS SongInstanceID,
                                      '[new record]' COLLATE utf8_unicode_ci AS SelectorDisplay
                               FROM dual
                               UNION ALL
                               SELECT SongInstanceID, SongInstanceLabel
                               FROM wsdb.songinstance_labels
                               ORDER BY CASE WHEN SongInstanceID = -1 THEN 1
                                             ELSE 2
                                        END,
                                        SelectorDisplay"

# Metrical psalms
selector.metrical.psalms.sql = "SELECT MetricalPsalmID, SelectorDisplay
                                FROM (SELECT -1 AS MetricalPsalmID,
                                       '[new record]' AS SelectorDisplay,
                                       0 AS PsalmNumber
                                FROM dual
                                UNION ALL
                                SELECT metricalpsalm_labels.MetricalPsalmID,
                                       metricalpsalm_labels.MetricalPsalmLabel,
                                       CAST(metricalpsalms.PsalmNumber AS UNSIGNED) AS PsalmNumber
                                FROM wsdb.metricalpsalm_labels
                                     JOIN wsdb.metricalpsalms
                                     ON metricalpsalm_labels.MetricalPsalmID = metricalpsalms.MetricalPsalmID
                                ORDER BY PsalmNumber) sorted_labels"

# Alternative tunes by song
selector.alt.by.song.sql = "SELECT SongID, SongLabel AS SelectorDisplay
                            FROM wsdb.song_labels
                            WHERE SongID IN (SELECT SongID
                                             FROM wsdb.psalmsongs)
                            ORDER BY SelectorDisplay"

# Alternative tunes by metrical psalm
selector.alt.by.metrical.psalm.sql = "SELECT MetricalPsalmID,
                                             MetricalPsalmLabel AS SelectorDisplay
                                      FROM wsdb.metricalpsalm_labels
                                      ORDER BY SelectorDisplay"

# Alternative tunes by tune
selector.alt.by.tune.sql = "SELECT TuneID, TuneLabel AS SelectorDisplay
                            FROM wsdb.tune_song_labels
                            ORDER BY SelectorDisplay"

# Songbooks
selector.songbook.sql = "SELECT SongbookID, SongbookLabel AS SelectorDisplay
                         FROM wsdb.songbook_labels"

# Songbook volumes
selector.songbook.volume.sql = "SELECT -1 AS SongbookVolumeID,
                                       ' ' AS SelectorDisplay
                                FROM dual
                                UNION ALL
                                SELECT SongbookVolumeID,
                                       SongbookVolumeLabel AS SelectorDisplay
                                FROM wsdb.songbookvolume_labels"

#### Combined info ####

# List with everything
selector.sql = list(
  manage.lyrics.id = selector.lyrics.sql,
  manage.tunes.id = selector.tunes.sql,
  manage.arrangements.id = selector.arrangements.sql,
  manage.songs.id = selector.songs.sql,
  manage.song.instances.id = selector.song.instances.sql,
  manage.metrical.psalms.id = selector.metrical.psalms.sql,
  alt.by.song.id = selector.alt.by.song.sql,
  alt.by.metrical.psalm.id = selector.alt.by.metrical.psalm.sql,
  alt.by.tune.id = selector.alt.by.tune.sql,
  process.songbook.id = selector.songbook.sql,
  process.songbook.volume.id = selector.songbook.volume.sql
)

#### Useful functions ####

# Update the selector
update.selector = function(selector.name, db.con, session, current.selection) {
  if(!is.null(db.con)) {
    selector.list = list()
    tryCatch(
      {
        selector.list = dbGetQuery(db.con, selector.sql[[selector.name]]) %>%
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
        selected.item = current.selection
        if(length(selector.list) == 0 | length(selected.item) == 0) {
          selected.item = character(0)
        }
        else if(!(selected.item %in% selector.list)) {
          selected.item = character(0)
        }
        updateSelectizeInput(session, selector.name, choices = selector.list,
                             selected = selected.item, server = T)
      }
    )
  }
}