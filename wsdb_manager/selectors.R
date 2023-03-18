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
                             selected = selected.item)
      }
    )
  }
}