list(
  write = list(
    wsf_lyrics_first_lines =
      "SELECT *
       FROM wsf.lyrics_first_lines
       WHERE LyricsID IN ({keys*})",
    wsf_languages =
      "SELECT *
       FROM wsf.languages
       WHERE LanguageID IN
             (SELECT songinstances_languages.LanguageID
              FROM wsf.songinstances_languages
                   JOIN wsdb.songinstances_lyrics
                   ON songinstances_languages.SongInstanceID = songinstances_lyrics.SongInstanceID
              WHERE songinstances_lyrics.LyricsID IN ({keys*}))",
    wsf_artists =
      "SELECT *
       FROM wsf.artists
       WHERE ArtistID IN
             (SELECT lyrics_artists.ArtistID
              FROM wsdb.lyrics_artists
                   JOIN wsdb.songinstances_lyrics
                   ON lyrics_artists.LyricsID = songinstances_lyrics.LyricsID
              WHERE lyrics_artists.LyricsID IN ({keys*}))",
    wsf_meters =
      "SELECT *
       FROM wsf.meters
       WHERE MeterID IN
             (SELECT lyrics_meters.MeterID
              FROM wsdb.lyrics_meters
                   JOIN wsdb.songinstances_lyrics
                   ON lyrics_meters.LyricsID = songinstances_lyrics.LyricsID
              WHERE lyrics_meters.LyricsID IN ({keys*}))",
    wsf_scripturereferences =
      "SELECT *
       FROM wsf.scripturereferences
       WHERE ScriptureReferenceID IN
             (SELECT lyrics_scripturereferences.ScriptureReferenceID
              FROM wsdb.lyrics_scripturereferences
                   JOIN wsdb.songinstances_lyrics
                   ON lyrics_scripturereferences.LyricsID = songinstances_lyrics.LyricsID
              WHERE lyrics_scripturereferences.LyricsID IN ({keys*}))",
    wsf_songinstances =
      "WITH RECURSIVE
            translations AS
            (SELECT lyrics.LyricsID,
                    lyrics_translations.TranslatedFromID
             FROM wsdb.lyrics
                  LEFT JOIN wsdb.lyrics_translations
                  ON lyrics.LyricsID = lyrics_translations.LyricsID
             UNION ALL
             SELECT translations.LyricsID,
                    lyrics_translations.TranslatedFromID
             FROM translations
                  JOIN wsdb.lyrics_translations
                  ON translations.TranslatedFromID = lyrics_translations.LyricsID)
       SELECT *
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_lyrics.SongInstanceID
              FROM wsdb.songinstances_lyrics
                   JOIN translations
                   ON songinstances_lyrics.LyricsID = translations.LyricsID
              WHERE translations.LyricsID IN ({keys*})
                    OR translations.TranslatedFromID IN ({keys*}))",
    wsf_psalmsongs =
      "WITH RECURSIVE
            translations AS
            (SELECT lyrics.LyricsID,
                    lyrics_translations.TranslatedFromID
             FROM wsdb.lyrics
                  LEFT JOIN wsdb.lyrics_translations
                  ON lyrics.LyricsID = lyrics_translations.LyricsID
             UNION ALL
             SELECT translations.LyricsID,
                    lyrics_translations.TranslatedFromID
             FROM translations
                  JOIN wsdb.lyrics_translations
                  ON translations.TranslatedFromID = lyrics_translations.LyricsID)
       SELECT *
       FROM wsf.psalmsongs
       WHERE SongID IN
             (SELECT songinstances.SongID
              FROM wsdb.songinstances
                   JOIN wsdb.songinstances_lyrics
                   ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
                   JOIN translations
                   ON songinstances_lyrics.LyricsID = translations.LyricsID
              WHERE translations.LyricsID IN ({keys*})
                    OR translations.TranslatedFromID IN ({keys*}))
             OR MetricalPsalmID IN
                (SELECT metricalpsalms_lyrics.MetricalPsalmID
                 FROM wsdb.metricalpsalms_lyrics
                      JOIN translations
                      ON metricalpsalms_lyrics.LyricsID = translations.LyricsID
                 WHERE translations.LyricsID IN ({keys*})
                       OR translations.TranslatedFromID IN ({keys*}))"
  ),
  multi = list(
    wsf_songinstances_artists = list(
      keys = c("SongInstanceIDRole", "ArtistID"),
      sql = "SELECT *
             FROM wsf.songinstances_artists
             WHERE SongInstanceID IN
                   (SELECT songinstances_lyrics.SongInstanceID
                    FROM wsdb.songinstances_lyrics
                    WHERE songinstances_lyrics.LyricsID IN ({keys*}))"
    ),
    wsf_songinstances_languages = list(
      keys = c("SongInstanceID", "LanguageID"),
      sql = "SELECT *
             FROM wsf.songinstances_languages
             WHERE SongInstanceID IN
                   (SELECT songinstances_lyrics.SongInstanceID
                    FROM wsdb.songinstances_lyrics
                    WHERE songinstances_lyrics.LyricsID IN ({keys*}))"
    ),
    wsf_songinstances_scripturereferences = list(
      keys = c("SongInstanceID", "ScriptureReferenceID"),
      sql = "SELECT *
             FROM wsf.songinstances_scripturereferences
             WHERE SongInstanceID IN
                   (SELECT songinstances_lyrics.SongInstanceID
                    FROM wsdb.songinstances_lyrics
                    WHERE songinstances_lyrics.LyricsID IN ({keys*}))"
    ),
    wsf_songinstances_meters = list(
      keys = c("SongInstanceID", "MeterID"),
      sql = "SELECT *
             FROM wsf.songinstances_meters
             WHERE SongInstanceID IN
                   (SELECT songinstances_lyrics.SongInstanceID
                    FROM wsdb.songinstances_lyrics
                    WHERE songinstances_lyrics.LyricsID IN ({keys*}))"
    ),
    wsf_psalmsongs_lyrics_tabs = list(
      keys = c("PsalmSongID", "LyricsOrder"),
      sql = "WITH RECURSIVE
                  translations AS
                  (SELECT lyrics.LyricsID,
                          lyrics_translations.TranslatedFromID
                   FROM wsdb.lyrics
                        LEFT JOIN wsdb.lyrics_translations
                        ON lyrics.LyricsID = lyrics_translations.LyricsID
                   UNION ALL
                   SELECT translations.LyricsID,
                          lyrics_translations.TranslatedFromID
                   FROM translations
                        JOIN wsdb.lyrics_translations
                        ON translations.TranslatedFromID = lyrics_translations.LyricsID)
             SELECT *
             FROM wsf.psalmsongs_lyrics_tabs
             WHERE PsalmSongID IN
                   (SELECT psalmsongs.PsalmSongID
                    FROM wsdb.songinstances
                         JOIN wsdb.songinstances_lyrics
                         ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
                         JOIN translations
                         ON songinstances_lyrics.LyricsID = translations.LyricsID
                         JOIN wsf.psalmsongs
                         ON songinstances.SongID = psalmsongs.SongID
                    WHERE translations.LyricsID IN ({keys*})
                          OR translations.TranslatedFromID IN ({keys*}))
                   OR PsalmSongID IN
                      (SELECT psalmsongs.PsalmSongID
                       FROM wsdb.metricalpsalms_lyrics
                            JOIN translations
                            ON metricalpsalms_lyrics.LyricsID = translations.LyricsID
                            JOIN wsf.psalmsongs
                            ON metricalpsalms_lyrics.MetricalPsalmID = psalmsongs.MetricalPsalmID
                       WHERE translations.LyricsID IN ({keys*})
                             OR translations.TranslatedFromID IN ({keys*}))"
    )
  )
)
