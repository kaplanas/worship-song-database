list(
  write = list(
    wsf_languages =
      "SELECT *
       FROM wsf.languages
       WHERE LanguageID IN
             (SELECT songinstances_languages.LanguageID
              FROM wsf.songinstances_languages
              WHERE songinstances_languages.LanguageID IN ({keys*}))"
  ),
  multi = list(
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
                         LEFT JOIN wsdb.lyrics l
                         ON translations.LyricsID = l.LyricsID
                         LEFT JOIN wsdb.lyrics t
                         ON translations.TranslatedFromID = t.LyricsID
                    WHERE l.LanguageID IN ({keys*})
                          OR t.LanguageID IN ({keys*}))
                   OR PsalmSongID IN
                      (SELECT psalmsongs.PsalmSongID
                       FROM wsdb.metricalpsalms_lyrics
                            JOIN translations
                            ON metricalpsalms_lyrics.LyricsID = translations.LyricsID
                            JOIN wsf.psalmsongs
                            ON metricalpsalms_lyrics.MetricalPsalmID = psalmsongs.MetricalPsalmID
                            LEFT JOIN wsdb.lyrics l
                            ON translations.LyricsID = l.LyricsID
                            LEFT JOIN wsdb.lyrics t
                            ON translations.TranslatedFromID = t.LyricsID
                       WHERE l.LanguageID IN ({keys*})
                             OR t.LanguageID IN ({keys*}))"
    )
  ),
  delete = "wsf_languages"
)
