list(
  write = list(
    wsf_scripturereferences =
      "SELECT *
       FROM wsf.scripturereferences
       WHERE ScriptureReferenceID IN
             (SELECT songinstances_scripturereferences.ScriptureReferenceID
              FROM wsf.songinstances_scripturereferences
              WHERE songinstances_scripturereferences.ScriptureReferenceID IN ({keys*}))",
    wsf_songinstances =
      "SELECT *
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_scripturereferences.SongInstanceID
              FROM wsf.songinstances_scripturereferences
              WHERE songinstances_scripturereferences.ScriptureReferenceID IN ({keys*}))",
    wsf_psalmsongs =
      "SELECT *
       FROM wsf.psalmsongs
       WHERE SongID IN
             (SELECT songinstances.SongID
              FROM wsf.songinstances
                   JOIN wsf.songinstances_scripturereferences
                        ON songinstances.SongInstanceID = songinstances_scripturereferences.SongInstanceID
              WHERE songinstances_scripturereferences.ScriptureReferenceID IN ({keys*}))
             OR MetricalPsalmID IN
             (SELECT metricalpsalms_lyrics.MetricalPsalmID
              FROM wsdb.metricalpsalms_lyrics
                   JOIN wsdb.lyrics_scripturereferences
                   ON metricalpsalms_lyrics.LyricsID = lyrics_scripturereferences.LyricsID
              WHERE lyrics_scripturereferences.ScriptureReferenceID IN ({keys*}))"
  ),
  delete = "wsf_scripturereferences"
)
