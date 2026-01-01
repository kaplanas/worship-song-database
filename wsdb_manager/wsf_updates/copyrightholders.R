list(
  write = list(
    wsf_songinstances =
      "SELECT *
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_lyrics.SongInstanceID
              FROM wsdb.songinstances_lyrics
                   JOIN wsdb.lyrics_copyrightholders
                   ON songinstances_lyrics.LyricsID = lyrics_copyrightholders.LyricsID
              WHERE lyrics_copyrightholders.CopyrightHolderID IN ({keys*}))
             OR SongInstanceID IN
             (SELECT songinstances_tunes.SongInstanceID
              FROM wsdb.songinstances_tunes
                   JOIN wsdb.tunes_copyrightholders
                   ON songinstances_tunes.TuneID = tunes_copyrightholders.TuneID
              WHERE tunes_copyrightholders.CopyrightHolderID IN ({keys*}))
             OR ArrangementID IN
             (SELECT ArrangementID
              FROM wsdb.arrangements_copyrightholders
              WHERE arrangements_copyrightholders.CopyrightHolderID IN ({keys*}))"
  )
)
