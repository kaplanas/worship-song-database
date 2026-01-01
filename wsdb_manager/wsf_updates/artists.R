list(
  write = list(
    wsf_artists =
      "SELECT *
       FROM wsf.artists
       WHERE ArtistID IN ({keys*})",
    wsf_songinstances =
      "SELECT *
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_artists.SongInstanceID
              FROM wsf.songinstances_artists
              WHERE songinstances_artists.ArtistID IN ({keys*}))",
    wsf_psalmsongs =
      "SELECT *
       FROM wsf.psalmsongs
       WHERE SongID IN
             (SELECT songinstances.SongID
              FROM wsf.songinstances
                   JOIN wsf.songinstances_artists
                        ON songinstances.SongInstanceID = songinstances_artists.SongInstanceID
              WHERE songinstances_artists.ArtistID IN ({keys*}))
             OR MetricalPsalmID IN
             (SELECT metricalpsalms_lyrics.MetricalPsalmID
              FROM wsdb.metricalpsalms_lyrics
                   JOIN wsdb.lyrics_artists
                   ON metricalpsalms_lyrics.LyricsID = lyrics_artists.LyricsID
              WHERE lyrics_artists.ArtistID IN ({keys*}))"
  ),
  delete = "wsf_artists"
)
