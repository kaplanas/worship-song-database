list(
  write = list(
    wsf_artists =
      "SELECT *
       FROM wsf.artists
       WHERE ArtistID IN ({keys*})",
    wsf_songinstances =
      "SELECT SongInstanceID, SongID, NumEntries, HTML
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_artists.SongInstanceID
              FROM wsf.songinstances_artists
              WHERE songinstances_artists.ArtistID IN ({keys*}))",
    wsf_psalmsongs =
      "SELECT *
       FROM wsf.psalmsongs
       WHERE PsalmSongID IN
             (SELECT CONCAT('PS', psalmsongs.PsalmSongID)
              FROM wsdb.psalmsongs
                   JOIN wsf.songinstances
                   ON psalmsongs.SongID = songinstances.SongID
                   JOIN wsf.songinstances_artists
                        ON songinstances.SongInstanceID = songinstances_artists.SongInstanceID
              WHERE songinstances_artists.ArtistID IN ({keys*}))
             OR PsalmSongID IN
             (SELECT CONCAT('MP', metricalpsalms_lyrics.MetricalPsalmID)
              FROM wsdb.metricalpsalms_lyrics
                   JOIN wsdb.lyrics_artists
                   ON metricalpsalms_lyrics.LyricsID = lyrics_artists.LyricsID
              WHERE lyrics_artists.ArtistID IN ({keys*}))"
  ),
  delete = c("wsf_artists")
)
