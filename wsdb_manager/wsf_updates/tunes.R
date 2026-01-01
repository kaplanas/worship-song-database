list(
  write = list(
    wsf_tunes =
      "SELECT *
       FROM wsf.tunes
       WHERE TuneID IN
              (SELECT songinstances_tunes.TuneID
               FROM wsf.songinstances_tunes
               WHERE songinstances_tunes.TuneID IN ({keys*}))",
    wsf_artists =
      "SELECT *
       FROM wsf.artists
       WHERE ArtistID IN
             (SELECT tunes_artists.ArtistID
              FROM wsdb.tunes_artists
                   JOIN wsf.songinstances_tunes
                   ON tunes_artists.TuneID = songinstances_tunes.TuneID
              WHERE tunes_artists.TuneID IN ({keys*}))",
    wsf_songinstances =
      "SELECT *
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_tunes.SongInstanceID
              FROM wsdb.songinstances_tunes
              WHERE songinstances_tunes.TuneID IN ({keys*}))",
    wsf_psalmsongs =
      "SELECT *
       FROM wsf.psalmsongs
       WHERE SongID IN
             (SELECT songinstances.SongID
              FROM wsdb.songinstances
                   JOIN wsdb.songinstances_tunes
                   ON songinstances.SongInstanceID = songinstances_tunes.SongInstanceID
              WHERE songinstances_tunes.TuneID IN ({keys*}))"
  ),
  multi = list(
    wsf_songinstances_tunes = list(
      keys = c("SongInstanceID", "TuneID"),
      sql = "SELECT *
             FROM wsf.songinstances_tunes
             WHERE SongInstanceID IN ({keys})"
    ),
    wsf_songinstances_artists = list(
      keys = c("SongInstanceIDRole", "ArtistID"),
      sql = "SELECT *
             FROM wsf.songinstances_artists
             WHERE SongInstanceID IN
                   (SELECT songinstances_tunes.SongInstanceID
                    FROM wsdb.songinstances_tunes
                    WHERE songinstances_tunes.TuneID IN ({keys*}))"
    ),
    wsf_songinstances_meters = list(
      keys = c("SongInstanceID", "MeterID"),
      sql = "SELECT *
             FROM wsf.songinstances_meters
             WHERE SongInstanceID IN
                   (SELECT songinstances_tunes.SongInstanceID
                    FROM wsdb.songinstances_tunes
                    WHERE songinstances_tunes.TuneID IN ({keys*}))"
    )
  ),
  delete = "wsf_tunes"
)
