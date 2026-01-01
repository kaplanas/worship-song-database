list(
  write = list(
    wsf_songinstances =
      "SELECT *
       FROM wsf.songinstances
       WHERE SongInstanceID IN ({keys*})",
    wsf_artists =
      "SELECT *
       FROM wsf.artists
       WHERE ArtistID IN
             (SELECT songinstances_artists.ArtistID
              FROM wsf.songinstances_artists
              WHERE songinstances_artists.SongInstanceID IN ({keys*}))",
    wsf_tunes =
      "SELECT *
       FROM wsf.tunes
       WHERE TuneID IN
             (SELECT songinstances_tunes.TuneID
              FROM wsf.songinstances_tunes
              WHERE songinstances_tunes.SongInstanceID IN ({keys*}))",
    wsf_keysignatures =
      "SELECT *
       FROM wsf.keysignatures
       WHERE KeySignatureID IN
             (SELECT songinstances_keysignatures.KeySignatureID
              FROM wsf.songinstances_keysignatures
              WHERE songinstances_keysignatures.SongInstanceID IN ({keys*}))",
    wsf_timesignatures =
      "SELECT *
       FROM wsf.timesignatures
       WHERE TimeSignatureID IN
             (SELECT songinstances_timesignatures.TimeSignatureID
              FROM wsf.songinstances_timesignatures
              WHERE songinstances_timesignatures.SongInstanceID IN ({keys*}))",
    wsf_meters =
      "SELECT *
       FROM wsf.meters
       WHERE MeterID IN
             (SELECT songinstances_meters.MeterID
              FROM wsf.songinstances_meters
              WHERE songinstances_meters.SongInstanceID IN ({keys*}))",
    wsf_scripturereferences =
      "SELECT *
       FROM wsf.scripturereferences
       WHERE ScriptureReferenceID IN
             (SELECT songinstances_scripturereferences.ScriptureReferenceID
              FROM wsf.songinstances_scripturereferences
              WHERE songinstances_scripturereferences.SongInstanceID IN ({keys*}))",
    wsf_languages =
      "SELECT *
       FROM wsf.languages
       WHERE LanguageID IN
             (SELECT songinstances_languages.LanguageID
              FROM wsf.songinstances_languages
              WHERE songinstances_languages.SongInstanceID IN ({keys*}))",
    wsf_arrangementtypes =
      "SELECT *
       FROM wsf.arrangementtypes
       WHERE ArrangementTypeID IN
             (SELECT songinstances_arrangementtypes.ArrangementTypeID
              FROM wsf.songinstances_arrangementtypes
              WHERE songinstances_arrangementtypes.SongInstanceID IN ({keys*}))",
    wsf_psalmsongs =
      "SELECT *
       FROM wsf.psalmsongs
       WHERE SongID IN
             (SELECT songinstances.SongID
              FROM wsdb.songinstances
              WHERE songinstances.SongInstanceID IN ({keys*}))"
  ),
  multi = list(
    wsf_songinstances_arrangementtypes = list(
      keys = c("SongInstanceID", "ArrangementTypeID"),
      sql = "SELECT *
             FROM wsf.songinstances_arrangementtypes
             WHERE SongInstanceID IN ({keys*})"
    ),
    wsf_songinstances_artists = list(
      keys = c("SongInstanceIDRole", "ArtistID"),
      sql = "SELECT *
             FROM wsf.songinstances_artists
             WHERE SongInstanceID IN ({keys*})"
    ),
    wsf_songinstances_keysignatures = list(
      keys = c("SongInstanceID", "KeySignatureID"),
      sql = "SELECT *
             FROM wsf.songinstances_keysignatures
             WHERE SongInstanceID IN ({keys*})"
    ),
    wsf_songinstances_languages = list(
      keys = c("SongInstanceID", "LanguageID"),
      sql = "SELECT *
             FROM wsf.songinstances_languages
             WHERE SongInstanceID IN ({keys*})"
    ),
    wsf_lyrics_first_lines = list(
      keys = c("SongInstanceID", "LyricsIDFirstLineOrder"),
      sql = "SELECT *
             FROM wsf.lyrics_first_lines
             WHERE SongInstanceID IN ({keys*})"
    ),
    wsf_songinstances_meters = list(
      keys = c("SongInstanceID", "MeterID"),
      sql = "SELECT *
             FROM wsf.songinstances_meters
             WHERE SongInstanceID IN ({keys*})"
    ),
    wsf_songinstances_scripturereferences = list(
      keys = c("SongInstanceID", "ScriptureReferenceID"),
      sql = "SELECT *
             FROM wsf.songinstances_scripturereferences
             WHERE SongInstanceID IN ({keys*})"
    ),
    wsf_songinstances_songbooks = list(
      keys = c("SongInstanceIDEntryNumber", "SongbookID"),
      sql = "SELECT *
             FROM wsf.songinstances_songbooks
             WHERE SongInstanceID IN ({keys*})"
    ),
    wsf_songinstances_timesignatures = list(
      keys = c("SongInstanceID", "TimeSignatureID"),
      sql = "SELECT *
             FROM wsf.songinstances_timesignatures
             WHERE SongInstanceID IN ({keys*})"
    ),
    wsf_songinstances_tunes = list(
      keys = c("SongInstanceID", "TuneID"),
      sql = "SELECT *
             FROM wsf.songinstances_tunes
             WHERE SongInstanceID IN ({keys*})"
    ),
    wsf_psalmsongs_lyrics_tabs = list(
      keys = c("PsalmSongID", "LyricsOrder"),
      sql = "SELECT *
             FROM wsf.psalmsongs_lyrics_tabs
             WHERE PsalmSongID IN
                   (SELECT psalmsongs.PsalmSongID
                    FROM wsf.psalmsongs
                         JOIN wsdb.songinstances
                         ON psalmsongs.SongID = songinstances.SongID
                    WHERE songinstances.SongInstanceID IN ({keys*}))"
    )
  ),
  delete = "wsf_songinstances"
)
