list(
  write = list(
    wsf_songinstances =
      "SELECT SongInstanceID, SongID, NumEntries, HTML
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_songbooks.SongInstanceID
              FROM wsf.songinstances_songbooks
              WHERE songinstances_songbooks.SongbookVolumeID IN ({keys*}))",
    wsf_psalmsongs =
      "SELECT *
       FROM wsf.psalmsongs
       WHERE PsalmSongID IN
             (SELECT CONCAT('PS', psalmsongs.PsalmSongID)
              FROM wsf.songinstances_songbooks
                   JOIN wsdb.psalmsongs
                   ON songinstances_songbooks.SongID = psalmsongs.SongID
              WHERE songinstances_songbooks.SongbookVolumeID IN ({keys*}))"
  )
)
