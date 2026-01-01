list(
  write = list(
    wsf_songinstances =
      "SELECT *
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_songbooks.SongInstanceID
              FROM wsf.songinstances_songbooks
              WHERE songinstances_songbooks.SongbookVolumeID IN ({keys*}))",
    wsf_psalmsongs =
      "SELECT *
       FROM wsf.psalmsongs
       WHERE SongID IN
             (SELECT songinstances_songbooks.SongID
              FROM wsf.songinstances_songbooks
              WHERE songinstances_songbooks.SongbookVolumeID IN ({keys*}))"
  )
)
