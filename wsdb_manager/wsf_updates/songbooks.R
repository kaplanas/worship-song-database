list(
  write = list(
    wsf_songbooks =
      "SELECT *
       FROM wsf.songbooks
       WHERE SongbookID IN ({keys*})",
    wsf_songinstances =
      "SELECT *
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_songbooks.SongInstanceID
              FROM wsf.songinstances_songbooks
              WHERE songinstances_songbooks.SongbookID IN ({keys*}))",
    wsf_songinstances_songbooks =
      "SELECT *
       FROM wsf.songinstances_songbooks
       WHERE SongbookID IN ({keys*})",
    wsf_psalmsongs =
      "SELECT *
       FROM wsf.psalmsongs
       WHERE SongID IN
             (SELECT songinstances_songbooks.SongID
              FROM wsf.songinstances_songbooks
              WHERE songinstances_songbooks.SongbookID IN ({keys*}))"
  ),
  delete = "wsf_songbooks"
)
