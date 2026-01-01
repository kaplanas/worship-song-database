list(
  write = list(
    wsf_songinstances_songbooks =
      "SELECT *
       FROM wsf.songinstances_songbooks
       WHERE SongbookEntryID IN ({keys*})",
    wsf_songinstances =
      "SELECT *
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_songbooks.SongInstanceID
              FROM wsf.songinstances_songbooks
              WHERE songinstances_songbooks.SongbookEntryID IN ({keys*}))",
    wsf_psalmsongs =
      "SELECT *
       FROM wsf.psalmsongs
       WHERE SongID IN
             (SELECT songinstances.SongID
              FROM wsdb.songinstances
                   JOIN wsf.songinstances_songbooks
                   ON songinstances.SongInstanceID = songinstances_songbooks.SongInstanceID
              WHERE songinstances_songbooks.SongbookEntryID IN ({keys*}))"
  ),
  delete = "wsf_songinstances_songbooks"
)
