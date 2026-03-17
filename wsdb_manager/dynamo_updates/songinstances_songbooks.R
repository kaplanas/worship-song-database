list(
  write = list(
    wsf_songinstances_songbooks =
      "SELECT *
       FROM wsf.songinstances_songbooks
       WHERE SongbookEntryID IN ({keys*})",
    wsf_songinstances =
      "SELECT SongInstanceID, SongID, NumEntries, HTML
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_songbooks.SongInstanceID
              FROM wsf.songinstances_songbooks
              WHERE songinstances_songbooks.SongbookEntryID IN ({keys*}))",
    wsf_psalmsongs =
      "SELECT *
       FROM wsf.psalmsongs
       WHERE PsalmSongID IN
             (SELECT CONCAT('PS', psalmsongs.PsalmSongID)
              FROM wsdb.songinstances
                   JOIN wsdb.psalmsongs
                   ON songinstances.SongID = psalmsongs.SongID
                   JOIN wsf.songinstances_songbooks
                   ON songinstances.SongInstanceID = songinstances_songbooks.SongInstanceID
              WHERE songinstances_songbooks.SongbookEntryID IN ({keys*}))"
  ),
  delete = c("wsf_songinstances_songbooks")
)
