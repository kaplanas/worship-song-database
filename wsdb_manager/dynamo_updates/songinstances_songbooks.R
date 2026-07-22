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
             (SELECT songbookentries.SongInstanceID
              FROM wsdb.songbookentries
              WHERE songbookentries.SongbookEntryID IN ({keys*}))",
    wsf_psalmsongs =
      "SELECT *
       FROM wsf.psalmsongs
       WHERE PsalmSongID IN
             (SELECT CONCAT('PS', psalmsongs.PsalmSongID)
              FROM wsdb.songinstances
                   JOIN wsdb.psalmsongs
                   ON songinstances.SongID = psalmsongs.SongID
                   JOIN wsdb.songbookentries
                   ON songinstances.SongInstanceID = songbookentries.SongInstanceID
              WHERE songbookentries.SongbookEntryID IN ({keys*}))",
    och_songinstances =
      "SELECT SongInstanceID, SongID, SongInstanceLabel
       FROM och.songinstance_labels
       WHERE SongInstanceID IN
             (SELECT songbookentries.SongInstanceID
              FROM wsdb.songbookentries
              WHERE songbookentries.SongbookEntryID IN ({keys*}))"
  ),
  delete = c("wsf_songinstances_songbooks")
)
