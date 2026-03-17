list(
  write = list(
    wsf_songs =
      "SELECT *
       FROM wsf.songs
       WHERE SongID IN ({keys*})",
    wsf_topics =
      "SELECT *
       FROM wsf.topics
       WHERE TopicID IN
             (SELECT songs_topics.TopicID
              FROM wsf.songs_topics
              WHERE songs_topics.SongID IN ({keys*}))",
    wsf_psalmsongs =
      "SELECT *
       FROM wsf.psalmsongs
       WHERE PsalmSongID IN
             (SELECT CONCAT('PS', psalmsongs.PsalmSongID)
              FROM wsdb.psalmsongs
              WHERE psalmsongs.SongID IN ({keys*}))",
    och_songs =
      "SELECT SongID, SongLabel
       FROM och.song_labels
       WHERE SongID IN ({keys*})",
    och_songtitles =
      "SELECT SongID, SongTitles
       FROM och.song_titles
       WHERE SongID IN ({keys*})",
    och_song_info =
      "SELECT *
       FROM och.song_info
       WHERE SongID IN ({keys*})"
  ),
  multi = list(
    wsf_songs_topics = list(
      keys = c("SongID", "TopicID"),
      sql = "SELECT *
             FROM wsf.songs_topics
             WHERE SongID IN ({keys*})"
    )
  ),
  delete = c("wsf_songs", "och_songs")
)
