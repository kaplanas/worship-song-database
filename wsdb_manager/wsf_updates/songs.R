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
  delete = "wsf_songs"
)
