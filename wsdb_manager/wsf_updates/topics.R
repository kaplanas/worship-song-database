list(
  write = list(
    wsf_topics =
      "SELECT *
       FROM wsf.topics
       WHERE TopicID IN ({keys*})",
    wsf_songs =
      "SELECT *
       FROM wsf.songs
       WHERE SongID IN
             (SELECT songs_topics.SongID
              FROM wsf.songs_topics
              WHERE songs_topics.TopicID IN ({keys*}))"
  ),
  delete = "wsf_topics"
)
