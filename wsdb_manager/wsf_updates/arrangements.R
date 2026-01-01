list(
  write = list(
    wsf_arrangementtypes =
      "SELECT *
       FROM wsf.arrangementtypes
       WHERE ArrangementTypeID IN
             (SELECT songinstances_arrangementtypes.ArrangementTypeID
              FROM wsf.songinstances_arrangementtypes
                   JOIN wsdb.songinstances
                   ON songinstances_arrangementtypes.SongInstanceID = songinstances.SongInstanceID
              WHERE songinstances.ArrangementID IN ({keys*}))",
    wsf_songinstances =
      "SELECT *
       FROM wsf.songinstances
       WHERE ArrangementID IN ({keys*})"
  ),
  multi = list(
    wsf_songinstances_arrangementtypes = list(
      keys = c("SongInstanceID", "ArrangementTypeID"),
      sql = "SELECT *
             FROM wsf.songinstances_arrangementtypes
             WHERE SongInstanceID IN
                   (SELECT songinstances.SongInstanceID
                    FROM wsdb.songinstances
                    WHERE songinstances.ArrangementID IN ({keys*}))"
    )
  )
)
