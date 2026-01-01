list(
  write = list(
    wsf_arrangementtypes =
      "SELECT *
       FROM wsf.arrangementtypes
       WHERE ArrangementTypeID IN
             (SELECT songinstances_arrangementtypes.ArrangementTypeID
              FROM wsf.songinstances_arrangementtypes
              WHERE songinstances_arrangementtypes.ArrangementTypeID IN ({keys*}))",
    wsf_songinstances =
      "SELECT *
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_arrangementtypes.SongInstanceID
              FROM wsf.songinstances_arrangementtypes
              WHERE songinstances_arrangementtypes.ArrangementTypeID IN ({keys*}))"
  ),
  delete = "wsf_arrangementtypes"
)
