list(
  write = list(
    wsf_timesignatures =
      "SELECT *
       FROM wsf.timesignatures
       WHERE TimeSignatureID IN
             (SELECT songinstances_timesignatures.TimeSignatureID
              FROM wsf.songinstances_timesignatures
              WHERE songinstances_timesignatures.TimeSignatureID IN ({keys*}))",
    wsf_songinstances =
      "SELECT *
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_timesignatures.SongInstanceID
              FROM wsf.songinstances_timesignatures
              WHERE songinstances_timesignatures.TimeSignatureID IN ({keys*}))"
  ),
  delete = "wsf_timesignatures"
)
