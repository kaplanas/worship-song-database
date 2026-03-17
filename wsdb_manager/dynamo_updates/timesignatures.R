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
      "SELECT SongInstanceID, SongID, NumEntries, HTML
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_timesignatures.SongInstanceID
              FROM wsf.songinstances_timesignatures
              WHERE songinstances_timesignatures.TimeSignatureID IN ({keys*}))",
    och_songinstances =
      "SELECT SongInstanceID, SongInstanceLabel
       FROM och.songinstance_labels
       WHERE SongInstanceID IN
             (SELECT songinstances_timesignatures.SongInstanceID
              FROM och.songinstances_timesignatures
              WHERE songinstances_timesignatures.TimeSignature IN ({keys*}))"
  ),
  delete = c("wsf_timesignatures")
)
