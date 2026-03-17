list(
  write = list(
    wsf_keysignatures =
      "SELECT *
       FROM wsf.keysignatures
       WHERE KeySignatureID IN
             (SELECT keysignatures.KeySignatureID
              FROM wsdb.keysignatures
                   JOIN wsf.songinstances_keysignatures
                   ON keysignatures.KeySignatureID = songinstances_keysignatures.KeySignatureID
              WHERE keysignatures.AccidentalID IN ({keys*}))",
    wsf_songinstances =
      "SELECT SongInstanceID, SongID, NumEntries, HTML
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_keysignatures.SongInstanceID
              FROM wsf.songinstances_keysignatures
                   JOIN wsdb.keysignatures
                   ON songinstances_keysignatures.KeySignatureID = keysignatures.KeySignatureID
              WHERE keysignatures.AccidentalID IN ({keys*}))",
    och_songinstances =
      "SELECT SongInstanceID, SongInstanceLabel
       FROM och.songinstances_labels
       WHERE SongInstanceID IN
             (SELECT songinstances_keysignatures.SongInstanceID
              FROM wsdb.songinstances_keysignatures
                   JOIN wsdb.keysignatures
                   ON songinstances_keysignatures.KeySignatureID = keysignatures.KeySignatureID
              WHERE keysignatures.AccidentalID IN ({keys*}))"
  )
)
