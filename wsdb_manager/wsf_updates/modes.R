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
              WHERE keysignatures.ModeID IN ({keys*}))",
    wsf_songinstances =
      "SELECT *
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_keysignatures.SongInstanceID
              FROM wsf.songinstances_keysignatures
                   JOIN wsdb.keysignatures
                   ON songinstances_keysignatures.KeySignatureID = keysignatures.KeySignatureID
              WHERE keysignatures.ModeID IN ({keys*}))"
  )
)
