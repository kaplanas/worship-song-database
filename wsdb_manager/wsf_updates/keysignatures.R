list(
  write = list(
    wsf_keysignatures =
      "SELECT *
       FROM wsf.keysignatures
       WHERE KeySignatureID IN
             (SELECT songinstances_keysignatures.KeySignatureID
              FROM wsf.songinstances_keysignatures
              WHERE songinstances_keysignatures.KeySignatureID IN ({keys*}))",
    wsf_songinstances =
      "SELECT *
       FROM wsf.songinstances
       WHERE SongInstanceID IN
             (SELECT songinstances_keysignatures.SongInstanceID
              FROM wsf.songinstances_keysignatures
              WHERE songinstances_keysignatures.KeySignatureID IN ({keys*}))"
  ),
  delete = "wsf_keysignatures"
)
