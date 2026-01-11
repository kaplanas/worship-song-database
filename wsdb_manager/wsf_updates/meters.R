list(
  write = list(
    wsf_meters =
      "SELECT *
       FROM wsf.meters
       WHERE MeterID IN
             (SELECT songinstances_meters.MeterID
              FROM wsf.songinstances_meters
              WHERE songinstances_meters.MeterID IN ({keys*})
              GROUP BY songinstances_meters.MeterID
              HAVING COUNT(DISTINCT songinstances_meters.SongID) >= 5)"
  ),
  delete = "wsf_meters"
)
