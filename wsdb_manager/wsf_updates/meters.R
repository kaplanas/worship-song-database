list(
  write = list(
    wsf_meters =
      "SELECT *
       FROM wsf.meters
       WHERE MeterID IN
             (SELECT songinstances_meters.MeterID
              FROM wsf.songinstances_meters
              WHERE songinstances_meters.MeterID IN ({keys*}))"
  ),
  delete = "wsf_meters"
)
