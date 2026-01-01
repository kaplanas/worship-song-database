list(
  write = list(
    wsf_psalmsongs_alternativetunes =
      "SELECT *
       FROM wsf.psalmsongs_alternativetunes
       WHERE AlternativeTuneID IN ({keys*})"
  ),
  multi = list(
    wsf_psalmsongs_alternativetunes = list(
      keys = c("PsalmSongID", "AlternativeTuneID"),
      sql = "SELECT *
             FROM wsf.psalmsongs_alternativetunes
             WHERE AlternativeTuneID IN ({keys*})"
    )
  )
)
