list(
  write = list(
    wsf_psalmsongs_alternativetunes =
      "SELECT *
       FROM wsf.psalmsongs_alternativetunes
       WHERE AlternativeTuneID IN ({keys*})"
  )
)
