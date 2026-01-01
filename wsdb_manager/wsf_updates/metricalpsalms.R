list(
  write = list(
    wsf_psalmsongs =
      "SELECT *
       FROM wsf.psalmsongs
       WHERE MetricalPsalmID IN ({keys*})"
  ),
  delete = "wsf_psalmsongs"
)
