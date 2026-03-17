list(
  write = list(
    wsf_psalmsongs =
      "SELECT *
       FROM wsf.psalmsongs
       WHERE MetricalPsalmID IN ({keys*})"
  ),
  delete = c("wsf_psalmsongs")
)
