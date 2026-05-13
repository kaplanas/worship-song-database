list(
  write = list(
    wsf_psalmsongs =
      "SELECT *
       FROM wsf.psalmsongs
       WHERE PsalmSongID IN
             (SELECT CONCAT('PS', psalmsongs.PsalmSongID)
              FROM wsdb.alternativetunes
                   JOIN wsdb.psalmsongs
                   ON alternativetunes.SongID = psalmsongs.SongID
              WHERE alternativetunes.AlternativeTuneID IN ({keys*}))
             OR PsalmSongID IN
             (SELECT CONCAT('MP', metricalpsalms.MetricalPsalmID)
              FROM wsdb.alternativetunes
                   JOIN wsdb.metricalpsalms
                   ON alternativetunes.MetricalPsalmID = metricalpsalms.MetricalPsalmID
              WHERE alternativetunes.AlternativeTuneID IN ({keys*}))"
  )
)
