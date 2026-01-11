# Filter by song title
if(nchar(input$songTitle) >= 3) {
  if(input$songTitleOptions == "String") {
    parts = c(input$songTitle)
  }
  else {
    parts = unlist(strsplit(input$songTitle, " "))
    if(input$songTitleOptions == "Whole words") {
      parts = paste("\\b", parts, "\\b", sep = "")
    }
  }
  for(part in parts) {
    results.df = results.df %>%
      separate_rows(panel.name, sep = "<br/>") %>%
      mutate(panel.name = gsub("<[a-z]+/?>", "", panel.name)) %>%
      filter(grepl(part, panel.name, ignore.case = T)) %>%
      dplyr::select(song.id) %>%
      distinct()
  }
}

# Filter by artist
if(nchar(input$artistName) >= 3) {
  parts = unlist(strsplit(input$artistName, " "))
  parts = parts[nchar(parts) > 0]
  expression.attribute.values = lapply(
    set_names(parts, paste(":p", 1:length(parts), sep = "")),
    function(p) { list(S = str_to_lower(p)) }
  )
  filter.expression = paste(paste("contains(ArtistNameLower, :p",
                                  1:length(parts), ")", sep = ""),
                            collapse = " OR ")
  artist.ids = wsf.db$scan(TableName = "wsf_artists",
                           ExpressionAttributeValues = expression.attribute.values,
                           FilterExpression = filter.expression)
  artist.ids = map_int(artist.ids$Items,
                       function(x) { as.numeric(x$ArtistID$N) })
  song.ids = do.call(
    c,
    map(artist.ids,
        function(ai) {
          query.results = wsf.db$query(TableName = "wsf_songinstances_artists",
                                       ExpressionAttributeValues = list(`:a` = list(N = ai),
                                                                        `:r` = list(S = input$artistNameOptions),
                                                                        `:any` = list(S = "any")),
                                       KeyConditionExpression = "ArtistID = :a",
                                       ProjectionExpression = "SongID",
                                       FilterExpression = "ArtistRole = :r OR :r = :any")
          map_int(
            query.results$Items,
            function(item) {
              as.numeric(item$SongID$N)
            }
          )
        }
      )
  )
  song.ids = unique(sort(song.ids))
  results.df = results.df %>%
    filter(song.id %in% song.ids)
}

# Filter by topic
if(length(input$topicChoices) >= 1) {
  song.ids = map(
    input$topicChoices,
    function(ti) {
      query.results = wsf.db$query(TableName = "wsf_songs_topics",
                                   ExpressionAttributeValues = list(`:t` = list(N = ti)),
                                   KeyConditionExpression = "TopicID = :t",
                                   ProjectionExpression = "SongID")
      map_int(
        query.results$Items,
        function(item) {
          as.numeric(item$SongID$N)
        }
      )
    }
  )
  if(input$topicOptions == "Match all" & length(input$topicChoices) > 1) {
    song.ids = do.call(intersect, song.ids)
  }
  else {
    song.ids = unique(sort(do.call(c, song.ids)))
  }
  results.df = results.df %>%
    filter(song.id %in% song.ids)
}

# Filter by scripture reference
if(input$scriptureBook != 0 &
   (input$scriptureOptions == "Single verse" |
    as.numeric(input$scriptureVerseStart) <= as.numeric(input$scriptureVerseEnd))) {
  if(input$scriptureOptions == "Single verse") {
    scripture.reference.ids = scripture.references.df() %>%
      filter(chapter == input$scriptureChapter,
             verse == input$scriptureVerseStart) %>%
      pull(scripture.reference.id)
  }
  else {
    scripture.reference.ids = scripture.references.df() %>%
      filter(chapter == input$scriptureChapter,
             verse >= as.numeric(input$scriptureVerseStart),
             verse <= as.numeric(input$scriptureVerseEnd)) %>%
      pull(scripture.reference.id)
  }
  song.ids = do.call(
    c,
    map(
      scripture.reference.ids,
      function(sri) {
        query.results = wsf.db$query(TableName = "wsf_songinstances_scripturereferences",
                                     ExpressionAttributeValues = list(`:sr` = list(N = sri)),
                                     KeyConditionExpression = "ScriptureReferenceID = :sr",
                                     ProjectionExpression = "SongID")
        map_int(
          query.results$Items,
          function(item) {
            as.numeric(item$SongID$N)
          }
        )
      }
    )
  )
  song.ids = unique(sort(song.ids))
  results.df = results.df %>%
    filter(song.id %in% song.ids)
}

# Filter by language
if(length(input$languageChoices) >= 1) {
  song.ids = map(
    input$languageChoices,
    function(li) {
      query.results = wsf.db$query(TableName = "wsf_songinstances_languages",
                                   ExpressionAttributeValues = list(`:l` = list(N = li)),
                                   KeyConditionExpression = "LanguageID = :l",
                                   ProjectionExpression = "SongID")
      map_int(
        query.results$Items,
        function(item) {
          as.numeric(item$SongID$N)
        }
      )
    }
  )
  if(input$languageOptions == "Match all" & length(input$languageChoices) > 1) {
    song.ids = do.call(intersect, song.ids)
  }
  else {
    song.ids = unique(sort(do.call(c, song.ids)))
  }
  results.df = results.df %>%
    filter(song.id %in% song.ids)
}

# Filter by songbook
if(length(input$songbookChoices) >= 1) {
  song.ids = map(
    input$songbookChoices,
    function(si) {
      query.results = wsf.db$query(TableName = "wsf_songinstances_songbooks",
                                   ExpressionAttributeValues = list(`:s` = list(N = si)),
                                   KeyConditionExpression = "SongbookID = :s",
                                   ProjectionExpression = "SongID")
      map_int(
        query.results$Items,
        function(item) {
          as.numeric(item$SongID$N)
        }
      )
    }
  )
  if(input$songbookOptions == "Match all" & length(input$songbookChoices) > 1) {
    song.ids = do.call(intersect, song.ids)
  }
  else {
    song.ids = unique(sort(do.call(c, song.ids)))
  }
  results.df = results.df %>%
    filter(song.id %in% song.ids)
}

# Filter by arrangement type
if(length(input$arrangementChoices) >= 1) {
  if(input$arrangementOptions == "Include") {
    song.ids = do.call(
      c,
      map(
        input$arrangementChoices,
        function(ati) {
          query.results = wsf.db$query(TableName = "wsf_songinstances_arrangementtypes",
                                       ExpressionAttributeValues = list(`:at` = list(N = ati)),
                                       KeyConditionExpression = "ArrangementTypeID = :at",
                                       ProjectionExpression = "SongID")
          map_int(
            query.results$Items,
            function(item) {
              as.numeric(item$SongID$N)
            }
          )
        }
      )
    )
    song.ids = unique(sort(song.ids))
    results.df = results.df %>%
      filter(song.id %in% song.ids)
  }
  else {
    song.instance.ids = do.call(
      c,
      map(
        input$arrangementChoices,
        function(ati) {
          query.results = wsf.db$query(TableName = "wsf_songinstances_arrangementtypes",
                                       ExpressionAttributeValues = list(`:at` = list(N = ati)),
                                       KeyConditionExpression = "ArrangementTypeID = :at",
                                       ProjectionExpression = "SongInstanceID")
          map_int(
            query.results$Items,
            function(item) {
              as.numeric(item$SongInstanceID$N)
            }
          )
        }
      )
    )
    song.instance.ids = unique(sort(song.instance.ids))
    results.df = results.df %>%
      anti_join(song.instances.df %>%
                  mutate(excluded = song.instance.id %in% song.instance.ids) %>%
                  group_by(song.id) %>%
                  summarise(all.excluded = all(excluded), .groups = "drop") %>%
                  filter(all.excluded),
                by = "song.id")
  }
}

# Filter by key signature
if(length(input$keyChoices) >= 1) {
  if(input$keyOptions == "Match all" & length(input$keyChoices) > 1) {
    song.instance.ids = do.call(
      intersect,
      map(
        input$keyChoices,
        function(ksi) {
          query.results = wsf.db$query(TableName = "wsf_songinstances_keysignatures",
                                       ExpressionAttributeValues = list(`:ks` = list(N = ksi)),
                                       KeyConditionExpression = "KeySignatureID = :ks",
                                       ProjectionExpression = "SongInstanceID")
          map_int(
            query.results$Items,
            function(item) {
              as.numeric(item$SongInstanceID$N)
            }
          )
        }
      )
    )
    results.df = results.df %>%
      semi_join(song.instances.df %>%
                  filter(song.instance.id %in% song.instance.ids) %>%
                  dplyr::select(song.id) %>%
                  distinct(),
                by = "song.id")
  }
  else {
    song.ids = do.call(
      c,
      map(
        input$keyChoices,
        function(ksi) {
          query.results = wsf.db$query(TableName = "wsf_songinstances_keysignatures",
                                       ExpressionAttributeValues = list(`:ks` = list(N = ksi)),
                                       KeyConditionExpression = "KeySignatureID = :ks",
                                       ProjectionExpression = "SongID")
          map_int(
            query.results$Items,
            function(item) {
              as.numeric(item$SongID$N)
            }
          )
        }
      )
    )
    song.ids = unique(sort(song.ids))
    results.df = results.df %>%
      filter(song.id %in% song.ids)
  }
}

# Filter by time signature
if(length(input$timeChoices) >= 1) {
  if(input$timeOptions == "Match all" & length(input$timeChoices) > 1) {
    song.instance.ids = do.call(
      intersect,
      map(
        input$timeChoices,
        function(tsi) {
          query.results = wsf.db$query(TableName = "wsf_songinstances_timesignatures",
                                       ExpressionAttributeValues = list(`:ts` = list(N = tsi)),
                                       KeyConditionExpression = "TimeSignatureID = :ts",
                                       ProjectionExpression = "SongInstanceID")
          map_int(
            query.results$Items,
            function(item) {
              as.numeric(item$SongInstanceID$N)
            }
          )
        }
      )
    )
    results.df = results.df %>%
      semi_join(song.instances.df %>%
                  filter(song.instance.id %in% song.instance.ids) %>%
                  dplyr::select(song.id) %>%
                  distinct(),
                by = "song.id")
  }
  else {
    song.ids = do.call(
      c,
      map(
        input$timeChoices,
        function(tsi) {
          query.results = wsf.db$query(TableName = "wsf_songinstances_timesignatures",
                                       ExpressionAttributeValues = list(`:ts` = list(N = tsi)),
                                       KeyConditionExpression = "TimeSignatureID = :ts",
                                       ProjectionExpression = "SongID")
          map_int(
            query.results$Items,
            function(item) {
              as.numeric(item$SongID$N)
            }
          )
        }
      )
    )
    song.ids = unique(sort(song.ids))
    results.df = results.df %>%
      filter(song.id %in% song.ids)
  }
}

# Filter by meter
if(length(input$meterChoices) >= 1) {
  if(input$meterOptions == "Match all" & length(input$meterChoices) > 1) {
    song.instance.ids = do.call(
      intersect,
      map(
        input$meterChoices,
        function(mi) {
          query.results = wsf.db$query(TableName = "wsf_songinstances_meters",
                                       ExpressionAttributeValues = list(`:m` = list(N = mi)),
                                       KeyConditionExpression = "MeterID = :m",
                                       ProjectionExpression = "SongInstanceID")
          map_int(
            query.results$Items,
            function(item) {
              as.numeric(item$SongInstanceID$N)
            }
          )
        }
      )
    )
    results.df = results.df %>%
      semi_join(song.instances.df %>%
                  filter(song.instance.id %in% song.instance.ids) %>%
                  dplyr::select(song.id) %>%
                  distinct(),
                by = "song.id")
  }
  else {
    song.ids = do.call(
      c,
      map(
        input$meterChoices,
        function(mi) {
          query.results = wsf.db$query(TableName = "wsf_songinstances_meters",
                                       ExpressionAttributeValues = list(`:m` = list(N = mi)),
                                       KeyConditionExpression = "MeterID = :m",
                                       ProjectionExpression = "SongID")
          map_int(
            query.results$Items,
            function(item) {
              as.numeric(item$SongID$N)
            }
          )
        }
      )
    )
    song.ids = unique(sort(song.ids))
    results.df = results.df %>%
      filter(song.id %in% song.ids)
  }
}

# Filter by tune name
if(nchar(input$tuneName) >= 3) {
  if(input$tuneNameOptions == "String") {
    parts = c(input$tuneName)
  }
  else {
    parts = unlist(strsplit(input$tuneName, " "))
  }
  expression.attribute.values = lapply(
    set_names(parts, paste(":p", 1:length(parts), sep = "")),
    function(p) { list(S = str_to_lower(p)) }
  )
  filter.expression = paste(paste("contains(TuneNameLower, :p",
                                  1:length(parts), ")", sep = ""),
                            collapse = " OR ")
  tune.ids = wsf.db$scan(TableName = "wsf_tunes",
                         ExpressionAttributeValues = expression.attribute.values,
                         FilterExpression = filter.expression)
  tune.ids = map_int(tune.ids$Items,
                     function(x) { as.numeric(x$TuneID$N) })
  song.ids = do.call(
    c,
    map(
      tune.ids,
      function(ti) {
        query.results = wsf.db$query(TableName = "wsf_songinstances_tunes",
                                     ExpressionAttributeValues = list(`:t` = list(N = ti)),
                                     KeyConditionExpression = "TuneID = :t",
                                     ProjectionExpression = "SongID")
        map_int(
          query.results$Items,
          function(item) {
            as.numeric(item$SongID$N)
          }
        )
      }
    )
  )
  song.ids = unique(sort(song.ids))
  results.df = results.df %>%
    filter(song.id %in% song.ids)
}
