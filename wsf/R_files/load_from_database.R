# Function that converts data from an entire DynamoDB table to a dataframe
dynamodb.table.to.df = function(db, table.name, start.key = NULL) {
  if(is.null(start.key)) {
    db.result = db$scan(Table = table.name)
  } else {
    db.result = db$scan(Table = table.name, ExclusiveStartKey = start.key)
  }
  df = map_dfr(
    db.result$Items,
    function(item) {
      map(
        item,
        function(field) {
          if(length(field$N) > 0) {
            as.numeric(field$N)
          } else if(length(field$S) > 0) {
            field$S
          } else {
            as.logical(field$BOOL)
          }
        }
      )
    }
  )
  if(length(db.result$LastEvaluatedKey) > 0) {
    next.page.df = dynamodb.table.to.df(db, table.name,
                                        db.result$LastEvaluatedKey)
    return(bind_rows(df, next.page.df))
  } else {
    return(df)
  }
}

# Function that converts data from DynamoDB to a dataframe
dynamodb.items.to.df = function(db, table.name, partition.key,
                                partition.key.value, start.key = NULL) {
  key.condition.expression = paste(partition.key, "= :v")
  partition.key.class = list("character" = "S", "integer" = "N", "numeric" = "N")[[class(partition.key.value)]]
  eav = list()
  eav[[partition.key.class]] = partition.key.value
  expression.attribute.values = list(":v" = eav)
  if(is.null(start.key)) {
    db.result = db$query(TableName = table.name,
                         ExpressionAttributeValues = expression.attribute.values,
                         KeyConditionExpression = key.condition.expression)
  } else {
    db.result = db$query(TableName = table.name,
                         ExpressionAttributeValues = expression.attribute.values,
                         KeyConditionExpression = key.condition.expression,
                         ExclusiveStartKey = start.key)
  }
  df = map_dfr(
    db.result$Items,
    function(item) {
      map(
        item,
        function(field) {
          if(length(field$N) > 0) {
            as.numeric(field$N)
          } else if(length(field$S) > 0) {
            field$S
          } else {
            as.logical(field$BOOL)
          }
        }
      )
    }
  )
  if(length(db.result$LastEvaluatedKey) > 0) {
    next.page.df = dynamodb.items.to.df(db, table.name, partition.key,
                                        partition.key.value,
                                        db.result$LastEvaluatedKey)
    return(bind_rows(df, next.page.df))
  } else {
    return(df)
  }
}

#### Song instance info ####

# Get table of books of the Bible
bible.books.df = dynamodb.table.to.df(wsf.db, "wsf_bible_books") %>%
  dplyr::select(book.id = BookID, book.name = BookName,
                book.abbreviation = BookAbbreviation) %>%
  arrange(book.id)

# Get table of languages
languages.df = dynamodb.table.to.df(wsf.db, "wsf_languages") %>%
  dplyr::select(language.id = LanguageID, language.name = LanguageName)

# Get table of songbooks
songbooks.df = dynamodb.table.to.df(wsf.db, "wsf_songbooks") %>%
  dplyr::select(songbook.id = SongbookID, songbook.name = SongbookName,
                songbook.abbreviation = SongbookAbbreviation)

# Get table of arrangement types
arrangement.types.df = dynamodb.table.to.df(wsf.db, "wsf_arrangementtypes") %>%
  dplyr::select(arrangement.type.id = ArrangementTypeID,
                arrangement.type = ArrangementType)

# Get table of key signatures
key.signatures.df = dynamodb.table.to.df(wsf.db, "wsf_keysignatures") %>%
  dplyr::select(key.signature.id = KeySignatureID, pitch.name = PitchName,
                accidental.id = AccidentalID,
                accidental.symbol = AccidentalSymbol, mode.id = ModeID,
                mode.name = ModeName, key.signature.string = KeySignatureString)

# Get table of time signatures
time.signatures.df = dynamodb.table.to.df(wsf.db, "wsf_timesignatures") %>%
  dplyr::select(time.signature.id = TimeSignatureID,
                time.signature.beat = TimeSignatureBeat,
                time.signature.measure = TimeSignatureMeasure,
                time.signature.string = TimeSignatureString)

# Get table of meters
meters.df = dynamodb.table.to.df(wsf.db, "wsf_meters") %>%
  dplyr::select(meter.id = MeterID, sort.string = SortString,
                meter.string = MeterString)

#### Song info ####

# Get table of songs
songs.df = dynamodb.table.to.df(wsf.db, "wsf_songs") %>%
  dplyr::select(song.id = SongID, song.name = SongName,
                panel.name = PanelName, topics = Topics)

song.instances.df = dynamodb.table.to.df(wsf.db, "wsf_songinstances") %>%
  dplyr::select(song.instance.id = SongInstanceID, song.id = SongID, HTML,
                num.entries = NumEntries)

# Get table of topics
topics.df = dynamodb.table.to.df(wsf.db, "wsf_topics") %>%
  dplyr::select(topic.id = TopicID, topic.name = TopicName)

#### Psalm song info ####

# Get table of psalm song types
psalm.song.types.df = dynamodb.table.to.df(wsf.db, "wsf_psalmsongtypes") %>%
  dplyr::select(psalm.song.type.id = PsalmSongTypeID,
                psalm.song.type = PsalmSongType)
