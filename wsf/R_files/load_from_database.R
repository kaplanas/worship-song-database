# Function that converts data from DynamoDB to a dataframe
dynamodb.to.df = function(db, table.name, start.key = NULL) {
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
    next.page.df = dynamodb.to.df(db, table.name, db.result$LastEvaluatedKey)
    return(bind_rows(df, next.page.df))
  } else {
    return(df)
  }
}

#### Song instance info ####

# Get table of song instances
print("song instances")
song.instances.df = dynamodb.to.df(wsf.db, "wsf_songinstances") %>%
  dplyr::select(song.instance.id = SongInstanceID, song.instance = SongInstance,
                song.id = SongID, last.lyrics.year = LastLyricsYear,
                last.tune.year = LastTuneYear, num.entries = NumEntries,
                html = HTML)

# Get table of artists
print("artists")
artists.df = dynamodb.to.df(wsf.db, "wsf_artists") %>%
  dplyr::select(artist.id = ArtistID, last.name = LastName,
                first.name = FirstName)

# Get table of books of the Bible
print("books of the bible")
bible.books.df = dynamodb.to.df(wsf.db, "wsf_bible_books") %>%
  dplyr::select(book.id = BookID, book.name = BookName,
                book.abbreviation = BookAbbreviation) %>%
  arrange(book.id)

# Get table of scripture references
print("scripture references")
scripture.references.df = dynamodb.to.df(wsf.db, "wsf_scripturereferences") %>%
  dplyr::select(scripture.reference.id = ScriptureReferenceID, book.id = BookID,
                book.name = BookName, book.abbreviation = BookAbbreviation,
                chapter = Chapter, verse = Verse) %>%
  arrange(book.id, chapter, verse)

# Get table of languages
print("languages")
languages.df = dynamodb.to.df(wsf.db, "wsf_languages") %>%
  dplyr::select(language.id = LanguageID, language.name = LanguageName)

# Get table of songbooks
print("songbooks")
songbooks.df = dynamodb.to.df(wsf.db, "wsf_songbooks") %>%
  dplyr::select(songbook.id = SongbookID, songbook.name = SongbookName,
                songbook.abbreviation = SongbookAbbreviation)

# Get table of arrangement types
print("arrangement types")
arrangement.types.df = dynamodb.to.df(wsf.db, "wsf_arrangementtypes") %>%
  dplyr::select(arrangement.type.id = ArrangementTypeID,
                arrangement.type = ArrangementType)

# Get table of key signatures
print("key signatures")
key.signatures.df = dynamodb.to.df(wsf.db, "wsf_keysignatures") %>%
  dplyr::select(key.signature.id = KeySignatureID, pitch.name = PitchName,
                accidental.id = AccidentalID,
                accidental.symbol = AccidentalSymbol, mode.id = ModeID,
                mode.name = ModeName, key.signature.string = KeySignatureString)

# Get table of time signatures
print("time signatures")
time.signatures.df = dynamodb.to.df(wsf.db, "wsf_timesignatures") %>%
  dplyr::select(time.signature.id = TimeSignatureID,
                time.signature.beat = TimeSignatureBeat,
                time.signature.measure = TimeSignatureMeasure,
                time.signature.string = TimeSignatureString)

# Get table of meters
print("meters")
meters.df = dynamodb.to.df(wsf.db, "wsf_meters") %>%
  dplyr::select(meter.id = MeterID, meter = Meter, sort.string = SortString,
                multiplier = Multiplier, meter.string = MeterString,
                total.songs = TotalSongs)

# Get table of tunes
print("tunes")
tunes.df = dynamodb.to.df(wsf.db, "wsf_tunes") %>%
  filter(RealTuneName) %>%
  dplyr::select(tune.id = TuneID, tune.name = TuneName, RealTuneName)

#### Song info ####

# Get table of songs
print("songs")
songs.df = dynamodb.to.df(wsf.db, "wsf_songs") %>%
  dplyr::select(song.id = SongID, song.name = SongName,
                song.disambiguator = SongDisambiguator,
                song.name.unique = SongNameUnique,
                song.name.sort = SongNameSort, panel.name = PanelName,
                topics = Topics)

# Get table of topics
print("topics")
topics.df = dynamodb.to.df(wsf.db, "wsf_topics") %>%
  dplyr::select(topic.id = TopicID, topic.name = TopicName)

#### Psalm song info ####

# Get table of psalm songs
print("psalm songs")
psalm.songs.df = dynamodb.to.df(wsf.db, "wsf_psalmsongs") %>%
  dplyr::select(psalm.song.id = PsalmSongID, psalm.number = PsalmNumber,
                song.id = SongID, psalm.song.type.id = PsalmSongTypeID,
                psalm.song.type = PsalmSongType,
                psalm.song.title = PsalmSongTitle, panel.name = PanelName,
                pretty.scripture.list = PrettyScriptureList, artists = Artists,
                html.info = HTMLInfo, html.alternatives = HTMLAlternatives)

# Get table that connects psalm songs and lyrics
psalm.songs.lyrics.sql = "SELECT PsalmSongID, TabName, LyricsHTML
                          FROM psalmsongs_lyrics_tabs"
psalm.songs.lyrics.df = dbGetQuery(wsf.shiny.con, psalm.songs.lyrics.sql) %>%
  dplyr::select(psalm.song.id = PsalmSongID, tab.name = TabName,
                full.lyrics = LyricsHTML)
Encoding(psalm.songs.lyrics.df$tab.name) = "UTF-8"
Encoding(psalm.songs.lyrics.df$full.lyrics) = "UTF-8"

# Get table of psalm song types
psalm.song.types.sql = "SELECT PsalmSongTypeID, PsalmSongType
                        FROM psalmsongtypes"
psalm.song.types.df = dbGetQuery(wsf.shiny.con, psalm.song.types.sql) %>%
  dplyr::select(psalm.song.type.id = PsalmSongTypeID,
                psalm.song.type = PsalmSongType)
Encoding(psalm.song.types.df$psalm.song.type) = "UTF-8"

# Get table of alternative tunes
alternative.tunes.sql = "SELECT PsalmSongID, TuneID, TuneDisplayName, Notes
                         FROM psalmsongs_alternativetunes"
alternative.tunes.df = dbGetQuery(wsf.shiny.con, alternative.tunes.sql) %>%
  dplyr::select(psalm.song.id = PsalmSongID, tune.id = TuneID,
                tune.display.name = TuneDisplayName, notes = Notes)
Encoding(alternative.tunes.df$tune.display.name) = "UTF-8"
Encoding(alternative.tunes.df$notes) = "UTF-8"

#### Collect song info into pretty formats ####

# Get info for all song instances
song.instance.info.df = song.instances.df %>%
  mutate(year = ifelse(is.na(last.lyrics.year) & is.na(last.tune.year),
                       NA, pmax(last.lyrics.year, last.tune.year)),
         decade = floor(year / 10) * 10) %>%
  arrange(desc(num.entries)) %>%
  dplyr::select(song.id, song.instance.id, title = song.instance, year, decade,
                num.entries)

# Get info for all songs
song.info.df = songs.df %>%
  left_join(song.instances.df %>%
              group_by(song.instance.id, song.id) %>%
              summarise(last.lyrics.year = max(last.lyrics.year),
                        last.tune.year = max(last.tune.year),
                        .groups = "drop") %>%
              group_by(song.id) %>%
              summarise(last.lyrics.year = min(last.lyrics.year),
                        last.tune.year = min(last.tune.year),
                        .groups = "drop"),
            by = c("song.id")) %>%
  mutate(year = ifelse(is.na(last.lyrics.year) & is.na(last.tune.year),
                       NA, pmax(last.lyrics.year, last.tune.year)),
         decade = floor(year / 10) * 10) %>%
  dplyr::select(song.id, title = song.name, panel.name, topics, year, decade)
