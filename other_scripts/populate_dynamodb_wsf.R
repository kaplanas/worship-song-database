# Populate DynamoDB for WSF from scratch.

library(paws)
library(RMySQL)
library(aws.ec2metadata)
library(tidyverse)

db = dynamodb(endpoint = "dynamodb.us-west-2.api.aws")
wsdb.con = dbConnect(MySQL(), user = "root", password = Sys.getenv("WSDB_PWD"),
                     host = Sys.getenv("WSDB_HOST"), port = 3306,
                     dbname = "wsf")

# Function that writes to DynamoDB.
write.to.dynamodb = function(df, n.fields, s.fields, b.fields, table.name) {
  print(table.name)
  items = list()
  for(i in 1:nrow(df)) {
    item = list()
    for(field in c(n.fields, s.fields, b.fields)) {
      if(!is.na(df[i,field]) & df[i,field] != "") {
        if(field %in% n.fields) {
          item[[field]] = list(N = df[i,field])
        } else if(field %in% s.fields) {
          item[[field]] = list(S = df[i,field])
        } else if(field %in% b.fields) {
          item[[field]] = list(BOOL = df[i,field])
        }
      }
    }
    items[[length(items)+1]] = list(PutRequest = list(Item = item))
    if(length(items) == 25) {
      request.list = list()
      request.list[[table.name]] = items
      db$batch_write_item(RequestItems = request.list)
      items = list()
      print(paste(i, nrow(df), sep = " / "))
    }
  }
  if(length(items) > 0) {
    request.list = list()
    request.list[[table.name]] = items
    db$batch_write_item(RequestItems = request.list)
    print(paste(i, nrow(df), sep = " / "))
  }
}

# Table of songbooks.
songbooks.df = dbGetQuery(wsdb.con, "SELECT * FROM wsf.songbooks")
write.to.dynamodb(songbooks.df,
                  c("SongbookID"),
                  c("SongbookName", "SongbookAbbreviation"),
                  c(),
                  "wsf_songbooks")

# Table that connects song instances and songbooks.
song.instances.songbooks.df = dbGetQuery(wsdb.con,
                                         "SELECT * FROM wsf.songinstances_songbooks")
write.to.dynamodb(song.instances.songbooks.df,
                  c("SongbookEntryID", "SongInstanceID", "SongID", "SongbookID",
                    "SongbookVolumeID"),
                  c("SongbookName", "SongbookAbbreviation", "SongbookVolume",
                    "EntryNumber", "EntryString", "EntryStringNoName",
                    "SongInstanceIDEntryNumber"),
                  c(),
                  "wsf_songinstances_songbooks")

# Table that connects song instances and artists.
song.instances.artists.df = dbGetQuery(wsdb.con,
                                       "SELECT * FROM wsf.songinstances_artists")
write.to.dynamodb(song.instances.artists.df,
                  c("SongInstanceID", "SongID", "ArtistID"),
                  c("Role", "SongInstanceIDRole"),
                  c(),
                  "wsf_songinstances_artists")

# Table of artists.
artists.df = dbGetQuery(wsdb.con, "SELECT * FROM wsf.artists")
write.to.dynamodb(artists.df,
                  c("ArtistID"),
                  c("LastName", "FirstName"),
                  c(),
                  "wsf_artists")

# Table that connects song instances and tunes.
song.instances.tunes.df = dbGetQuery(wsdb.con, "SELECT * FROM wsf.songinstances_tunes")
write.to.dynamodb(song.instances.tunes.df,
                  c("SongInstanceID", "SongID", "TuneID"),
                  c(),
                  c(),
                  "wsf_songinstances_tunes")

# Table of tunes.
tunes.df = dbGetQuery(wsdb.con, "SELECT * FROM wsf.tunes")
write.to.dynamodb(tunes.df,
                  c("TuneID"),
                  c("TuneName"),
                  c("RealTuneName"),
                  "wsf_tunes")

# Table that connects song instances and key signatures.
song.instances.key.signatures.df = dbGetQuery(wsdb.con,
                                              "SELECT * FROM wsf.songinstances_keysignatures")
write.to.dynamodb(song.instances.key.signatures.df,
                  c("SongInstanceID", "SongID", "KeySignatureID"),
                  c(),
                  c(),
                  "wsf_songinstances_keysignatures")

# Table of key signatures.
key.signatures.df = dbGetQuery(wsdb.con, "SELECT * FROM wsf.keysignatures")
write.to.dynamodb(key.signatures.df,
                  c("KeySignatureID", "AccidentalID", "ModeID"),
                  c("PitchName", "AccidentalSymbol", "ModeName",
                    "KeySignatureString"),
                  c(),
                  "wsf_keysignatures")

# Table that connects song instances and time signatures.
song.instances.time.signatures.df = dbGetQuery(wsdb.con,
                                               "SELECT * FROM wsf.songinstances_timesignatures")
write.to.dynamodb(song.instances.time.signatures.df,
                  c("SongInstanceID", "SongID", "TimeSignatureID"),
                  c(),
                  c(),
                  "wsf_songinstances_timesignatures")

# Table of time signatures.
time.signatures.df = dbGetQuery(wsdb.con,
                                "SELECT * FROM wsf.timesignatures")
write.to.dynamodb(time.signatures.df,
                  c("TimeSignatureID", "TimeSignatureBeat",
                    "TimeSignatureMeasure"),
                  c("TimeSignatureString"),
                  c(),
                  "wsf_timesignatures")

# Table that connects song instances and meters.
song.instances.meters.df = dbGetQuery(wsdb.con,
                                      "SELECT * FROM wsf.songinstances_meters")
write.to.dynamodb(song.instances.meters.df,
                  c("SongInstanceID", "SongID", "MeterID"),
                  c(),
                  c(),
                  "wsf_songinstances_meters")

# Table of meters.
#meters.df = dbGetQuery(wsdb.con, "SELECT * FROM wsf.meters")
write.to.dynamodb(meters.df,
                  c("MeterID", "TotalSongs"),
                  c("Meter", "SortString", "Multiplier", "MeterString"),
                  c(),
                  "wsf_meters")

# Table that connects song instances and scripture references.
song.instances.scripture.references.df = dbGetQuery(wsdb.con,
                                                    "SELECT * FROM wsf.songinstances_scripturereferences")
write.to.dynamodb(song.instances.scripture.references.df,
                  c("SongInstanceID", "SongID", "ScriptureReferenceID"),
                  c(),
                  c(),
                  "wsf_songinstances_scripturereferences")

# Table of books of the Bible.
bible.books.df = dbGetQuery(wsdb.con, "SELECT * FROM wsf.bible_books")
write.to.dynamodb(bible.books.df,
                  c("BookID"),
                  c("BookName", "BookAbbreviation"),
                  c(),
                  "wsf_bible_books")

# Table of scripture references.
scripture.references.df = dbGetQuery(wsdb.con,
                                     "SELECT * FROM wsf.scripturereferences")
write.to.dynamodb(scripture.references.df,
                  c("ScriptureReferenceID", "BookID", "Chapter", "Verse"),
                  c("BookName", "BookAbbreviation"),
                  c(),
                  "wsf_scripturereferences")

# Table that connects song instances and languages.
song.instances.languages.df = dbGetQuery(wsdb.con, 
                                         "SELECT * FROM wsf.songinstances_languages")
write.to.dynamodb(song.instances.languages.df,
                  c("SongInstanceID", "SongID", "LanguageID"),
                  c(),
                  c(),
                  "wsf_songinstances_languages")

# Table of languages.
languages.df = dbGetQuery(wsdb.con, "SELECT * FROM wsf.languages")
write.to.dynamodb(languages.df,
                  c("LanguageID"),
                  c("LanguageName"),
                  c(),
                  "wsf_languages")

# Table that connects song instances and arrangement types.
song.instances.arrangement.types.df = dbGetQuery(wsdb.con,
                                                 "SELECT * FROM wsf.songinstances_arrangementtypes")
write.to.dynamodb(song.instances.arrangement.types.df,
                  c("SongInstanceID", "SongID", "ArrangementTypeID"),
                  c(),
                  c(),
                  "wsf_songinstances_arrangementtypes")

# Table of arrangement types.
arrangement.types.df = dbGetQuery(wsdb.con,
                                  "SELECT * FROM wsf.arrangementtypes")
write.to.dynamodb(arrangement.types.df,
                  c("ArrangementTypeID"),
                  c("ArrangementType"),
                  c(),
                  "wsf_arrangementtypes")

# Table of lyrics first lines for all song instances.
lyrics.first.lines.df = dbGetQuery(wsdb.con,
                                   "SELECT * FROM wsf.lyrics_first_lines")
write.to.dynamodb(lyrics.first.lines.df,
                  c("SongInstanceID", "FirstLineOrder", "LyricsID"),
                  c("FirstLine", "LyricsIDFirstLineOrder"),
                  c(),
                  "wsf_lyrics_first_lines")

# Table of song instances.
song.instances.df = dbGetQuery(wsdb.con, "SELECT * FROM wsf.songinstances")
write.to.dynamodb(song.instances.df,
                  c("SongInstanceID", "SongID", "LastLyricsYear",
                    "LastTuneYear", "NumEntries"),
                  c("SongInstance", "HTML"),
                  c(),
                  "wsf_songinstances")

# Table that connects songs and topics.
songs.topics.df = dbGetQuery(wsdb.con, "SELECT * FROM wsf.songs_topics")
write.to.dynamodb(songs.topics.df,
                  c("SongID", "TopicID"),
                  c(),
                  c(),
                  "wsf_songs_topics")

# Table of topics.
topics.df = dbGetQuery(wsdb.con, "SELECT * FROM wsf.topics")
write.to.dynamodb(topics.df,
                  c("TopicID"),
                  c("TopicName"),
                  c(),
                  "wsf_topics")

# Table of songs.
songs.df = dbGetQuery(wsdb.con, "SELECT * FROM wsf.songs")
write.to.dynamodb(songs.df,
                  c("SongID"),
                  c("SongName", "SongDisambiguator", "SongNameUnique",
                    "SongNameSort", "PanelName", "Copyrighted",
                    "SongbookEntries", "Topics"),
                  c(),
                  "wsf_songs")

# Table of lyrics tabs for psalm songs.
psalmsongs.lyrics.tabs.df = dbGetQuery(wsdb.con,
                                       "SELECT * FROM wsf.psalmsongs_lyrics_tabs")
write.to.dynamodb(psalmsongs.lyrics.tabs.df,
                  c("LyricsOrder"),
                  c("PsalmSongID", "LyricsHTML"),
                  c(),
                  "wsf_psalmsongs_lyrics_tabs")

# Table of psalm song types.
psalmsongtypes.df = dbGetQuery(wsdb.con, "SELECT * FROM wsf.psalmsongtypes")
write.to.dynamodb(psalmsongtypes.df,
                  c("PsalmSongTypeID"),
                  c("PsalmSongType"),
                  c(),
                  "wsf_psalmsongtypes")

# Table that connects psalm songs and alternative tunes.
psalmsongs.alternativetunes.df = dbGetQuery(wsdb.con,
                                            "SELECT * FROM wsf.psalmsongs_alternativetunes")
write.to.dynamodb(psalmsongs.alternativetunes.df,
                  c("AlternativeTuneID", "TuneID"),
                  c("PsalmSongID", "Notes"),
                  c(),
                  "wsf_psalmsongs_alternativetunes")

# Table of psalm songs.
psalmsongs.df = dbGetQuery(wsdb.con, "SELECT * FROM wsf.psalmsongs")
write.to.dynamodb(psalmsongs.df,
                  c("PsalmNumber", "SongID", "MetricalPsalmID",
                    "PsalmSongTypeID"),
                  c("PsalmSongID", "SongOrMetricalPsalmID", "PsalmSongType",
                    "PsalmSongTitle", "PanelName", "PrettyScriptureList",
                    "Artists", "HTMLInfo", "HTMLAlternatives"),
                  c(),
                  "wsf_psalmsongs")
