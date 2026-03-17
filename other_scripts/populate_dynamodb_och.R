# Populate DynamoDB for OCH from scratch.

library(paws)
library(RMySQL)
library(aws.ec2metadata)
library(tidyverse)

db = dynamodb(endpoint = "dynamodb.us-west-2.api.aws")
wsdb.con = dbConnect(MySQL(), user = "root", password = Sys.getenv("WSDB_PWD"),
                     host = Sys.getenv("WSDB_HOST"), port = 3306,
                     dbname = "och")
dbSendQuery(wsdb.con, "SET SESSION group_concat_max_len = 1000000")

write.history.to.dynamodb = function(old.df, congregation.username,
                                     congregation.id) {
  df = old.df %>%
    filter(CongregationID == congregation.id) %>%
    group_by(WorshipDate) %>%
    arrange(WorshipHistoryID) %>%
    mutate(Congregation = congregation.username,
           WorshipDate = as.numeric(format(ymd(WorshipDate), "%Y%m%d")),
           HistoryID = as.numeric(paste(WorshipDate,
                                        str_pad(row_number(), 4, "left", "0"),
                                        sep = "")),
           FileName = gsub("church_of_christ_[0-9]+_", "", FileName),
           ProcessedRecord = as.logical(Processed),
           Notes = AmbiguousSong) %>%
    ungroup() %>%
    data.frame()
  items = list()
  for(i in 1:nrow(df)) {
    item = list()
    for(field in c("Congregation", "RawLine", "Notes", "FileName")) {
      if(!is.na(df[i,field])) {
        item[[field]] = list(S = df[i,field])
      }
    }
    for(field in c("HistoryID", "WorshipDate", "SongID", "SongInstanceID")) {
      if(!is.na(df[i,field])) {
        item[[field]] = list(N = df[i,field])
      }
    }
    for(field in c("ProcessedRecord", "SundayMorning")) {
      if(!is.na(df[i,field])) {
        item[[field]] = list(BOOL = df[i,field])
      }
    }
    items[[length(items)+1]] = list(PutRequest = list(Item = item))
    if(length(items) == 25) {
      request.list = list()
      request.list[["och_history"]] = items
      db$batch_write_item(RequestItems = request.list)
      items = list()
      print(paste(i, nrow(df), sep = " / "))
    }
  }
  if(length(items) > 0) {
    request.list = list()
    request.list[["och_history"]] = items
    db$batch_write_item(RequestItems = request.list)
    print(paste(i, nrow(df), sep = " / "))
  }
}

write.dates.to.dynamodb = function(old.df, congregation.username,
                                   congregation.id) {
  df = old.df %>%
    filter(CongregationID == congregation.id) %>%
    group_by(WorshipDate) %>%
    summarise(AnyUnprocessed = any(!as.logical(Processed)),
              AnyNotes = any(!is.na(AmbiguousSong) & AmbiguousSong != ""),
              .groups = "drop") %>%
    mutate(Congregation = congregation.username,
           WorshipDate = as.numeric(format(ymd(WorshipDate), "%Y%m%d"))) %>%
    data.frame()
  items = list()
  for(i in 1:nrow(df)) {
    item = list()
    for(field in c("Congregation")) {
      if(!is.na(df[i,field])) {
        item[[field]] = list(S = df[i,field])
      }
    }
    for(field in c("WorshipDate")) {
      if(!is.na(df[i,field])) {
        item[[field]] = list(N = df[i,field])
      }
    }
    for(field in c("AnyUnprocessed", "AnyNotes")) {
      if(!is.na(df[i,field])) {
        item[[field]] = list(BOOL = df[i,field])
      }
    }
    items[[length(items)+1]] = list(PutRequest = list(Item = item))
    if(length(items) == 25) {
      request.list = list()
      request.list[["och_dates"]] = items
      db$batch_write_item(RequestItems = request.list)
      items = list()
      print(paste(i, nrow(df), sep = " / "))
    }
  }
  if(length(items) > 0) {
    request.list = list()
    request.list[["och_dates"]] = items
    db$batch_write_item(RequestItems = request.list)
    print(paste(i, nrow(df), sep = " / "))
  }
}

#history.df = readRDS("och.history.df.rds") %>%
  #mutate(SundayMorning = wday(ymd(WorshipDate), label = T) == "Sun")
#write.history.to.dynamodb(history.df, "windsong", 64)
#write.dates.to.dynamodb(history.df, "windsong", 64)

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

#songs.df = dbGetQuery(wsdb.con, "SELECT * FROM och.song_labels")
#write.to.dynamodb(songs.df,
                  #c("SongID"),
                  #c("SongLabel"),
                  #c(),
                  #"och_songs")

#songinstances.df = dbGetQuery(wsdb.con, "SELECT * FROM och.songinstance_labels")
#write.to.dynamodb(songinstances.df,
                  #c("SongInstanceID", "SongID"),
                  #c("SongInstanceLabel"),
                  #c(),
                  #"och_songinstances")

#song.titles.df = dbGetQuery(wsdb.con, "SELECT * FROM och.song_titles")
#write.to.dynamodb(song.titles.df,
                  #c("SongID"),
                  #c("SongTitles"),
                  #c(),
                  #"och_songtitles")

song.info.df = dbGetQuery(wsdb.con, "SELECT * FROM och.song_info")
song.topics.df = dbGetQuery(wsdb.con, "SELECT * FROM och.songs_topics")
song.info.topics.df = song.info.df %>%
  left_join(song.topics.df %>%
              mutate(has.topic = T) %>%
              pivot_wider(names_from = "Topic", values_from = "has.topic"),
            by = "SongID")
write.to.dynamodb(song.info.topics.df,
                  c("SongID", "Year"),
                  c("SongName"),
                  sort(unique(song.topics.df$Topic)),
                  "och_song_info")
