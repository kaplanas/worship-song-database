#### Populate song search outputs ####

# Function that creates a panel for a specific song
create.song.panel = function(sid) {
  song.row = songs.df %>% filter(song.id ==  sid)
  panel.title = tags$span(HTML(song.row$panel.name), display = "flex",
                          justify = "space-between")
  panel.content = list(h1(song.row$song.name))
  if(!is.na(song.row$topics)) {
    panel.content[[length(panel.content) + 1]] = tags$p(song.row$topics)
  }
  song.instances = song.instances.df %>%
    filter(song.id == sid) %>%
    arrange(desc(num.entries))
  info.panels = list(tabPanel("Song info", lapply(song.instances$HTML, HTML)))
  lyrics.df = dynamodb.items.to.df(wsf.db, "wsf_songs_lyrics_tabs", "SongID",
                                   sid)
  if(nrow(lyrics.df) > 0) {
    lyrics.df = lyrics.df %>%
      dplyr::select(lyrics.order = LyricsOrder, lyrics.html = LyricsHTML,
                    tab.name = TabName) %>%
      mutate(non.english = grepl("[(]", tab.name)) %>%
      arrange(non.english, lyrics.order)
    if(nchar(input$lyricsSearch) >= 4) {
      parts = input$lyricsSearch
      if(input$lyricsOptions == "Parts") {
        parts = unlist(strsplit(parts, " "))
        parts = parts[nchar(parts) > 0]
      }
      for(p in parts) {
        lyrics.df = lyrics.df %>%
          mutate(lyrics.html = gsub(paste("(", p, ")", sep = ""),
                                    "<b><mark>\\1</mark></b>",
                                    lyrics.html, ignore.case = T))
      }
    }
    for(j in 1:nrow(lyrics.df)) {
      info.panels[[length(info.panels) + 1]] = tabPanel(lyrics.df$tab.name[j],
                                                        tags$p(),
                                                        HTML(lyrics.df$lyrics.html[j]))
    }
  }
  song.panel = tabPanel(tags$style(HTML("mark { background-color: yellow; }")),
                        title = panel.title, panel.content,
                        do.call(tabsetPanel, info.panels))
  return(song.panel)
}

# Filter and sort the song list
get.song.list.results = reactive({
  # Start with a list of all songs
  results.df = songs.df %>%
    select(song.id, panel.name)
  # Filter the songs
  source("R_files/filter_results.R", local = T)
  # Order the results
  results.df = inner_join(results.df, songs.df,
                          by = "song.id") %>%
    arrange(gsub("^['\"¡¿]", "", song.name))
  # Return the results
  results.df
})

# Populate the results list
output$songList = renderUI({
  results.df = get.song.list.results()
  if(nrow(results.df) > 0 &
     (nchar(input$songTitle) >= 3 |
      nchar(input$artistName) >= 3 |
      length(input$topicChoices) > 0 |
      (input$scriptureBook != 0 &
       (input$scriptureOptions == "Single verse" |
        as.numeric(input$scriptureVerseStart) <= as.numeric(input$scriptureVerseEnd))) |
      length(input$songbookChoices) > 0 |
      length(input$arrangementChoices) > 0 |
      length(input$languageChoices) > 0 |
      length(input$keyChoices) > 0 |
      length(input$timeChoices) > 0 |
      length(input$meterChoices) > 0 |
      nchar(input$tuneName) >= 3 |
      nchar(input$lyricsSearch) >= 4)) {
    n.songs = nrow(results.df)
    if(n.songs >= 50) {
      withProgress(message = "Loading songs", value = 0, {
        song.panels =
          lapply(1:n.songs,
                 function(i) {
                   sp = create.song.panel(results.df$song.id[i])
                   incProgress(1 / n.songs)
                   return(sp)
                 })
      })
    } else {
      song.panels =
        lapply(1:n.songs,
               function(i) {
                 sp = create.song.panel(results.df$song.id[i])
                 return(sp)
               })
    }
    song.panels[["widths"]] = c(5, 7)
    song.panels[["well"]] = F
    song.panels[["id"]] = "songResultsPanel"
    do.call(navlistPanel, song.panels)
  }
})

#### Populate psalm song outputs ####

# Populate the results list
get.psalm.song.list.results = reactive({
  # Get psalm songs for this psalm
  psalm.song.results.df = dynamodb.items.to.df(wsf.db, "wsf_psalmsongs",
                                               "PsalmNumber",
                                               as.numeric(input$psalmNumber)) %>%
    dplyr::select(psalm.song.id = PsalmSongID, psalm.number = PsalmNumber,
                  psalm.song.title = PsalmSongTitle, panel.name = PanelName,
                  psalm.song.type.id = PsalmSongTypeID, html.info = HTMLInfo,
                  html.alternatives = HTMLAlternatives) %>%
    filter(psalm.number == input$psalmNumber)
  if(length(input$psalmSongType) > 0) {
    psalm.song.results.df = psalm.song.results.df %>%
      filter(coalesce(psalm.song.type.id, as.integer(1)) %in% input$psalmSongType)
  }
  # Order the results
  psalm.song.results.df = psalm.song.results.df %>%
    arrange(psalm.song.title)
  # Return the results
  psalm.song.results.df
})
output$psalmSongList = renderUI({
  # Create the list of results
  psalm.song.results.df = get.psalm.song.list.results()
  psalm.song.panels =
    lapply(1:nrow(psalm.song.results.df),
           function(i) {
             main.title = h1(HTML(psalm.song.results.df$psalm.song.title[i]))
             info.panels = list(tabPanel("Song info",
                                         HTML(psalm.song.results.df$html.info[i])))
             lyrics.df = dynamodb.items.to.df(wsf.db,
                                              "wsf_psalmsongs_lyrics_tabs",
                                              "PsalmSongID",
                                              psalm.song.results.df$psalm.song.id[i])
             if(nrow(lyrics.df) > 0) {
               lyrics.df = lyrics.df %>%
                 dplyr::select(lyrics.order = LyricsOrder,
                               lyrics.html = LyricsHTML, tab.name = TabName) %>%
                 arrange(lyrics.order)
               for(j in 1:nrow(lyrics.df)) {
                 info.panels[[length(info.panels) + 1]] =
                   tabPanel(lyrics.df$tab.name[j], tags$p(),
                            HTML(lyrics.df$lyrics.html[j]))
               }
             }
             if(!is.na(psalm.song.results.df$html.alternatives[i])) {
               info.panels[[length(info.panels) + 1]] =
                 tabPanel("Alternative tunes",
                          HTML(psalm.song.results.df$html.alternatives[i]))
             }
             return(tabPanel(title = HTML(psalm.song.results.df$panel.name[i]),
                             list(main.title,
                                  do.call(tabsetPanel, info.panels))))
           })
  psalm.song.panels[["widths"]] = c(5, 7)
  psalm.song.panels[["well"]] = F
  psalm.song.panels[["id"]] = "psalmSongResultsPanel"
  temp.panels = do.call(navlistPanel, psalm.song.panels)
  # Assign classes based on the psalm song type (so css can assign colors)
  for(i in 1:nrow(psalm.song.results.df)) {
    current.class = NULL
    if("class" %in% names(temp.panels$children[[1]]$children[[1]]$children[[i]]$attribs)) {
      current.class = temp.panels$children[[1]]$children[[1]]$children[[i]]$attribs$class
    }
    new.class = paste("psalmsongtype",
                      coalesce(psalm.song.results.df$psalm.song.type.id[i],
                               as.integer(0)),
                      sep = "")
    if(is.null(current.class)) {
      current.class = new.class
    } else {
      current.class = paste(current.class, new.class, sep = " ")
    }
    temp.panels$children[[1]]$children[[1]]$children[[i]]$attribs$class = current.class
  }
  temp.panels
})
