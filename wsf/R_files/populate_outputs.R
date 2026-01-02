#### Populate song search outputs ####

# Populate the results list
get.song.list.results = reactive({
  # Start with a list of all songs
  results.df = songs.df %>%
    select(song.id, song.name)
  # Filter the songs
  source("R_files/filter_results.R", local = T)
  # Order the results
  results.df = inner_join(results.df, songs.df,
                          by = c("song.id", "song.name")) %>%
    arrange(song.name.sort)
  # Return the results
  results.df
})
output$songList = renderUI({
  
  # Create the list of results
  results.df = get.song.list.results()
  if(nrow(results.df) > 0 &
     (input$songTitle != "" |
      input$artistName != "" |
      length(input$topicChoices) > 0 |
      input$scriptureBook != 0 |
      length(input$songbookChoices) > 0 |
      length(input$arrangementChoices) > 0 |
      length(input$languageChoices) > 0 |
      length(input$keyChoices) > 0 |
      length(input$timeChoices) > 0 |
      length(input$meterChoices) > 0 |
      input$tuneName != "") |
     length(input$requestChoices) > 0) {
    song.panels =
      lapply(1:nrow(results.df),
             function(i) {
               return(tabPanel(title = all.song.panel.titles[[as.character(results.df$song.id[i])]],
                               all.song.panels[[as.character(results.df$song.id[i])]]))
             })
    song.panels[["widths"]] = c(5, 7)
    song.panels[["well"]] = F
    song.panels[["id"]] = "songResultsPanel"
    do.call(navlistPanel, song.panels)
  }
})

#### Populate psalm song outputs ####

# Populate the results list
get.psalm.song.list.results = reactive({
  # Start with a list of all psalm songs
  psalm.song.results.df = psalm.songs.df %>%
    select(psalm.song.id, psalm.number, song.id, psalm.song.type.id,
           psalm.song.title)
  # Filter the songs
  psalm.song.results.df = psalm.song.results.df %>%
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
             return(tabPanel(title = all.psalm.song.panel.titles[[as.character(psalm.song.results.df$psalm.song.id[i])]],
                             all.psalm.song.panels[[as.character(psalm.song.results.df$psalm.song.id[i])]]))
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
