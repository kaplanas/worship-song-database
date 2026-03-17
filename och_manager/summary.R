#### Useful constants ####

color.me = "#008000"
color.other = "#800080"

#### Component panels ####

load.history.panel = tabPanel(
  "Load worship histories",
  fluidRow(
    column(4, uiOutput("view.wh.start.date")),
    column(4, uiOutput("view.wh.end.date")),
    column(1,
           actionButton("load.all.histories",
                        label = "Load worship history for other congregations"))
  ),
  tableOutput("wh.row.count"),
  downloadButton("download.history.all", "Download history")
)

worship.history.table = tabPanel(
  "Table of songs sung",
  DTOutput("my.worship.history")
)

number.sung.sunday = tabPanel(
  "Number of songs per Sunday",
  tabsetPanel(
    tabPanel(
      "This congregation",
      plotlyOutput("number.sung.sunday.me")
    ),
    tabPanel(
      "All congregations",
      plotlyOutput("number.sung.sunday.all")
    )
  )
)

top.songs = tabPanel(
  "Most frequently sung songs",
  tabsetPanel(
    tabPanel(
      "This congregation",
      fluidRow(
        column(6, numericInput("top.songs.me.n", "Top _ songs:", 5, 1, 10)),
        column(6, selectInput("top.songs.me.metric", "Measure of frequency:",
                              c("Number of times sung per year",
                                "Percent of songs sung")))
      ),
      fluidRow(plotlyOutput("top.songs.me", width = "80vw"))
    ),
    tabPanel(
      "All congregations",
      fluidRow(
        column(6, numericInput("top.songs.all.n", "Top _ songs:", 5, 1, 10)),
        column(6, selectInput("top.songs.all.metric", "Measure of frequency:",
                              c("Number of times sung per year",
                                "Percent of songs sung")))
      ),
      fluidRow(plotlyOutput("top.songs.all", width = "80vw"))
    ),
    tabPanel(
      "Most frequent songs overall",
      fluidRow(column(6,
                      numericInput("top.songs.overall.n", "Top _ songs:", 20, 5,
                                   100))),
      plotlyOutput("top.songs.overall")
    )
  )
)

hapaxes = tabPanel(
  "Songs sung exactly once",
  tabsetPanel(
    tabPanel(
      "This congregation",
      tableOutput("hapaxes.me"),
      DTOutput("hapax.me.list")
    ),
    tabPanel(
      "All congregations",
      fluidRow(
        column(6, selectInput("hapaxes.all.metric", "Measure of frequency:",
                              c("Number of times sung per year",
                                "Percent of worship history")))
      ),
      uiOutput("hapaxes.all"),
      DTOutput("hapax.all.list")
    )
  )
)

song.year = tabPanel(
  "Song year",
  tabsetPanel(
    tabPanel(
      "This congregation",
      plotlyOutput("song.year.me"),
      DTOutput("year.me.list")
    ),
    tabPanel(
      "All congregations",
      uiOutput("song.year.all"),
      DTOutput("year.all.list")
    )
  )
)

topics = tabPanel(
  "Topics",
  tabsetPanel(
    tabPanel(
      "This congregation",
      fluidRow(
        column(6, selectInput("topics.me.metric", "Measure of frequency:",
                              c("Number of songs",
                                "Number of times sung per year",
                                "Percent of worship history")))
      ),
      uiOutput("topics.me"),
      DTOutput("topics.me.list")
    ),
    tabPanel(
      "All congregations",
      fluidRow(
        column(6, selectInput("topics.all.topic", "Topic:", c())),
        column(6, selectInput("topics.all.metric", "Measure of frequency:",
                              c("Number of songs",
                                "Number of times sung per year",
                                "Percent of worship history")))
      ),
      uiOutput("topics.all"),
      DTOutput("topics.all.list")
    )
  )
)

#### Viewing page ####

summary.page = tabPanel("Summary",
                        navlistPanel(
                          load.history.panel,
                          worship.history.table,
                          number.sung.sunday,
                          top.songs,
                          hapaxes,
                          song.year,
                          topics,
                          well = F,
                          widths = c(2, 10)
                        ))

#### Useful functions ####

# Get dates we need to update the date picker
get.history.dates = function(current.dates.df, start.current, end.current) {
  highlighted.dates = current.dates.df$worship.date
  selected.start.date = min(highlighted.dates)
  selected.end.date = max(highlighted.dates)
  if(!is.null(start.current) && start.current %in% highlighted.dates) {
    selected.start.date = start.current
  }
  if(!is.null(end.current) && end.current %in% highlighted.dates) {
    selected.end.date = end.current
  }
  return(list(highlighted = highlighted.dates, start = selected.start.date,
              end = selected.end.date))
}

# Populate the worship history summary table
populate.wh = function(congregations.df, song.labels.df, view.wh.start.date,
                       view.wh.end.date, db, progress.file) {
  if(!is.null(db)) {
    min.history.id = as.numeric(paste(format(view.wh.start.date, "%Y%m%d"),
                                      "0000", sep = ""))
    max.history.id = as.numeric(paste(format(view.wh.end.date, "%Y%m%d"),
                                      "9999", sep = ""))
    df = map_dfr(
      1:nrow(congregations.df),
      function(i) {
        cong.id = congregations.df$congregation[i]
        cong.name = congregations.df$name[i]
        attr.vals = list(`:c` = list(S = cong.id),
                         `:h1` = list(N = min.history.id),
                         `:h2` = list(N = max.history.id),
                         `:t` = list(BOOL = T))
        key.expr = "Congregation = :c AND HistoryID BETWEEN :h1 AND :h2"
	filter.expr = "ProcessedRecord = :t AND SundayMorning = :t"
        songs.df = query.dynamodb(db = db, table.name = "och_history", 
                                  index.name = "och_history_songs",
                                  expression.attribute.values = attr.vals,
                                  key.condition.expression = key.expr,
                                  projection.expression = "WorshipDate, SongID",
                                  filter.expression = filter.expr)
        new.df = query.dynamodb(db = db, table.name = "och_history",
                                index.name = "och_history_newsongs",
                                expression.attribute.values = attr.vals,
                                key.condition.expression = key.expr,
                                projection.expression = "WorshipDate, NewSong",
                                filter.expression = paste(filter.expr,
                                                          "AND attribute_not_exists(SongID)"))
        notes.df = query.dynamodb(db = db, table.name = "och_history",
                                  index.name = "och_history_notes",
                                  expression.attribute.values = attr.vals,
                                  key.condition.expression = key.expr,
                                  projection.expression = "WorshipDate, Notes",
                                  filter.expression = paste(filter.expr,
                                                            "AND attribute_not_exists(SongID) AND attribute_not_exists(NewSong)"))
        df = bind_rows(songs.df, new.df, notes.df) %>%
          mutate(congregation = cong.id,
                 congregation.name = cong.name)
        writeLines(as.character(i / nrow(congregations.df)),
                   con = progress.file)
        df
      }
    )
    if(!("NewSong" %in% colnames(df))) {
      df$NewSong = NA
    }
    if(!("Notes" %in% colnames(df))) {
      df$Notes = NA
    }
    df = df %>%
      inner_join(congregations.df, by = "congregation") %>%
      group_by(congregation, is.me) %>%
      filter(n_distinct(WorshipDate) >= 52 | is.me) %>%
      ungroup() %>%
      left_join(song.labels.df, by = "SongID") %>%
      mutate(worship.date = ymd(as.character(WorshipDate)),
             song.label = coalesce(SongLabel, NewSong, Notes),
             song.title = coalesce(gsub(".*<b>(.*)</b>.*", "\\1", SongLabel),
                                   paste("(", coalesce(NewSong, Notes), ")",
                                         sep = ""))) %>%
      dplyr::select(is.me, congregation.name, congregation.label, size, praise,
                    women, worship.date, song.id = SongID, song.label,
                    song.title)
    return(df)
  } else {
    return(NULL)
  }
}

# Populate the song info table
populate.song.info = function(db) {
  if(!is.null(db)) {
    tryCatch(
      {
        df = scan.dynamodb(db, table.name = "och_song_info")
        return(df)
      },
      error = function(err) {
        print(err)
        return(NULL)
      }
    )
  } else {
    return(NULL)
  }
}

# Create the worship history table
my.wh.table = function(wh.df) {
  wh.df %>%
    filter(is.me) %>%
    group_by(song.label, song.title) %>%
    summarise(times.sung = n_distinct(worship.date),
              last.date = max(worship.date),
              .groups = "drop") %>%
    left_join(wh.df %>%
                group_by(song.label) %>%
                summarise(total.times.sung = n_distinct(paste(congregation.label,
                                                              worship.date)),
                          total.congregations = n_distinct(congregation.label),
                          .groups = "drop"),
              by = "song.label") %>%
    mutate(last.date.display = strftime(last.date, "%b %-d, %Y")) %>%
    dplyr::select(song.title, Song = song.label,
                  `Times sung here` = times.sung, last.date,
                  `Most recently sung` = last.date.display,
                  `Total times sung` = total.times.sung,
                  `Total congregations` = total.congregations) %>%
    arrange(desc(`Times sung here`)) %>%
    datatable(options = list(searching = F, paging = F, info = F,
                             buttons = c("csv"), fixedHeader = T,
                             dom = "Bfrtip",
                             columnDefs = list(list(targets = 4, orderData = 3),
                                               list(targets = c(0, 3),
                                                    visible = F))),
              rownames = F, escape = F,
              extensions = c("Buttons", "FixedHeader"))
}

# Create the graph of number of songs sung per Sunday for this congregation
songs.per.sunday.me = function(wh.df) {
  if(any(wh.df$is.me)) {
    df = wh.df %>%
      filter(is.me) %>%
      group_by(worship.date) %>%
      summarise(n.songs = n_distinct(song.label),
                .groups = "drop") %>%
      ungroup() %>%
      group_by(n.songs) %>%
      summarise(n.dates = n(),
                .groups = "drop") %>%
      mutate(prop.dates = n.dates / sum(n.dates)) %>%
      mutate(hover.text = paste(format(n.songs, big.mark = ",", trim = T),
                                " song", if_else(n.songs == 1, "", "s"), "<br>",
                                format(n.dates, big.mark = ",", trim = T),
                                " Sunday", if_else(n.dates == 1, "", "s"), " (",
                                round(prop.dates * 100), "% of total)",
                                sep = "")) %>%
      arrange(as.numeric(as.character(n.songs)))
    df %>%
      plot_ly(x = ~n.songs, y = ~n.dates, text = ~hover.text,
              marker = list(color = color.me),
              hovertemplate = paste("%{text}", "<extra></extra>", sep = "<br>"),
              texttemplate = "", type = "bar") %>%
      layout(barmode = "stack",
             xaxis = list(title = "Number of songs sung on one Sunday",
                          tickvals = seq(min(df$n.songs), max(df$n.songs)),
                          zeroline = FALSE, showgrid = FALSE),
             yaxis = list(title = "Number of Sundays"),
             hoverlabel = list(bgcolor = "white"))
  }
}

# Create the graph of number of songs sung per Sunday for all congregations
songs.per.sunday.all = function(wh.df) {
  color.pal.me = colour_ramp(c("#FFFFFF", color.me))
  color.pal.other = colour_ramp(c("#FFFFFF", color.other))
  df = wh.df %>%
    group_by(congregation.label, is.me, worship.date) %>%
    summarise(n.songs = n_distinct(song.label),
              .groups = "drop") %>%
    group_by(congregation.label) %>%
    mutate(mode.songs = fmode(n.songs),
           mean.songs = mean(n.songs)) %>%
    ungroup() %>%
    group_by(congregation.label, is.me, mode.songs, mean.songs, n.songs) %>%
    summarise(n.dates = n(),
              .groups = "drop") %>%
    mutate(n.songs = as.character(n.songs),
           n.songs = fct_expand(n.songs,
                                as.character(1:max(as.numeric(n.songs))))) %>%
    complete(nesting(congregation.label, is.me, mode.songs, mean.songs),
             n.songs, fill = list(n.dates = 0)) %>%
    group_by(congregation.label, is.me) %>%
    mutate(prop.dates = n.dates / sum(n.dates),
           n.songs.color = if_else(is.me,
                                   color.pal.me(n.dates / max(n.dates)),
                                   color.pal.other(n.dates / max(n.dates)))) %>%
    ungroup() %>%
    mutate(hover.text = paste(format(n.songs, big.mark = ",", trim = T),
                              " song", if_else(n.songs == 1, "", "s"), "<br>",
                              format(n.dates, big.mark = ",", trim = T),
                              " Sunday", if_else(n.dates == 1, "", "s"), " (",
                              round(prop.dates * 100), "% of total)",
                              sep = "")) %>%
    arrange(mode.songs, mean.songs, congregation.label,
            as.numeric(as.character(n.songs))) %>%
    mutate(congregation.label = fct_inorder(congregation.label))
  df %>%
    plot_ly(x = 1, y = ~congregation.label, text = ~hover.text,
            marker = list(color = ~n.songs.color),
            hovertemplate = paste("<b>%{y}</b>", "%{text}",
                                  "<extra></extra>", sep = "<br>"),
            texttemplate = "", type = "bar", orientation = "h",
            height = 20 + (20 * length(unique(df$congregation.label)))) %>%
    layout(barmode = "stack",
           xaxis = list(title = "Number of songs sung on one Sunday",
                        tickvals = seq(from = 0.5,
                                       length.out = max(df$n.dates), by = 1),
                        ticktext = seq(1, max(df$n.dates), 1),
                        zeroline = FALSE, showgrid = FALSE),
           yaxis = list(title = "", zeroline = FALSE, showgrid = FALSE),
           hoverlabel = list(bgcolor = "white"))
}

# Create the graph of most frequently sung songs for this congregation
top.songs.me = function(wh.df, top.n, metric.name) {
  if(any(wh.df$is.me)) {
    df = wh.df %>%
      filter(is.me, !is.na(song.id)) %>%
      mutate(total.dates = n_distinct(worship.date),
             total.singings = n_distinct(paste(worship.date, song.label))) %>%
      group_by(total.dates, total.singings, song.title, song.label) %>%
      summarise(song.dates = n_distinct(worship.date),
                .groups = "drop") %>%
      mutate(mn = metric.name,
             metric = case_match(mn,
                                 "Number of times sung per year" ~
                                   (song.dates / total.dates) * 52,
                                 "Percent of songs sung" ~
                                   (song.dates / total.singings) * 100)) %>%
      arrange(desc(song.dates)) %>%
      slice_head(n = top.n) %>%
      mutate(song.label = fct_reorder(song.label, song.dates),
             hover.text = paste(song.title, "<br>",
                                format(round(metric, 2), nsmall = 2),
                                case_match(metric.name,
                                           "Number of times sung per year" ~
                                             " times per year",
                                           "Percent of songs sung" ~
                                             "% of songs sung"),
                                sep = ""))
    plot.height = 20 + (top.n * 50)
    df %>%
      plot_ly(x = ~metric, y = ~song.label, type = "bar",
              marker = list(color = color.me),
              text = ~hover.text, texttemplate = "",
              hovertemplate = paste("%{text}", "<extra></extra>",
                                    sep = "<br>"),
              orientation = "h", height = plot.height) %>%
      layout(xaxis = list(title = metric.name,
                          range = list(0, max(df$metric)),
                          zeroline = FALSE),
             yaxis = list(title = "", zeroline = FALSE),
             hoverlabel = list(bgcolor = "white"))
  }
}

# Create the graph of most frequently sung songs by congregation for all
# congregations
top.songs.all = function(wh.df, top.n, metric.name) {
  df = wh.df %>%
    filter(!is.na(song.id)) %>%
    group_by(congregation.label) %>%
    mutate(total.dates = n_distinct(worship.date),
           total.singings = n_distinct(paste(worship.date, song.title))) %>%
    ungroup() %>%
    group_by(congregation.label, is.me, total.dates, total.singings,
             song.title) %>%
    summarise(song.dates = n_distinct(worship.date),
              .groups = "drop") %>%
    mutate(mn = metric.name,
           metric = case_match(mn,
                               "Number of times sung per year" ~
                                 (song.dates / total.dates) * 52,
                               "Percent of songs sung" ~
                                 (song.dates / total.singings) * 100)) %>%
    group_by(congregation.label) %>%
    arrange(desc(song.dates)) %>%
    slice_head(n = top.n) %>%
    ungroup() %>%
    mutate(congregation.color = if_else(is.me, color.me, color.other))
  all.congregations = df %>%
    group_by(congregation.label) %>%
    summarise(mean.metric = mean(metric), .groups = "drop") %>%
    arrange(desc(mean.metric)) %>%
    pull(congregation.label)
  n.rows = ceiling(length(all.congregations) / 3)
  plot.height = 20 + (n.rows * (100 + (top.n * 40)))
  all.subplots = lapply(
    all.congregations,
    function(cl) {
      temp.df = df %>%
        filter(congregation.label == cl) %>%
        mutate(song.title.wrap = str_wrap(song.title, 20),
               song.title.wrap = fct_reorder(song.title.wrap, song.dates),
               hover.text = paste("<b>", cl, "</b><br>", song.title, "<br>",
                                  format(round(metric, 2), nsmall = 2),
                                  case_match(metric.name,
                                             "Number of times sung per year" ~
                                               " times per year",
                                             "Percent of songs sung" ~
                                               "% of songs sung"),
                                  sep = ""))
      temp.df %>%
        plot_ly(x = ~metric, y = ~song.title.wrap, type = "bar",
                marker = list(color = temp.df$congregation.color[1]),
                text = ~hover.text, texttemplate = "",
                hovertemplate = paste("%{text}", "<extra></extra>",
                                      sep = "<br>"),
                orientation = "h", height = plot.height) %>%
        layout(xaxis = list(title = metric.name,
                            range = list(0, max(df$metric)),
                            zeroline = FALSE),
               annotations = list(x = max(df$metric) / 2, y = top.n,
                                  text = cl, showarrow = F,
                                  font = list(size = 20, family = "bold")),
               hoverlabel = list(bgcolor = "white"))
    }
  )
  all.subplots[["nrows"]] = n.rows
  all.subplots[["margin"]] = c(0.1, 0.03, 0.01, 0.02)
  all.subplots[["titleX"]] = TRUE
  do.call("subplot", all.subplots) %>%
    layout(showlegend = FALSE)
}

# Create the graph of most frequently sung songs overall
top.songs.overall = function(wh.df, top.n) {
  color.me = paste("rgba(",
                   paste(as.vector(col2rgb(color.me)), collapse = ", "),
                   ", 0.5)", sep = "")
  color.other = paste("rgba(",
                      paste(as.vector(col2rgb(color.other)), collapse = ", "),
                      ", 0.5)", sep = "")
  df = wh.df %>%
    filter(!is.na(song.id)) %>%
    group_by(congregation.label) %>%
    mutate(total.dates = n_distinct(worship.date)) %>%
    ungroup() %>%
    group_by(congregation.label, is.me, total.dates, song.title, song.label) %>%
    summarise(song.dates = n_distinct(worship.date),
              .groups = "drop") %>%
    mutate(times.per.year = (song.dates / total.dates) * 52)
  df = df %>%
    inner_join(df %>%
                 group_by(song.label) %>%
                 summarise(total.singings = sum(song.dates),
                           .groups = "drop") %>%
                 arrange(desc(total.singings)) %>%
                 slice_head(n = top.n),
               by = "song.label") %>%
    dplyr::select(congregation.label, is.me, song.title, song.label,
                  times.per.year, song.dates) %>%
    complete(nesting(congregation.label, is.me),
             nesting(song.title, song.label),
             fill = list(song.dates = 0, times.per.year = 0)) %>%
    mutate(congregation.color = if_else(is.me, color.me, color.other),
           congregation.size = if_else(is.me, 20, 10),
           song.label = fct_reorder(song.label, song.dates, sum),
           song.jitter = as.numeric(song.label) + rnorm(n(), sd = 0.1),
           hover.text = paste("<b>", congregation.label, "</b><br>", song.title,
                              "<br>",
                              format(round(times.per.year, 2), nsmall = 2),
                              " times per year<br>",
                              format(song.dates, big.mark = ",", trim = T),
                              " time", if_else(song.dates == 1, "", "s"),
                              " total", sep = ""))
  plot.height = 20 + (top.n * 50)
  df %>%
    plot_ly(x = ~times.per.year, y = ~song.jitter, type = "scatter",
            text = ~hover.text,
            marker = list(color = ~congregation.color,
                          line = list(color = ~congregation.color),
                          size = ~congregation.size),
            hovertemplate = paste("%{text}", "<extra></extra>", sep = "<br>"),
            texttemplate = "", height = plot.height) %>%
    layout(xaxis = list(title = "Number of times sung per year"),
           yaxis = list(title = "", zeroline = FALSE,
                        tickvals = seq(1, max(as.numeric(df$song.label))),
                        ticktext = levels(df$song.label)))
}

# Summarize hapaxes for this congregation
create.hapax.me.table = function(wh.df) {
  if(any(wh.df$is.me)) {
    df = wh.df %>%
      filter(is.me, !is.na(song.id)) %>%
      mutate(total.dates = n_distinct(worship.date),
             total.songs = n_distinct(song.id),
             total.singings = n_distinct(paste(song.id, worship.date))) %>%
      ungroup() %>%
      group_by(total.dates, total.songs, total.singings, song.id) %>%
      filter(n_distinct(worship.date) == 1) %>%
      ungroup() %>%
      group_by(total.dates, total.songs, total.singings) %>%
      summarise(n.hapaxes = n_distinct(song.id),
                .groups = "drop") %>%
      mutate(times.per.year = format(round((n.hapaxes / total.dates) * 52, 2),
                                     trim = T),
             pct.history = format(round((n.hapaxes / total.singings) * 100, 2),
                                  trim = T),
             n.hapaxes = format(n.hapaxes, big.mark = ",", trim = T)) %>%
      dplyr::select(`Number of songs sung once` = n.hapaxes,
                    `Number of times sung per year` = times.per.year,
                    `Percent of worship history` = pct.history) %>%
      pivot_longer(cols = everything())
  }
}

# Create the table of hapaxes for this congregation
create.hapax.me.list = function(wh.df) {
  if(any(wh.df$is.me)) {
    wh.df %>%
      filter(is.me, !is.na(song.id)) %>%
      group_by(song.label) %>%
      filter(n_distinct(worship.date) == 1) %>%
      ungroup() %>%
      dplyr::select(song.title, song.label, worship.date) %>%
      distinct() %>%
      mutate(worship.date.display = strftime(worship.date, "%b %-d, %Y")) %>%
      dplyr::select(song.title, Song = song.label, worship.date,
                    `Date sung` = worship.date.display) %>%
      arrange(song.title) %>%
      datatable(options = list(searching = F, paging = F, info = F,
                               buttons = c("csv"), fixedHeader = T,
                               dom = "Bfrtip",
                               columnDefs = list(list(targets = 3,
                                                      orderData = 2),
                                                 list(targets = c(0, 2),
                                                      visible = F))),
                rownames = F, escape = F,
                extensions = c("Buttons", "FixedHeader"))
  }
}

# Determine the height of the hapax graph
hapax.all.height = function(wh.df) {
  h = (length(unique(wh.df$congregation.label)) * 20) + 20
}

# Create the graph of songs sung once for all congregations
hapaxes.all = function(wh.df, metric.name) {
  df = wh.df %>%
    filter(!is.na(song.id)) %>%
    group_by(congregation.label) %>%
    mutate(total.dates = n_distinct(worship.date),
           total.songs = n_distinct(song.id),
           total.singings = n_distinct(paste(song.id, worship.date))) %>%
    ungroup() %>%
    group_by(congregation.label, is.me, total.dates, total.songs,
             total.singings, song.id) %>%
    filter(n_distinct(worship.date) == 1) %>%
    ungroup() %>%
    group_by(congregation.label, is.me, total.dates, total.songs,
             total.singings) %>%
    summarise(n.hapaxes = n_distinct(song.id),
              hapax.singings = n_distinct(paste(song.id, worship.date)),
              .groups = "drop") %>%
    mutate(mn = metric.name,
           metric = case_match(mn,
                               "Number of times sung per year" ~
                                 (hapax.singings / total.dates) * 52,
                               "Percent of worship history" ~
                                 (hapax.singings / total.singings) * 100),
           congregation.color = if_else(is.me, color.me, color.other),
           congregation.label = fct_reorder(congregation.label, metric),
           hover.text = paste("<b>", congregation.label, "</b><br>",
                              format(total.dates, big.mark = ",", trim = T),
                              " date", if_else(total.dates == 1, "", "s"),
                              "<br>",
                              format(n.hapaxes, big.mark = ",", trim = T),
                              " song", if_else(n.hapaxes == 1, "", "s"),
                              " sung exactly once<br>",
                              format(round(metric, 2), nsmall = 2, trim = T),
                              case_match(mn,
                                         "Number of times sung per year" ~
                                           " times sung per year",
                                         "Percent of worship history" ~
                                           "% of worship history"),
                              sep = ""))
  x.title = ""
  if(metric.name == "Number of times sung per year") {
    x.title = "Number of times per year a one-time song was sung"
  } else if(metric.name == "Percent of worship history") {
    x.title = "Percent of worship history consisting of one-time songs"
  }
  df %>%
    plot_ly(x = ~metric, y = ~congregation.label, text = ~hover.text,
            marker = list(color = ~congregation.color),
            type = "bar", orientation = "h",
            hovertemplate = paste("%{text}", "<extra></extra>", sep = "<br>"),
            texttemplate = "", source = "hapax.all.plot",
            height = hapax.all.height(wh.df)) %>%
    layout(xaxis = list(title = x.title),
           yaxis = list(title = ""),
           hoverlabel = list(bgcolor = "white")) %>%
    event_register("plotly_click")
}

# Create the table of hapaxes for clicked congregation
create.hapax.all.list = function(wh.df, hapax.c) {
  wh.df %>%
    filter(congregation.label == hapax.c, !is.na(song.id)) %>%
    group_by(congregation.name, song.label) %>%
    filter(n_distinct(worship.date) == 1) %>%
    ungroup() %>%
    dplyr::select(congregation.name, song.title, song.label, worship.date) %>%
    distinct() %>%
    mutate(worship.date.display = strftime(worship.date, "%b %-d, %Y")) %>%
    dplyr::select(Congregation = congregation.name, song.title,
                  Song = song.label, worship.date,
                  `Date sung` = worship.date.display) %>%
    arrange(song.title) %>%
    datatable(options = list(searching = F, paging = F, info = F,
                             buttons = c("csv"), fixedHeader = T,
                             dom = "Bfrtip",
                             columnDefs = list(list(targets = 4, orderData = 3),
                                               list(targets = c(1, 3),
                                                    visible = F))),
              rownames = F, escape = F,
              extensions = c("Buttons", "FixedHeader"))
}

# Create plot of song year for this congregation
song.year.me = function(wh.df, song.info.df) {
  if(any(wh.df$is.me)) {
    df = wh.df %>%
      filter(is.me) %>%
      dplyr::select(song.id, worship.date) %>%
      distinct() %>%
      inner_join(song.info.df, by = c("song.id" = "SongID")) %>%
      filter(!is.na(Year)) %>%
      mutate(decade = floor(Year / 10) * 10) %>%
      group_by(decade) %>%
      summarise(n.singings = n(), .groups = "drop") %>%
      mutate(pct.singings = (n.singings / sum(n.singings)) * 100,
             hover.text = paste("<br>", decade, "s<br>",
                                format(round(pct.singings, 2), nsmall = 2),
                                "% of worship history", sep = ""))
    df %>%
      plot_ly(x = ~decade, y = ~pct.singings, type = "bar",
              marker = list(color = color.me),
              text = ~hover.text, texttemplate = "", source = "song.year.me",
              hovertemplate = paste("%{text}", "<extra></extra>",
                                    sep = "<br>")) %>%
      layout(xaxis = list(title = "Decade when song was written",
                          range = list(1695, max(df$decade) + 5),
                          zeroline = FALSE),
             yaxis = list(title = "% of worship history",
                          range = list(0, max(df$pct.singings))),
             hoverlabel = list(bgcolor = "white")) %>%
      event_register("plotly_click")
  }
}

# Create the table of songs by year for this congregation
create.year.me.list = function(wh.df, song.info.df, current.year) {
  if(any(wh.df$is.me)) {
    wh.df %>%
      filter(is.me, !is.na(song.id)) %>%
      inner_join(song.info.df %>%
                   filter(floor(Year / 10) * 10 == current.year),
                 by = c("song.id" = "SongID")) %>%
      group_by(song.title, song.label, Year) %>%
      summarise(times.sung = n_distinct(worship.date),
                last.date = max(worship.date),
                .groups = "drop") %>%
      mutate(last.date.display = strftime(last.date, "%b %-d, %Y")) %>%
      dplyr::select(song.title, Song = song.label, Year,
                    `Times sung` = times.sung, last.date,
                    `Most recently sung` = last.date.display) %>%
      arrange(desc(`Times sung`), desc(last.date), song.title) %>%
      datatable(options = list(searching = F, paging = F, info = F,
                               buttons = c("csv"), fixedHeader = T,
                               dom = "Bfrtip",
                               columnDefs = list(list(targets = 5,
                                                      orderData = 4),
                                                 list(targets = c(0, 4),
                                                      visible = F))),
                rownames = F, escape = F,
                extensions = c("Buttons", "FixedHeader"))
  }
}

# Determine the height of the song year graph
song.year.all.height = function(wh.df) {
  all.congregations = length(unique(wh.df$congregation.label))
  r = ceiling(all.congregations / 3)
  h = 20 + (r * 240)
  list(n.rows = r, plot.height = h)
}

# Create plot of song year for all congregations
song.year.all = function(wh.df, song.info.df) {
  df = wh.df %>%
    dplyr::select(congregation.label, is.me, song.id, worship.date) %>%
    distinct() %>%
    inner_join(song.info.df, by = c("song.id" = "SongID")) %>%
    filter(!is.na(Year)) %>%
    group_by(congregation.label) %>%
    mutate(mean.year = mean(Year)) %>%
    ungroup() %>%
    mutate(decade = floor(Year / 10) * 10) %>%
    group_by(congregation.label, is.me, mean.year, decade) %>%
    summarise(n.singings = n(), .groups = "drop") %>%
    group_by(congregation.label) %>%
    mutate(pct.singings = (n.singings / sum(n.singings)) * 100) %>%
    mutate(congregation.color = if_else(is.me, color.me, color.other))
  all.congregations = df %>%
    arrange(mean.year) %>%
    pull(congregation.label) %>%
    unique()
  rows.height = song.year.all.height(wh.df)
  all.subplots = lapply(
    all.congregations,
    function(cl) {
      temp.df = df %>%
        filter(congregation.label == cl) %>%
        mutate(hover.text = paste("<b>", cl, "</b><br>", decade, "s<br>",
                                  format(round(pct.singings, 2), nsmall = 2),
                                  "% of worship history", sep = ""))
      temp.df %>%
        plot_ly(x = ~decade, y = ~pct.singings, type = "bar",
                marker = list(color = temp.df$congregation.color[1]),
                text = ~hover.text, texttemplate = "", source = "song.year.all",
                customdata = cl,
                hovertemplate = paste("%{text}", "<extra></extra>",
                                      sep = "<br>"),
                height = rows.height$plot.height) %>%
        layout(xaxis = list(title = "Decade when song was written",
                            range = list(1695, max(df$decade) + 5),
                            zeroline = FALSE),
               yaxis = list(title = "% of worship history",
                            range = list(0, max(df$pct.singings))),
               annotations = list(x = 1700 + ((max(df$decade) - 1700) / 2),
                                  y = max(df$pct.singings),
                                  text = cl, showarrow = F,
                                  font = list(size = 20, family = "bold")),
               hoverlabel = list(bgcolor = "white")) %>%
        event_register("plotly_click")
    }
  )
  all.subplots[["nrows"]] = rows.height$n.rows
  all.subplots[["margin"]] = c(0.03, 0.03, 0.02, 0.02)
  all.subplots[["titleX"]] = TRUE
  all.subplots[["titleY"]] = TRUE
  do.call("subplot", all.subplots) %>%
    layout(showlegend = FALSE)
}

# Create the table of songs by year for all congregations
create.year.all.list = function(wh.df, song.info.df, current.year,
                                congregation) {
  wh.df %>%
    filter(congregation.label == congregation, !is.na(song.id)) %>%
    inner_join(song.info.df %>%
                 filter(floor(Year / 10) * 10 == current.year),
               by = c("song.id" = "SongID")) %>%
    group_by(congregation.name, song.title, song.label, Year) %>%
    summarise(times.sung = n_distinct(worship.date),
              last.date = max(worship.date),
              .groups = "drop") %>%
    mutate(last.date.display = strftime(last.date, "%b %-d, %Y")) %>%
    dplyr::select(Congregation = congregation.name, song.title,
                  Song = song.label, Year, `Times sung` = times.sung,
                  last.date, `Most recently sung` = last.date.display) %>%
    arrange(desc(`Times sung`), desc(last.date), song.title) %>%
    datatable(options = list(searching = F, paging = F, info = F,
                             buttons = c("csv"), fixedHeader = T,
                             dom = "Bfrtip",
                             columnDefs = list(list(targets = 6, orderData = 5),
                                               list(targets = c(1, 5),
                                                    visible = F))),
              rownames = F, escape = F,
              extensions = c("Buttons", "FixedHeader"))
}

# Determine the height of the graph of topics for this congregation
topics.me.height = function(wh.df, song.info.df) {
  topic.songs.df = song.info.df %>%
    pivot_longer(cols = -c("SongID", "SongName", "Year"), names_to = "topic",
                 values_to = "about") %>%
    filter(about)
  num.topics = wh.df %>%
    filter(is.me) %>%
    inner_join(topic.songs.df, by = c("song.id" = "SongID"),
               relationship = "many-to-many") %>%
    pull(topic) %>%
    unique() %>%
    length()
  h = (num.topics * 15) + 80
  return(h)
}

# Create the graph of topics for this congregation
topics.me = function(wh.df, song.info.df, metric.name) {
  if(any(wh.df$is.me)) {
    topic.songs.df = song.info.df %>%
      pivot_longer(cols = -c("SongID", "SongName", "Year"), names_to = "topic",
                   values_to = "about") %>%
      filter(about)
    df = wh.df %>%
      filter(is.me) %>%
      mutate(total.dates = n_distinct(worship.date),
             total.songs = n_distinct(song.id),
             total.singings = n_distinct(paste(song.id, worship.date))) %>%
      inner_join(topic.songs.df, by = c("song.id" = "SongID"),
                 relationship = "many-to-many") %>%
      group_by(total.dates, total.songs, total.singings, topic) %>%
      summarise(n.songs = n_distinct(song.id),
                n.singings = n_distinct(paste(song.id, worship.date)),
                .groups = "drop") %>%
      mutate(mn = metric.name,
             metric = case_match(mn,
                                 "Number of songs" ~ n.songs,
                                 "Number of times sung per year" ~
                                   (n.singings / total.dates) * 52,
                                 "Percent of worship history" ~
                                   (n.singings / total.singings) * 100),
	     topic = fct_reorder(topic, metric),
             hover.text = paste("<b>", topic, "</b><br>",
                                format(round(metric,
                                             if_else(mn == "Number of songs",
                                                     0, 2)),
                                       nsmall = if_else(mn == "Number of songs",
                                                        0, 2), trim = T),
                                case_match(mn,
                                           "Number of songs" ~
                                             paste(" song",
                                                   if_else(metric == 1, "", "s"),
                                                   sep = ""),
                                           "Number of times sung per year" ~
                                             " times sung per year",
                                           "Percent of worship history" ~
                                             "% of worship history"),
                                sep = ""))
    x.title = ""
    if(metric.name == "Number of songs") {
      x.title = "Number of songs on this topic"
    } else if(metric.name == "Number of times sung per year") {
      x.title = "Number of times per year a song on this topic was sung"
    } else if(metric.name == "Percent of worship history") {
      x.title = "Percent of worship history"
    }
    df %>%
      plot_ly(x = ~metric, y = ~topic, text = ~hover.text,
              marker = list(color = color.me),
              type = "bar", orientation = "h",
              hovertemplate = paste("%{text}", "<extra></extra>", sep = "<br>"),
              texttemplate = "", source = "topics.me.plot",
              height = topics.me.height(wh.df, song.info.df)) %>%
      layout(xaxis = list(title = x.title),
             yaxis = list(title = ""),
             hoverlabel = list(bgcolor = "white")) %>%
      event_register("plotly_click")
  }
}

# Create the responsive table of songs by topic for this congregation
create.topics.me.table = function(wh.df, song.info.df, topic.name) {
  if(any(wh.df$is.me)) {
    topic.songs.df = song.info.df[song.info.df[[topic.name]],]
    wh.df %>%
      filter(is.me, !is.na(song.id)) %>%
      mutate(total.dates = n_distinct(worship.date),
             total.singings = n_distinct(paste(song.label, worship.date))) %>%
      semi_join(topic.songs.df, by = c("song.id" = "SongID")) %>%
      group_by(song.title, song.label, total.dates, total.singings) %>%
      summarise(n.singings = n_distinct(if_else(is.me, worship.date, NA),
                                         na.rm = T),
                .groups = "drop") %>%
      mutate(times.per.year = round((n.singings / total.dates) * 52, 2),
             pct.history = round((n.singings / total.singings) * 100, 2)) %>%
      dplyr::select(song.title, Song = song.label, `Times sung` = n.singings,
                    `Times sung per year` = times.per.year,
                    `Percent of worship history` = pct.history) %>%
      arrange(desc(`Times sung`), song.title) %>%
      datatable(options = list(searching = F, paging = F, info = F,
                               buttons = c("csv"), fixedHeader = T,
                               dom = "Bfrtip",
                               columnDefs = list(list(targets = c(0),
                                                      visible = F))),
                rownames = F, escape = F,
                extensions = c("Buttons", "FixedHeader"))
  }
}

# Determine the height of the graph of topics for all congregations
topics.all.height = function(wh.df, song.info.df, topic.name) {
  topic.songs.df = song.info.df[song.info.df[[topic.name]],]
  num.congregations = wh.df %>%
    semi_join(topic.songs.df, by = c("song.id" = "SongID")) %>%
    pull(congregation.label) %>%
    unique() %>%
    length()
  h = (num.congregations * 20) + 80
  return(h)
}

# Create the graph of topics
topics.all = function(wh.df, song.info.df, topic.name, metric.name) {
  topic.songs.df = song.info.df[song.info.df[[topic.name]],]
  df = wh.df %>%
    group_by(congregation.label) %>%
    mutate(total.dates = n_distinct(worship.date, na.rm = T),
           total.songs = n_distinct(song.id, na.rm = T),
           total.singings = n_distinct(paste(song.id, worship.date),
                                       na.rm = T)) %>%
    ungroup() %>%
    semi_join(topic.songs.df, by = c("song.id" = "SongID")) %>%
    group_by(congregation.label, is.me, total.dates, total.songs,
             total.singings) %>%
    summarise(n.songs = n_distinct(song.id, na.rm = T),
              n.singings = n_distinct(paste(song.id, worship.date), na.rm = T),
              .groups = "drop") %>%
    mutate(mn = metric.name,
           metric = case_match(mn,
                               "Number of songs" ~ n.songs,
                               "Number of times sung per year" ~
                                 (n.singings / total.dates) * 52,
                               "Percent of worship history" ~
                                 (n.singings / total.singings) * 100),
           congregation.color = if_else(is.me, color.me, color.other),
           congregation.label = fct_reorder(congregation.label, metric),
           hover.text = paste("<b>", congregation.label, "</b><br>",
                              format(total.dates, big.mark = ",", trim = T),
                              " date", if_else(total.dates == 1, "", "s"),
                              "<br>",
                              format(round(metric,
                                           if_else(mn == "Number of songs", 0,
                                                   2)),
                                     nsmall = if_else(mn == "Number of songs",
                                                      0, 2), trim = T),
                              case_match(mn,
                                         "Number of songs" ~
                                           paste(" song",
                                                 if_else(metric == 1, "", "s"),
                                                 sep = ""),
                                         "Number of times sung per year" ~
                                           " times sung per year",
                                         "Percent of worship history" ~
                                           "% of worship history"),
                              sep = ""))
  x.title = ""
  if(metric.name == "Number of songs") {
    x.title = "Number of songs on this topic"
  } else if(metric.name == "Number of times sung per year") {
    x.title = "Number of times per year a song on this topic was sung"
  } else if(metric.name == "Percent of worship history") {
    x.title = "Percent of worship history"
  }
  df %>%
    plot_ly(x = ~metric, y = ~congregation.label, text = ~hover.text,
            marker = list(color = ~congregation.color),
            type = "bar", orientation = "h",
            hovertemplate = paste("%{text}", "<extra></extra>", sep = "<br>"),
            texttemplate = "", source = "topics.all.plot",
            height = topics.all.height(wh.df, song.info.df, topic.name)) %>%
    layout(xaxis = list(title = x.title),
           yaxis = list(title = ""),
           hoverlabel = list(bgcolor = "white"))
}

# Create the table of songs by topic
create.topics.all.table = function(wh.df, song.info.df, topic.name) {
  topic.songs.df = song.info.df[song.info.df[[topic.name]],]
  wh.df %>%
    filter(!is.na(song.id)) %>%
    semi_join(topic.songs.df, by = c("song.id" = "SongID")) %>%
    group_by(song.title, song.label) %>%
    summarise(my.singings = n_distinct(if_else(is.me, worship.date, NA),
                                       na.rm = T),
              all.singings = n_distinct(paste(congregation.label,
                                              worship.date)),
              .groups = "drop") %>%
    dplyr::select(song.title, Song = song.label,
                  `Times sung here` = my.singings,
                  `Total times sung` = all.singings) %>%
    arrange(desc(`Times sung here`), desc(`Total times sung`), song.title) %>%
    datatable(options = list(searching = F, paging = F, info = F,
                             buttons = c("csv"), fixedHeader = T,
                             dom = "Bfrtip",
                             columnDefs = list(list(targets = c(0), 
                                                    visible = F))),
              rownames = F, escape = F,
              extensions = c("Buttons", "FixedHeader"))
}
