library(shiny)
library(shinyjs)
library(shinyWidgets)
library(RMySQL)
library(tidyr)
library(date)
library(dplyr)
library(ggplot2)
library(stringr)
library(stringi)
library(gridExtra)
library(magrittr)
library(stringr)
library(DT)
library(RColorBrewer)
library(tibble)
library(xml2)
library(paws)
library(purrr)

source("version.R", local = T)
source("database_connection_local.R", local = T)

#### Useful initial settings ####

# Connect to the database
wsf.shiny.con = dbConnect(MySQL(), user = db.user, password = db.password,
                          dbname = ifelse(version == "ctcc", "wsf_ctcc",
                                          "wsf"),
                          host = db.host, port = 3306)
on.exit(dbDisconnect(wsf.shiny.con), add = T)
dbGetQuery(wsf.shiny.con, "SET NAMES utf8")
Sys.setenv(AWS_PROFILE = "greek_shiny_server_test")
wsf.db = dynamodb()

# Load stuff from the database
source("R_files/load_from_database.R", local = T)

# Settings for date inputs and outputs
date.input.sep = " to "
date.input.format = "MM d, yyyy"
date.output.format = "%B %e, %Y"
date.input.min = "2017-03-26"
date.input.start = seq(Sys.Date(), length = 7, by = "-1 month")[7]

# Create content panels
source("R_files/create_panels.R", local = T)

# Plotting theme
theme_set(theme_bw())

#### UI ####

# Panels we might or might not include in the UI
page.title = "Worship Song Finder"
page.theme = "theme.css"
search.page = tabPanel("Search for songs",
                       useShinyjs(),
                       sidebarLayout(
                         # The filters
                         sidebarPanel(
                           navlistPanel(
                             song.title.filter,
                             artist.name.filter,
                             topic.filter,
                             scripture.reference.filter,
                             language.filter,
                             songbook.filter,
                             arrangement.type.filter,
                             key.signature.filter,
                             time.signature.filter,
                             meter.filter,
                             tune.name.filter,
                             well = F,
                             widths = c(6, 6)
                           ),
                           width = 4
                         ),
                         # The results
                         mainPanel(
                           uiOutput("songList"),
                           width = 8
                         )
                       )
              )
psalm.singing.page = tabPanel("Psalm singing",
                              sidebarLayout(
                                sidebarPanel(
                                  psalm.number.filter,
                                  psalm.song.type.filter,
                                  helpText(tags$p(tags$b("Songs that refer to Psalms"))),
                                  helpText(tags$p("Includes songs from Church of Christ hymnody, plus some metrical paraphrases")),
                                  helpText(tags$p("Copyrighted lyrics are not shown")),
                                  helpText(tags$p("Alternative tunes have the same meter and can be substituted for the original tune (possibly with some creative adjustments)")),
                                  helpText(tags$p("Alternative tunes are listed only if both lyrics and music are in the public domain")),
                                  helpText(tags$p("Songs are grouped into four categories:")),
                                  helpText(tags$ul(
                                    tags$li(tags$span("Paraphrase",
                                                      style = "color: white; background-color: rgba(55, 126, 184, 0.7); padding: 2px;"),
                                            "(or extensive quotation; an existing song)",
                                            style = "margin: 5px;"),
                                    tags$li(tags$span("Metrical paraphrase",
                                                      style = "color: white; background-color: rgba(77, 175, 74, 0.6); padding: 2px;"),
                                            "(not an existing song)",
                                            style = "margin: 5px;"),
                                    tags$li(tags$span("Direct quotation",
                                                      style = "color: white; background-color: rgba(152, 78, 163, 0.7); padding: 2px;"),
                                            "(or close paraphrase, of just a few lines; major focus of song)",
                                            style = "margin: 5px;"),
                                    tags$li(tags$span("Minor reference",
                                                      style = "color: white; background-color: rgba(120, 120, 120, 1); padding: 2px;"),
                                            style = "margin: 5px;")
                                  )),
                                  width = 3
                                ),
                                mainPanel(
                                  uiOutput("psalmSongList"),
                                  width = 9
                                )
                               )
                     )
help.page = tabPanel("Help",
                     navlistPanel(
                       tabPanel("What is this site for?",
                                includeHTML("help/what_for.html")),
                       tabPanel("Songs vs. instances",
                                includeHTML("help/songs_instances.html")),
                       tabPanel("The title, artist, and tune name filters",
                                includeHTML("help/title_artist_tune_name_filters.html")),
                       tabPanel("The scripture reference filter",
                                includeHTML("help/scripture_reference_filter.html")),
                       tabPanel("The language filter",
                                includeHTML("help/language_filter.html")),
                       tabPanel("The topic, arrangement, and songbook filters",
                                includeHTML("help/topic_arrangement_songbook_filters.html")),
                       tabPanel("The key and time signature filters",
                                includeHTML("help/key_time_signature_filters.html")),
                       tabPanel("The meter filter",
                                includeHTML("help/meter_filter.html")),
                       tabPanel("Who should I contact with questions/comments/complaints/etc.?",
                                includeHTML("help/contact.html")),
                       tabPanel("Can I get the raw database?",
                                includeHTML("help/source_code.html"))
                     )
            )

page.header = tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico"),
                        tags$meta(property = "og:type", content = "website"),
                        tags$meta(property = "og:url",
                                  content = "https://worshipsongfinder.com/worship-song-finder/"),
                        tags$meta(property = "og:title",
                                  content = "Worship Song Finder"),
                        tags$meta(property = "og:image",
                                  content = "https://worshipsongfinder.com/worship-song-finder/wsf_screenshot.png"),
                        tags$meta(property = "og:description",
                                  content = "Search tool for worship songs in the hymnody of the Churches of Christ"))

# Define UI
ui <- navbarPage(
  page.title,
  theme = page.theme,
  header = page.header,
  search.page,
  psalm.singing.page,
  help.page
)

#### Server ####

# Define server logic
server <- function(input, output, session) {

  # Populate the scripture filter based on the selected options
  includeScriptureEnd = reactive({
    ifelse(input$scriptureOptions == "Range", T, F)
  })
  observe({
    if(includeScriptureEnd()) {
      updateSelectInput(session, "scriptureChapterStart",
                        label = "Starting chapter:")
      updateSelectInput(session, "scriptureVerseStart",
                        label = "Starting verse:")
      shinyjs::show(id = "scriptureChapterEnd")
      shinyjs::show(id = "scriptureVerseEnd")
    }
    else {
      shinyjs::hide(id = "scriptureChapterEnd")
      shinyjs::hide(id = "scriptureVerseEnd")
      updateSelectInput(session, "scriptureChapterStart",
                        label = "Chapter:")
      updateSelectInput(session, "scriptureVerseStart",
                        label = "Verse:")
    }
  })
  dynamicChapters = reactive({
    sort(unique(scripture.references.df$chapter[scripture.references.df$book.id == input$scriptureBook]))
  })
  observe({
    updateSelectInput(session, "scriptureChapterStart",
                      choices = dynamicChapters())
    updateSelectInput(session, "scriptureChapterEnd",
                      choices = dynamicChapters())
  })
  dynamicVersesStart = reactive({
    sort(unique(scripture.references.df$verse[scripture.references.df$book.id == input$scriptureBook &
                                                scripture.references.df$chapter == input$scriptureChapterStart]))
  })
  observe({
    updateSelectInput(session, "scriptureVerseStart",
                      choices = dynamicVersesStart())
  })
  dynamicVersesEnd = reactive({
    sort(unique(scripture.references.df$verse[scripture.references.df$book.id == input$scriptureBook &
                                              scripture.references.df$chapter == input$scriptureChapterEnd]))
  })
  observe({
    updateSelectInput(session, "scriptureVerseEnd",
                      choices = dynamicVersesEnd())
  })

  # Populate outputs
  source("R_files/populate_outputs.R", local = T)
  
}

#### Run ####

# Run the application 
shinyApp(ui = ui, server = server)

