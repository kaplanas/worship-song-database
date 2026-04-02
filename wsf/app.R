library(shiny)
library(shinyjs)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(paws)
library(aws.signature)
library(bslib)
library(future)
library(promises)
future::plan(multisession)

#### Useful initial settings ####

use_credentials("wsf-shiny")
wsf.db = dynamodb(endpoint = "dynamodb.us-west-2.api.aws")

# Load stuff from the database
source("R_files/load_from_database.R", local = T)

# Create content panels
source("R_files/create_panels.R", local = T)

#### UI ####

# Panels we might or might not include in the UI
page.title = "Worship Song Finder"
search.page = tabPanel("Search for songs",
                       useShinyjs(),
                       sidebarLayout(
                         # The filters
                         sidebarPanel(
                           navlistPanel(
                             header = list(tags$b("Large result lists may take a while to render"),
                                           tags$br(), tags$br()),
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
                             lyrics.filter,
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

page.header = tags$head(tags$link(rel = "stylesheet", type = "text/css",
                                  href = "theme.css"),
                        tags$link(rel = "shortcut icon", href = "favicon.ico"),
                        tags$meta(property = "og:type", content = "website"),
                        tags$meta(property = "og:url",
                                  content = "https://wsf.worshipsongfinder.com/"),
                        tags$meta(property = "og:title",
                                  content = "Worship Song Finder"),
                        tags$meta(property = "og:image",
                                  content = "https://wsf.worshipsongfinder.com/wsf_screenshot.png"),
                        tags$meta(property = "og:description",
                                  content = "Search tool for worship songs in the hymnody of the Churches of Christ"))

# Define UI
ui <- navbarPage(
  title = page.title,
  theme = bs_theme(bootswatch = "lumen"),
  header = page.header,
  search.page,
  psalm.singing.page,
  help.page
)

#### Server ####

# Define server logic
server <- function(input, output, session) {

  # List of scripture references for the selected book
  scripture.references.df = reactiveVal(NULL)
  observe({
    if(input$scriptureBook != 0 & !is.null(input$scriptureBook)) {
      scripture.references.df(
        dynamodb.items.to.df(wsf.db, "wsf_scripturereferences", "BookID",
                             c(as.numeric(input$scriptureBook))) %>%
          dplyr::select(scripture.reference.id = ScriptureReferenceID,
                        book.id = BookID, chapter = Chapter, verse = Verse)
      )
    }
  })

  # Populate the scripture filter based on the selected options
  includeScriptureEnd = reactive({
    ifelse(input$scriptureOptions == "Range", T, F)
  })
  observe({
    if(includeScriptureEnd()) {
      updateSelectInput(session, "scriptureVerseStart",
                        label = "Starting verse:")
      shinyjs::show(id = "scriptureVerseEnd")
    }
    else {
      shinyjs::hide(id = "scriptureVerseEnd")
      updateSelectInput(session, "scriptureVerseStart", label = "Verse:")
    }
  })
  dynamicChapters = reactive({
    sort(unique(scripture.references.df()$chapter))
  })
  observe({
    updateSelectInput(session, "scriptureChapter", choices = dynamicChapters())
  })
  dynamicVersesStart = reactive({
    sort(unique(scripture.references.df()$verse[scripture.references.df()$chapter == input$scriptureChapter]))
  })
  observe({
    updateSelectInput(session, "scriptureVerseStart",
                      choices = dynamicVersesStart())
  })
  dynamicVersesEnd = reactive({
    sort(unique(scripture.references.df()$verse[scripture.references.df()$chapter == input$scriptureChapter]))
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

