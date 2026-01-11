#### Get lists for populating filters ####

# A string for indicating that nothing is selected
none.selected.string = "[None selected]"

# Topics
topic.list = topics.df %>%
  arrange(topic.name)

# Books of the Bible
book.list = c(0, sort(bible.books.df$book.id))
names(book.list) = c(none.selected.string,
                     bible.books.df$book.name[book.list[-1]])
book.list = as.list(book.list)

# Songbooks
songbook.list = songbooks.df %>%
  arrange(songbook.name) %>%
  select(songbook.id, songbook.name)

# Arrangement types
arrangement.list = arrangement.types.df %>%
  arrange(arrangement.type)

# Languages
language.list = languages.df %>%
  arrange(language.name)

# Key signatures
key.signature.list = key.signatures.df %>%
  arrange(pitch.name, ifelse(accidental.id == 3, 0, accidental.id), mode.id) %>%
  dplyr::select(key.signature.id, key.signature.string)

# Time signatures
time.signature.list = time.signatures.df %>%
  arrange(time.signature.measure, time.signature.beat) %>%
  dplyr::select(time.signature.id, time.signature.string)

# Meters
meter.list = meters.df %>%
  filter(meter.id != 1) %>%
  arrange(sort.string) %>%
  dplyr::select(meter.id, meter = meter.string)

# Psalm numbers
psalm.number.list = 1:150

# Psalm song types
psalm.song.type.list = psalm.song.types.df %>%
  dplyr::select(psalm.song.type, psalm.song.type.id) %>%
  arrange(psalm.song.type) %>%
  deframe()

#### Create filter panels ####

# Song title filter
song.title.filter = tabPanel("Song title",
                             div(id = "songTitleFilter",
                                 radioButtons("songTitleOptions", NULL,
                                              choices = c("Parts", "Whole words", "String")),
                                 textInput("songTitle", NULL,
                                           placeholder = "Enter at least three characters"))
                             )

# Artist name filter
artist.name.filter = tabPanel("Artist name",
                              div(radioButtons("artistNameOptions", NULL,
                                               choices = c("Lyrics" = "lyricist",
                                                           "Music" = "composer",
                                                           "Arrangement" = "arranger",
                                                           "Any" = "any")),
                                  textInput("artistName", NULL,
                                            placeholder = "Enter at least three characters")))

# Topic filter
topic.filter = tabPanel("Topic",
                        radioButtons("topicOptions", NULL,
                                     choices = c("Match any", "Match all")),
                        checkboxGroupInput("topicChoices", NULL,
                                           choiceNames = topic.list$topic.name,
                                           choiceValues = topic.list$topic.id)
                        )

# Scripture reference filter
scripture.reference.filter = tabPanel("Scripture reference",
                                      radioButtons("scriptureOptions", NULL,
                                                   choices = c("Single verse", "Range")),
                                      selectInput("scriptureBook", "Book:",
                                                  choices = book.list),
                                      selectInput("scriptureChapter", "Chapter:", c()),
                                      selectInput("scriptureVerseStart", "Verse:", c()),
                                      selectInput("scriptureVerseEnd", "Ending verse:", c())
                                      )

# Language filter
language.filter = tabPanel("Language",
                           radioButtons("languageOptions", NULL,
                                        choices = c("Match any", "Match all")),
                           checkboxGroupInput("languageChoices", NULL,
                                              choiceNames = language.list$language.name,
                                              choiceValues = language.list$language.id)
                           )

# Songbook filter
songbook.filter= tabPanel("Songbook",
                          radioButtons("songbookOptions", NULL,
                                       choices = c("Match any", "Match all")),
                          checkboxGroupInput("songbookChoices", NULL,
                                             choiceNames = songbook.list$songbook.name,
                                             choiceValues = songbook.list$songbook.id)
                          )

# Arrangement type filter
arrangement.type.filter = tabPanel("Arrangement type",
                                   radioButtons("arrangementOptions", NULL,
                                                choices = c("Include", "Exclude")),
                                   checkboxGroupInput("arrangementChoices", NULL,
                                                      choiceNames = arrangement.list$arrangement.type,
                                                      choiceValues = arrangement.list$arrangement.type.id)
                                   )

# Key signature filter
key.signature.filter = tabPanel("Key signature",
                                radioButtons("keyOptions", NULL,
                                             choices = c("Match any", "Match all")),
                                checkboxGroupInput("keyChoices", NULL,
                                                   choiceNames = key.signature.list$key.signature.string,
                                                   choiceValues = key.signature.list$key.signature.id)
                                )

# Time signature filter
time.signature.filter = tabPanel("Time signature",
                                 radioButtons("timeOptions", NULL,
                                              choices = c("Match any", "Match all")),
                                 checkboxGroupInput("timeChoices", NULL,
                                                    choiceNames = time.signature.list$time.signature.string,
                                                    choiceValues = time.signature.list$time.signature.id)
                                 )

# Meter filter
meter.filter = tabPanel("Meter",
                        radioButtons("meterOptions", NULL,
                                     choices = c("Match any", "Match all")),
                        checkboxGroupInput("meterChoices", NULL,
                                           choiceNames = meter.list$meter,
                                           choiceValues = meter.list$meter.id)
)

# Tune name filter
tune.name.filter = tabPanel("Tune name",
                            div(id = "tuneNameFilter",
                                radioButtons("tuneNameOptions", NULL,
                                             choices = c("Parts", "String")),
                                textInput("tuneName", NULL,
                                          placeholder = "Enter at least three characters"))
)

# Psalm number filter
psalm.number.filter = selectInput("psalmNumber",
                                  "Psalm number:",
                                  1:150)

# Psalm song type filter
psalm.song.type.filter = pickerInput("psalmSongType",
                                     "Select one or more song types:",
                                     multiple = T,
                                     options = list(`actions-box` = TRUE),
                                     width = "100%",
                                     choices = psalm.song.type.list)
