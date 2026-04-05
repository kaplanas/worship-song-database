library(shiny)
library(bslib)
library(htmltools)
library(openssl)
library(tidyverse)
library(rhandsontable)
library(DT)
library(tidygeocoder)
library(shinyWidgets)
library(readxl)
library(aws.signature)
library(paws)
library(pdftools)
library(plotly)
library(scales)
library(collapse)
library(future)
library(promises)
future::plan(multisession)

#### Useful stuff ####

## Code in other files
source("congregation_profile.R", local = T)
source("query_dynamodb.R", local = T)
source("query_cognito.R", local = T)
source("login.R", local = T)
source("song_info_tables.R", local = T)
source("upload_history.R", local = T)
source("process_history.R", local = T)
source("summary.R", local = T)
source("help.R", local = T)

# Placeholder value for unspecified dates
date.placeholder = 99991231

# ggplot theme
theme_set(theme_bw())

#### UI ####

# Elements of the UI
page.title = "Observing Congregational Hymnody"
page.header = tags$head(tags$link(rel = "stylesheet", type = "text/css",
                                  href = "styles.css"),
                        tags$link(rel = "shortcut icon", href = "/favicon.ico",
                                  type = "image/x-icon"),
                        tags$meta(property = "og:type", content = "website"),
                        tags$meta(property = "og:url",
                                  content = "https://och.worshipsongfinder.com/"),
                        tags$meta(property = "og:title",
                                  content = "Observing Congregational Hymnody"),
                        tags$meta(property = "og:image",
                                  content = "https://och.worshipsongfinder.com/och_screenshot.png"),
                        tags$meta(property = "og:description",
                                  content = "Tool for congregations among the Churches of Christ to track their worship history"))

# Define UI
ui <- navbarPage(
  title = page.title,
  header = page.header,
  theme = bs_theme(bootswatch = "lumen"),
  login.page,
  congregation.page,
  upload.page,
  process.page,
  summary.page,
  help.page
)

#### Server ####

# Define server logic
server <- function(input, output, session) {
  
  # Allow large file uploads
  options(shiny.maxRequestSize = 30 * 1024^2)

  # Get IAM credentials
  use_credentials("och-shiny")

  # Get ready to authenticate with Cognito
  svc = cognitoidentityprovider()
  user.pool.id = Sys.getenv("OCH_COGNITO_USER_POOL")
  user.pool.client.id = Sys.getenv("OCH_COGNITO_APP_CLIENT")
  user.pool.client.secret = Sys.getenv("OCH_COGNITO_APP_SECRET")
  auth.result = reactiveVal(NULL)
  current.user = reactiveVal(NULL)
  user.attributes = reactiveVal(NULL)
  current.dates = reactiveVal(NULL)

  # Reactive table of congregations
  congregations = reactiveVal(NULL)

  # Reactive value for DynamoDB connection (only instantiated if the user
  # authenticates successfully)
  och.db = reactiveVal(NULL)

  # Currently selected row in the worship history processing table
  # (Make this a reactiveVal so that the table drop-down options recompute only
  # when the selected row changes, not any time the user selects a diferent
  # cell, even in the same row)
  current.row = reactiveVal(NULL)

  # Reactive value for S3 client (only instantiated if the user authenticates
  # successfully)
  s3.client = reactiveVal(NULL)

  # Reactive Textract progress file
  textract.progress.file = reactiveVal(paste("progress_files/textract_",
                                             session$token, ".txt", sep = ""))

  # Reactive song info tables
  song.table.task = ExtendedTask$new(function(db, st.info) {
    future_promise({
      use_credentials("och-shiny")
      lapply(
        set_names(names(st.info)),
        function(st) {
          d = scan.dynamodb(db = db, table.name = st.info[[st]])
          if(st == "song.labels") {
            d = d %>%
              arrange(SongLabel)
          } else if(st == "song.instance.labels") {
            d = d %>%
              arrange(SongInstanceLabel)
          }
          return(d)
        }
      )
    })
  })
  song.tables = reactive({
    song.table.task$result()
  })

  # Reactive processing table
  worship.history.processing = reactiveValues(
    table = NULL,
    refresh = NULL,
    changes = list(edit = c(), insert = F, delete = F)
  )

  # Reactive worship history data for all congregations
  wh.all.progress.file = reactiveVal(paste("progress_files/wh_all_",
                                           session$token, ".txt", sep = ""))
  wh.all.task = ExtendedTask$new(function(db, start.date, end.date,
                                          congregations.df, song.labels.df) {
    future_promise({
      use_credentials("och-shiny")
      populate.wh.all(congregations.df, song.labels.df, start.date, end.date,
                      db, isolate(wh.all.progress.file()))
    })
  })
  observeEvent(input$load.all.histories, {
    if(song.table.task$status() != "success") {
      showNotification("Song info not ready; try again in a moment",
                       type = "warning")
    } else {
      wh.all.progress.bar = shiny::Progress$new()
      wh.all.progress.bar$set(message = "Loading worship histories", value = 0)
      observe({
        if(!is.null(wh.all.prog())) {
          wh.all.progress.bar$set(value = wh.all.prog())
          if(wh.all.prog() == 1) {
            wh.all.progress.bar$close()
            file.remove(wh.all.progress.file())
            output$download.history.all.ui = renderUI({
              downloadButton("download.history.all", "Download all histories")
            })
          }
        }
      })
      wh.all.task$invoke(och.db(), input$view.wh.start.date,
                         input$view.wh.end.date, congregations(),
                         song.tables()$song.labels)
    }
  })
  wh.all = reactive({
    wh.all.task$result()
  })
  wh.all.prog = reactiveFileReader(100, session,
                                   isolate(wh.all.progress.file()),
    function(path) {
      if(file.exists(path)) {
        as.numeric(readLines(path, 1, warn = F))
      }
  })

  # Reactive worship history data for this congregation
  wh.me.progress.file = reactiveVal(paste("progress_files/wh_me_",
                                           session$token, ".txt", sep = ""))
  wh.me.task = ExtendedTask$new(function(db, start.date, end.date,
                                         congregations.df, song.labels.df) {
    future_promise({
      use_credentials("och-shiny")
      populate.wh.me(congregations.df, song.labels.df, start.date, end.date,
                     db, isolate(wh.me.progress.file()))
    })
  })
  observeEvent(input$load.my.history, {
    if(song.table.task$status() != "success") {
      showNotification("Song info not ready; try again in a moment",
                       type = "warning")
    } else {
      wh.me.progress.bar = shiny::Progress$new()
      wh.me.progress.bar$set(message = "Loading worship history", value = 0)
      observe({
        if(!is.null(wh.me.prog())) {
          wh.me.progress.bar$set(value = wh.me.prog())
          if(wh.me.prog() == 1) {
            wh.me.progress.bar$close()
            file.remove(wh.me.progress.file())
            output$download.history.me.ui = renderUI({
              downloadButton("download.history.me", "Download my history")
            })
          }
        }
      })
      wh.me.task$invoke(och.db(), input$view.wh.start.date,
                        input$view.wh.end.date, congregations(),
                        song.tables()$song.labels)
    }
  })
  wh.me = reactive({
    wh.me.task$result()
  })
  wh.me.prog = reactiveFileReader(100, session, isolate(wh.me.progress.file()),
    function(path) {
      if(file.exists(path)) {
        as.numeric(readLines(path, 1, warn = F))
      }
  })

  # Reactive worship history data
  wh.either = reactive({
    tryCatch(
      {
        wh.all()
      },
      shiny.silent.error = function(e) {
        wh.me() %>%
          mutate(congregation.label = congregations()$congregation.label[congregations()$is.me],
                 is.me = T)
      }
    )
  })

  # Collect topics
  observe({
    if(song.table.task$status() == "success") {
      topic.choices = sort(colnames(song.tables()$song.info))
      topic.choices = topic.choices[!(topic.choices %in% c("SongID", "SongName", "Year"))]
      updateSelectInput(session, "topics.all.topic", choices = topic.choices,
                        selected = topic.choices[1])
    }
  })

  # Reactive congregation for which we want to list hapaxes
  hapax.congregation = reactiveVal(NULL)

  # Reactive decades for which we want to list songs
  year.me = reactiveVal(NULL)
  year.all = reactiveVal(NULL)
  congregation.year = reactiveVal(NULL)

  # Reactive topic for which we want to show a table
  topics.me.topic = reactiveVal(NULL)

  # When the user changes a password, do that
  observeEvent(input$change.password, {
    change.password(svc, input$change.username, input$change.old.password,
                    input$change.new.password, user.pool.client.id,
                    user.pool.client.secret)
  })

  # If the user forgot his or her password, start the reset process
  observeEvent(input$forgot.password, {
    forgot.password(svc, input$forgotten.username, user.pool.client.id,
                    user.pool.client.secret)
  })

  # When the user attempts to complete the password reset process, do that
  observeEvent(input$reset.password, {
    reset.password(svc, input$reset.username, input$reset.code,
                   input$reset.new.password, user.pool.client.id,
                   user.pool.client.secret)
  })
  
  # When the user attempts to log in, attempt to create a connection and
  # get congregation info
  observeEvent(input$log.in, {

    # Log in
    tryCatch(
      {
        auth.result(log.in(svc, input$existing.username,
                           input$existing.password, user.pool.client.id,
                           user.pool.client.secret))
        showNotification("Successfully logged in; getting worship history...",
                         type = "message")
        och.db(dynamodb(endpoint = "dynamodb.us-west-2.api.aws"))
        s3.client(s3(endpoint = "https://s3.dualstack.us-west-2.amazonaws.com"))
        current.user(input$existing.username)
        user.attributes(svc$get_user(auth.result()$AuthenticationResult$AccessToken)$UserAttributes)
        congregations(get.users(svc, user.pool.id, date.placeholder, 
                                current.user()) %>% filter(share | is.me))
        current.dates(get.current.dates(och.db(), current.user()))
        song.table.task$invoke(och.db(), song.table.info)
      },
      error = function(err) {
        print(err)
        showNotification(err, type = "error")
      }
    )
  })
  
  # Automatically update the UI based on the congregation profile we retrived
  # from Cognito
  observe({ update.attribute.ui(user.attributes(), session) })

  # When the user attempts to update the congregation's profile, do that
  observeEvent(input$save.congregation, {
    update.attributes(lapply(set_names(attribute.ids),
                             function(x) { input[[x]] }), svc,
                      auth.result()$AuthenticationResult$AccessToken)
  })

  # When the user uploads a file, write its contents to the S3 bucket
  observeEvent(input$upload.wh.file, {
    
    # Determine the suffix that will be added to each filename: the current
    # timestamp for spreadsheets, and the worship date for bulletins.
    filename.suffix = strftime(Sys.time(), "%Y%m%d%H%M%S")
    filename.suffix = as.numeric(filename.suffix) + 1:nrow(input$wh.file)
    filename.suffix = as.character(filename.suffix)
    if(input$wh.file.type == "Bulletin") {
      trimmed.filename = unlist(gsub(".pdf$", "", input$wh.file$name))
      month.abbrs = "jan(uary)?|feb(ruary)?|mar(ch)?|apr(il)?|may|june?|july?|aug(ust)?|sep(tember)?|oct(ober)?|nov(ember)?|dec(ember)?"
      date.part.1 = paste("([0-9]+|", month.abbrs, ")", sep = "")
      date.part.2 = paste("([0-9]+|", month.abbrs, ")", sep = "")
      date.part.3 = "[0-9]+"
      date.regex = paste(date.part.1, "[-_ .]*", date.part.2, "[-_ ,.]*",
                         date.part.3, sep = "")
      file.worship.date = regmatches(tolower(trimmed.filename),
                                     regexpr(date.regex,
                                             tolower(trimmed.filename)))
      file.worship.date = parse_date_time(file.worship.date,
                                          orders = c("mdy", "ymd", "dmy"))
      filename.suffix = strftime(file.worship.date, "%Y%m%d")
    }

    # Determine the filenames
    filename = paste(current.user(), "/", current.user(), "_",
                     filename.suffix, sep = "")

    # Spreadsheets
    if(input$wh.file.type == "Spreadsheet") {
      
      # Add the correct suffix to the filenames
      filename = paste(filename, ".csv", sep = "")

      # Iterate over files
      for(i in 1:nrow(input$wh.file)) {
        
        # Read in as a dataframe
        if(grepl("xlsx?$", input$wh.file[[i,"name"]])) {
          wh.df = read_excel(input$wh.file[[i,"datapath"]], skip = 1,
                             col_names = c("WorshipDate", "Song"),
                             col_types = c("date", "text"))
        } else if(grepl("csv$", input$wh.file[[i,"name"]])) {
          wh.df = read.csv(input$wh.file[[i,"datapath"]], header = T)
        }
        wh.df$WorshipDate = as.Date(wh.df$WorshipDate)
        
        # If the user doesn't want to overwrite previously entered dates,
        # remove those rows
        if(!input$wh.file.overwrite) {
          wh.df = wh.df %>%
            filter(!(WorshipDate %in% format(current.dates()$worship.date,
                                             "%Y%m%d")))
        }
        
        # Write the file to S3
        tryCatch(
          {
            s3.client()$put_object(Bucket = "worship-och-tables",
                                   Body = charToRaw(format_csv(wh.df)),
                                   Key = filename[i])
            showNotification(paste("File", i, "successfully saved"),
                             type = "message")
          },
          error = function(err) {
            print(err)
            showNotification(paste("File", i, "could not be saved"),
                             type = "error")
          }
        )
        
      }
      
    }
    
    # Bulletins
    else if(input$wh.file.type == "Bulletin") {

      # Add the correct suffix to the filenames
      filename = paste(filename, ".pdf", sep = "")
      
      # Iterate over files
      for(i in 1:nrow(input$wh.file)) {
        
        # Display a warning if we're skipping this file because it's a
        # previously entered date
        if(filename.suffix[i] %in% format(current.dates()$worship.date,
                                          "%Y%m%d") &
           !input$wh.file.overwrite) {
          showNotification(paste("Date already entered:", filename.suffix[i],
                                 sep = "\n"), type = "warning")
        }
        
        # Otherwise, proceed as normal
        else {
          
          # Write the pdf to S3
          tryCatch(
            {
              s3.client()$put_object(Bucket = "worship-och-bulletins",
                                     Body = input$wh.file[[i,"datapath"]],
                                     Key = filename[i])
              showNotification(paste("pdf successfully saved:",
                                     gsub(".*/", "", filename[i]),
                                     sep = "\n"), type = "message")
            },
            error = function(err) {
              print(err)
              showNotification("pdf could not be saved", type = "error")
            }
          )
          
          # Attempt to extract text from the pdf
          temp.wh.text = pdf_text(input$wh.file[i,"datapath"])
          temp.wh.text = unlist(strsplit(temp.wh.text, "\n|     +"))
          temp.wh.text = gsub("^[[:space:]]+|[[:space:]]+$", "", temp.wh.text)
          temp.wh.text = temp.wh.text[temp.wh.text != ""]
          
          # If we got text, save it to the file
          if(length(temp.wh.text) > 0) {
            writeLines(temp.wh.text, con = textract.progress.file())
          }
          
          # If we didn't get any text, it's a scanned image and we have to use
          # Textract
	  else {
            if(exists("textract_result")) {
              rm(textract_result)
            }
            textract = textract(endpoint = "https://textract.us-west-2.api.aws")
            resp = textract$start_document_text_detection(
              DocumentLocation = list(
                S3Object = list(Bucket = "worship-och-bulletins",
                                Name = filename[i])
              )
            )
            textract.wait.task = ExtendedTask$new(function(t, jid,
                                                           progress.file) {
              future_promise({
                textract_result = t$get_document_text_detection(JobId = jid)
                count = 0
                while(count <= 60 &&
                      textract_result$JobStatus == "IN_PROGRESS") {
                  Sys.sleep(1)
                  count = count + 1
                  textract_result = t$get_document_text_detection(JobId = jid)
                }
                if(textract_result$JobStatus == "SUCCEEDED") {
                  wh.t = c()
                  get.results = T
                  while(get.results) {
                    temp.wh.text =  purrr::map_chr(textract_result$Blocks,
                                                   function(b) {
                                                     line = b$Text
                                                     if(length(line) == 0 |
                                                        b$BlockType != "LINE") {
                                                       line = ""
                                                     }
                                                     return(line)
                                                   })
                    temp.wh.text = gsub("^[[:space:]]+|[[:space:]]+$", "",
                                        temp.wh.text)
                    wh.t = c(wh.t, temp.wh.text[temp.wh.text != ""])
                    if(length(textract_result$NextToken) == 0) {
                      get.results = F
                    } else {
                      textract_result = t$get_document_text_detection(JobId = jid,
                                                                      NextToken = textract_result$NextToken)
                    }
                  }
                }
                writeLines(wh.t, con = progress.file)
              })
            })
            textract.wait.task$invoke(textract, resp$JobId,
                                      textract.progress.file())
          }

          # When the extracted text is ready, it will be in this file
          wh.text = reactiveFileReader(1000, session,
                                       isolate(textract.progress.file()),
            function(path) {
              if(file.exists(path)) {
                t = readLines(path, warn = F)
                file.remove(path)
                return(t)
              }
          })
          
          # Create a dataframe
          observe({
            if(!is.null(wh.text())) {
              wh.df = data.frame(WorshipDate = rep(strftime(file.worship.date[i],
                                                            "%Y-%m-%d"),
                                                   length(wh.text())),
                                 RawLine = wh.text())

              # Write the dataframe to S3 as a csv
              tryCatch(
                {
                  if(nrow(wh.df) > 0) {
                    s3.client()$put_object(Bucket = "worship-och-tables",
                                           Body = charToRaw(format_csv(wh.df)),
                                           Key = gsub("pdf$", "csv", filename[i]))
                    showNotification("csv successfully saved", type = "message")
                  } else {
                    showNotification("csv could not be saved", type = "error")
                  }
                },
                error = function(err) {
                  print(err)
                  showNotification(err, type = "error")
                  showNotification("csv could not be saved", type = "error")
                }
              )
	    }
          })
         
        }
        
        Sys.sleep(1)
        
      }
      
    }
    
  })

  # Update the date picker for the processing table
  observe({
    tryCatch(
      {
        temp.dates = get.processing.dates(current.dates(),
                                          input$process.wh.filter,
                                          isolate(input$process.wh.date))
        output$process.wh.date = renderUI({
          airDatepickerInput("process.wh.date", "Choose date:",
                             value = temp.dates$selected,
                             minDate = temp.dates$min, maxDate = temp.dates$max,
                             highlightedDates = temp.dates$highlighted)
        })
      },
      error = function(err) {
        print(err)
        showNotification(err, type = "error")
      }
    )
  })

  # Populate processing table (and refresh as needed)
  observe({
    current.row(input$process.worship.history_select$select$r)
  })
  observeEvent(
    {
      worship.history.processing$refresh
      input$process.wh.date
    },
    {
      worship.history.processing$table = populate.processing.table(och.db(),
                                                                   current.user(),
                                                                   input$process.wh.date)
      worship.history.processing$refresh = NULL
    }
  )
  output$process.worship.history = renderRHandsontable({
    create.worship.history.hot(worship.history.processing$table,
                               input$process.wh.date,
                               song.tables()$song.labels,
                               song.tables()$song.instance.labels,
                               input$dimension[1], input$dimension[2],
                               current.row())
  })

  # When the user requests a data refresh, do that
  observeEvent(input$refresh.process.wh, {
    current.dates(get.current.dates(och.db(), current.user()))
    worship.history.processing$refresh = T
  })
  
  # When the user requests to mark all records as processed, do that
  observeEvent(input$mark.all.processed.wh, {
    worship.history.processing$table = worship.history.processing$table %>%
      mutate(ProcessedRecord = T)
    worship.history.processing$changes$edit = c(worship.history.processing$changes$edit,
                                                worship.history.processing$table$HistoryID)
  })
  
  # When the user makes a change to the processing table, update the
  # underlying table accordingly
  observeEvent(input$process.worship.history, {
    update.worship.history.hot(input$process.worship.history$changes,
	                       input$process.wh.date, song.tables()$song.labels,
                               song.tables()$song.instance.labels,
                               worship.history.processing)
  })

  # When the user clicks the "Save worship history" button, attempt to save
  # the changes made by the user to the database (and refresh related tables)
  observeEvent(input$save.worship.history, {
    save.worship.history.table(worship.history.processing, current.user(),
                               input$process.wh.date, och.db())
    current.dates(get.current.dates(och.db(), current.user()))
    worship.history.processing$refresh = T
  })

  # Update the date pickers for the summary table
  observeEvent(current.dates(), {
    tryCatch(
      {
        temp.dates = get.history.dates(current.dates(),
                                       input$process.wh.start.date,
                                       input$process.wh.end.date)
        output$view.wh.start.date = renderUI({
          airDatepickerInput("view.wh.start.date", "Choose start date:",
                             value = temp.dates$start,
                             highlightedDates = temp.dates$highlighted)
        })
        output$view.wh.end.date = renderUI({
          airDatepickerInput("view.wh.end.date", "Choose end date:",
                             value = temp.dates$end,
                             highlightedDates = temp.dates$highlighted)
        })
      },
      error = function(err) {
        print(err)
        showNotification(err, type = "error")
      }
    )
  })

  # Worship history row count
  output$wh.row.count = renderTable({
    wh.either() %>%
      summarise(`Total congregations` = n_distinct(congregation.label),
                `Total dates` = n_distinct(worship.date),
                `Total songs` = n_distinct(song.label),
                `Total observations` = n()) %>%
      pivot_longer(cols = everything()) %>%
      mutate(value = format(value, big.mark = ",", trim = T))
  }, colnames = F)

  # Download worship history for this congregation
  output$download.history.me = downloadHandler(
    filename = function() {
      cong = congregations()$name[congregations()$congregation == current.user()]
      paste(cong, " ", strftime(input$view.wh.start.date, "%b %d %Y"), " - ",
            strftime(input$view.wh.end.date, "%b %d %Y"), ".csv", sep = "")
    },
    content = function(file) {
      song.info.df = song.tables()$song.info %>%
        pivot_longer(cols = -c("SongID", "SongName", "Year"),
                     names_to = "topic", values_to = "about") %>%
        filter(about) %>%
        group_by(SongID, Year) %>%
        arrange(topic) %>%
        summarise(topics = paste(topic, collapse = ", "), .groups = "drop")
      wh.me() %>%
        left_join(song.info.df, by = c("song.id" = "SongID")) %>%
        dplyr::select(worship_date = worship.date, history_id = history.id,
                      raw_line = raw.line, processed_record = processed.record,
                      sunday_morning = sunday.morning, song_id = song.id,
                      song_instance_id = song.instance.id, notes,
                      new_song = new.song, song_title = song.title,
                      song_year = Year, topics) %>%
        write.csv(file, na = "", row.names = F)
    }
  )

  # Download worship history for all congregations
  output$download.history.all = downloadHandler(
    filename = function() {
      paste("worship histories ",
            strftime(input$view.wh.start.date, "%b %d %Y"), " - ",
            strftime(input$view.wh.end.date, "%b %d %Y"), ".csv", sep = "")
    },
    content = function(file) {
      song.info.df = song.tables()$song.info %>%
        pivot_longer(cols = -c("SongID", "SongName", "Year"),
                     names_to = "topic", values_to = "about") %>%
        filter(about) %>%
        group_by(SongID, Year) %>%
        arrange(topic) %>%
        summarise(topics = paste(topic, collapse = ", "), .groups = "drop")
      wh.all() %>%
        left_join(song.info.df, by = c("song.id" = "SongID")) %>%
        mutate(congregation_size = na_if(size, "[not specified]")) %>%
        dplyr::select(congregation_name = congregation.name,
                      congregation_label = congregation.label,
                      congregation_size, praise_team_since = praise,
                      women_leading_since = women, worship_date = worship.date,
                      song_id = song.id, song_title = song.title,
                      song_year = Year, topics) %>%
        write.csv(file, na = "", row.names = F)
    }
  )

  # Graph of worship history dates for this congregation
  output$data.dates.me = renderPlotly({
    data.dates.me(wh.either())
  })

  # Graph of worship history dates for all congregations
  output$data.dates.all = renderPlotly({
    data.dates.all(wh.either())
  })

  # Worship history summary table
  output$my.worship.history = renderDT({
    req(wh.either())
    my.wh.table(wh.either())
  })

  # Graph of number of songs sung per Sunday for this congregation
  output$number.sung.sunday.me = renderPlotly({
    songs.per.sunday.me(wh.either())
  })

  # Graph of number of songs sung per Sunday for all congregations
  output$number.sung.sunday.all = renderPlotly({
    songs.per.sunday.all(wh.either())
  })

  # Graph most frequently sung songs by congregation for this congregation
  output$top.songs.me = renderPlotly({
    top.songs.me(wh.either(), input$top.songs.me.n, input$top.songs.me.metric)
  })

  # Graph most frequently sung songs by congregation for all congregations
  output$top.songs.all = renderPlotly({
    top.songs.all(wh.either(), input$top.songs.all.n, input$top.songs.all.metric)
  })

  # Graph most frequently sung songs overall
  output$top.songs.overall = renderPlotly({
    top.songs.overall(wh.either(), input$top.songs.overall.n)
  })

  # Summary of songs sung once for this congregation
  output$hapaxes.me = renderTable({
    create.hapax.me.table(wh.either())
  }, colnames = F)

  # Table of songs sung once for this congregation
  output$hapax.me.list = renderDT({
    create.hapax.me.list(wh.either())
  })

  # Graph of songs sung once for all congregations
  output$hapaxes_all_p = renderPlotly({hapaxes.all(wh.either(),
                                                   input$hapaxes.all.metric)})
  output$hapaxes.all = renderUI({
    plotlyOutput("hapaxes_all_p",
                 height = hapax.all.height(wh.either()))
  })

  # Display table of songs sung once
  observeEvent(event_data(event = "plotly_click", source = "hapax.all.plot"), {
    hapax.click.data = event_data("plotly_click", source = "hapax.all.plot")
    hapax.congregation(hapax.click.data$y)
  })
  output$hapax.all.list = renderDT({
    if(!is.null(hapax.congregation())) {
      create.hapax.all.list(wh.either(), hapax.congregation())
    }
  })

  # Graph song year for this congregation
  output$song.year.me = renderPlotly({
    song.year.me(wh.either(), song.tables()$song.info)
  })

  # Display table of songs by decade for this congregation
  observeEvent(event_data(event = "plotly_click", source = "song.year.me"), {
    year.me.click.data = event_data("plotly_click", source = "song.year.me")
    year.me(year.me.click.data$x)
  })
  output$year.me.list = renderDT({
    if(!is.null(year.me())) {
      create.year.me.list(wh.either(), song.tables()$song.info, year.me())
    }
  })

  # Graph song year for all congregations
  output$song_year_all_p = renderPlotly({
    song.year.all(wh.either(), song.tables()$song.info)
  })
  output$song.year.all = renderUI({
    plotlyOutput("song_year_all_p",
                 height = song.year.all.height(wh.either())$plot.height)
  })

  # Display table of songs by decade for all congregations
  observeEvent(event_data(event = "plotly_click", source = "song.year.all"), {
    year.all.click.data = event_data("plotly_click", source = "song.year.all")
    year.all(year.all.click.data$x)
    congregation.year(year.all.click.data$customdata)
  })
  output$year.all.list = renderDT({
    if(!is.null(year.all())) {
      create.year.all.list(wh.either(), song.tables()$song.info, year.all(),
                           congregation.year())
    }
  })

  # Graph of topics for this congregation
  output$topics_me_p = renderPlotly({
    topics.me(wh.either(), song.tables()$song.info, input$topics.me.metric)
  })
  output$topics.me = renderUI({
    plotlyOutput("topics_me_p",
                 height = topics.me.height(wh.either(),
                                           song.tables()$song.info))
  })

  # Display table of songs for this congregation by clicked topic
  observeEvent(event_data(event = "plotly_click", source = "topics.me.plot"), {
    topics.me.click.data = event_data("plotly_click",
                                       source = "topics.me.plot")
    topics.me.topic(topics.me.click.data$y)
  })
  output$topics.me.list = renderDT({
    if(!is.null(topics.me.topic())) {
      create.topics.me.table(wh.either(), song.tables()$song.info,
                             topics.me.topic())
    }
  })

  # Graph of topics for all congregations
  output$topics_all_p = renderPlotly({
    topics.all(wh.either(), song.tables()$song.info, input$topics.all.topic,
               input$topics.all.metric)
  })
  output$topics.all = renderUI({
    plotlyOutput("topics_all_p",
                 height = topics.all.height(wh.either(),
                                            song.tables()$song.info,
                                            input$topics.all.topic))
  })

  # Display table of songs by topic
  output$topics.all.list = renderDT({
    if(!is.null(wh.either())) {
      create.topics.all.table(wh.either(), song.tables()$song.info,
                              input$topics.all.topic)
    }
  })

}

#### Run ####

# Run the application 
shinyApp(ui = ui, server = server)

