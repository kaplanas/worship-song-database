library(shiny)
library(shinyjs)
library(RMySQL)
library(tidyverse)
library(date)
library(glue)
library(aws.ec2metadata)
library(rhandsontable)
library(DT)
library(tidygeocoder)
library(shinyWidgets)
library(lubridate)
library(aws.s3)
library(readxl)
library(paws)
library(pdftools)
library(ggmap)

#### Useful stuff ####

# Code in other files
source("label_tables.R", local = T)
source("selectors.R", local = T)
source("tables.R", local = T)
source("worship_histories.R", local = T)
source("summary.R", local = T)

# ggplot theme
theme_set(theme_bw())

# Notifications
show.changes.saved = function(success, db.table = NULL, err.msg = NULL) {
  if(success) {
    showNotification(paste("Changes saved to", db.table, sep = " "),
                     type = "message")
  } else {
    showNotification(paste("Some changes may not have been saved", err.msg,
                           sep = "\n"), type = "error")
  }
}

#### UI ####

# Elements of the UI
page.title = "Observing Congregational Hymnody"
login.page = tabPanel("Log in",
                      tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                '),
                                tags$style(HTML(".airdatepicker-highlighted {
                                                     background: #ADD8E6;
                                                 }"))),
                      textInput("och.username", "Username"),
                      passwordInput("och.password", "Password"),
                      actionButton("och.log.in", label = "Log in"))

# Define UI
ui <- navbarPage(
  page.title,
  login.page,
  tables.page,
  worship.history.page,
  summary.page
)

#### Server ####

# Define server logic
server <- function(input, output, session) {
  
  # Allow large file uploads
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  # Database connection
  och.con = reactiveVal(NULL)
  
  # Reactive label tables
  label.tables = do.call(
    "reactiveValues",
    lapply(label.table.sql, function(label.table) { NULL })
  )
  label.refresh = do.call(
    "reactiveValues",
    lapply(label.table.sql, function(label.table) { T })
  )
  
  # Track selector lists and when to update them
  selector.list = do.call(
    "reactiveValues",
    lapply(selector.info, function(selector) { NULL })
  )
  selector.refresh = do.call(
    "reactiveValues",
    lapply(selector.info, function(selector) { T })
  )
  
  # Reactive reference tables
  reference.tables = do.call(
    "reactiveValues",
    lapply(reference.table.info, function(reference.table) { NULL })
  )
  reference.refresh = do.call(
    "reactiveValues",
    lapply(reference.table.info, function(reference.table) { T })
  )
  reference.changes = do.call(
    "reactiveValues",
    lapply(
      reference.table.info,
      function(reference.table) {
        list(edit = c(), insert = F, delete = F)
      }
    )
  )
  
  # Reactive processing table
  worship.history.processing = reactiveValues(
    table = NULL,
    refresh = T,
    changes = list(edit = c(), insert = F, delete = F)
  )
  
  # Reactive summary tables
  summary.tables = do.call(
    "reactiveValues",
    lapply(summary.table.info, function(summary.table) { NULL })
  )
  summary.refresh = do.call(
    "reactiveValues",
    lapply(summary.table.info, function(summary.table) { T })
  )
  
  # When the user attempts to log in, attempt to create a connection and
  # populate the tables
  observeEvent(input$och.log.in, {
    
    # Try to create a connection; if this fails, set the connection to null
    tryCatch(
      {
        och.con(dbConnect(MySQL(), user = input$och.username,
                          password = input$och.password, host = "localhost",
                          port = 3306, dbname = "och"))
        dbGetQuery(och.con(), "SET NAMES utf8")
        showNotification("Login successful", type = "message")
      },
      error = function(err) {
        print(err)
        if(!is.null(och.con())) {
          dbDisconnect(och.con())
        }
        och.con(NULL)
        showNotification("Login failed", type = "error")
        return()
      }
    )
    
    # Label tables
    purrr::walk(
      names(label.table.sql),
      function(lt) {
        
        # Determine which reference tables are related to this label table
        related.reference.tables = c()
        for(rt in names(reference.table.info)) {
          if(lt %in% reference.table.info[[rt]]$columns$key.table) {
            related.reference.tables = c(related.reference.tables, rt)
          }
        }
        
        # Populate the label table (and refresh as needed)
        observeEvent(label.refresh[[lt]], {
          label.tables[[lt]] = populate.label.table(lt, och.con())
          label.refresh[[lt]] = NULL
          for(rt in related.reference.tables) {
            reference.refresh[[rt]] = T
          }
        })
        
      }
    )
    
    # Selector lists and selectors
    purrr::walk(
      names(selector.info),
      function(sn) {
        
        # Selector list
        purrr::walk(
          selector.info[[sn]]$input.dependencies,
          function(id) {
            observeEvent(input[[id]], {
              sub.input.dependencies = list()
              for(sub.id in selector.info[[sn]]$input.dependencies) {
                sub.input.dependencies[[sub.id]] = input[[sub.id]]
              }
              selector.list[[sn]] = update.selector.list(sn, och.con(),
                                                         sub.input.dependencies)
            })
          }
        )
        observeEvent(selector.refresh[[sn]], {
          input.dependencies = list()
          for(id in selector.info[[sn]]$input.dependencies) {
            input.dependencies[[id]] = input[[id]]
          }
          selector.list[[sn]] = update.selector.list(sn, och.con(),
                                                     input.dependencies)
          selector.refresh[[sn]] = NULL
        })
        
        # Selector
        sel.info = selector.info[[sn]]
        output[[sn]] = renderUI({
          if(sel.info$type == "select") {
            current.selected = input[[sn]]
            selectizeInput(sn, sel.info$label, choices = selector.list[[sn]],
                           selected = current.selected, width = sel.info$width)
          } else if(sel.info$type == "date") {
            min.date = NULL
            max.date = NULL
            selected.date = NULL
            if(input$process.wh.show.all.entered) {
              selected.date = Sys.Date()
            }
            else if(length(selector.list[[sn]]) > 0 ) {
              min.date = min(selector.list[[sn]])
              max.date = max(selector.list[[sn]])
              selected.date = min.date
            }
            airDatepickerInput(sn, sel.info$label, value = selected.date,
                               minDate = min.date, maxDate = max.date,
                               highlightedDates = selector.list[[sn]])
          }
        })
        
      }
    )
    
    # Reference tables
    purrr::walk(
      names(reference.table.info),
      function(rt) {
        
        # Populate tables (and refresh as needed)
        observeEvent(reference.refresh[[rt]], {
          reference.tables[[rt]] = populate.reference.table(rt, och.con(),
                                                            label.tables)
          reference.refresh[[rt]] = NULL
        })
        
        # Render as handsontable
        output[[rt]] = renderRHandsontable({
          create.reference.hot(reference.tables[[rt]], rt, label.tables,
                               input$dimension[1], input$dimension[2])
        })
        
        # When the user makes an update through the UI, propagate the changes to
        # the table
        observeEvent(input[[rt]]$change, {
          update.reference.table(rt, input[[rt]]$change, reference.tables,
                                 reference.changes)
        })
        
        # When the user clicks the "Save" button, attempt to write the table to
        # the database and update related tables/selectors
        observeEvent(input[[paste("save", rt, sep = ".")]], {
          save.reference.table(rt, och.con(), reference.tables, label.tables,
                               reference.changes)
          reference.refresh[[rt]] = T
          for(lt in reference.table.info[[rt]]$related.label.tables) {
            label.refresh[[lt]] = T
          }
          for(s in reference.table.info[[rt]]$related.selectors) {
            selector.refresh[[s]] = T
          }
        })
        
      }
    )
    
    # When the user uploads a file, write its contents to the S3 bucket
    observeEvent(input$upload.wh.file, {
      
      # Determine the suffix that will be added to the filename: the current
      # timestamp for spreadsheets, and the worship date for bulletins.
      filename.suffix = strftime(Sys.time(), "%Y%m%d%H%M%S")
      if(input$wh.file.type == "Bulletin") {
        trimmed.filename = gsub(".pdf$", "", input$wh.file$name)
        month.abbrs = "jan(uary)?|feb(ruary)?|mar(ch)?|apr(il)?|may|june?|july?|aug(ust)?|sep(tember)?|oct(ober)?|nov(ember)?|dec(ember)?"
        date.part.1 = paste("([0-9]+|", month.abbrs, ")", sep = "")
        date.part.2 = paste("([0-9]+|", month.abbrs, ")", sep = "")
        date.part.3 = "[0-9]+"
        date.regex = paste(date.part.1, "[-_ ]*", date.part.2, "[-_ ,.]*",
                           date.part.3, sep = "")
        file.worship.date = regmatches(tolower(trimmed.filename),
                                       regexpr(date.regex,
                                               tolower(trimmed.filename)))
        file.worship.date = parse_date_time(file.worship.date,
                                            orders = c("mdy", "ymd", "dmy"))
        filename.suffix = strftime(file.worship.date, "%Y%m%d")
      }
      
      # Determine the filename
      filename = reference.tables$congregations %>%
        filter(CongregationID == input$wh.file.congregation.id) %>%
        pull(FolderName)
      filename = paste(filename, "/", filename, "_", filename.suffix, sep = "")
      
      # Spreadsheets
      if(input$wh.file.type == "Spreadsheet") {
        
        # Read in as a dataframe
        wh.df = read_excel(input$wh.file$datapath,
                           col_names = c("WorshipDate", "Song"),
                           col_types = c("date", "text"))
        
        # If the user doesn't want to overwrite previously entered dates,
        # remove those rows
        if(!input$wh.file.overwrite) {
          existing.dates.sql = "SELECT DISTINCT WorshipDate
                                FROM och.worshiphistory
                                WHERE CongregationID = {input$wh.file.congregation.id}"
          existing.dates = ymd(dbGetQuery(och.con(),
                                          glue_sql(existing.dates.sql,
                                                   .con = och.con()))$WorshipDate)
          wh.df = wh.df %>%
            filter(!(as.Date(WorshipDate) %in% existing.dates))
        }
        
        # Add the correct suffix to the filename
        filename = paste(filename, ".csv", sep = "")
        
        # Write the file to S3
        tryCatch(
          {
            s3write_using(wh.df, FUN = write.csv,
                          object = paste("observing_congregational_hymnody",
                                         filename, sep = "/"), row.names = F,
                          bucket = "worship-song-database")
            showNotification("File successfully saved", type = "message")
          },
          error = function(err) {
            print(err)
            showNotification("File could not be saved", type = "error")
          }
        )
        
      }
      
      # Bulletins
      else if(input$wh.file.type == "Bulletin") {
        
        # Add the correct suffix to the filename
        filename = paste(filename, ".pdf", sep = "")
        
        # Write the pdf to S3
        tryCatch(
          {
            put_object(file = input$wh.file$datapath[1],
                       object = paste("observing_congregational_hymnody",
                                      filename, sep = "/"),
                       bucket = "worship-song-database")
            showNotification(paste("pdf successfully saved", filename.suffix,
                                   sep = "\n"), type = "message")
          },
          error = function(err) {
            print(err)
            showNotification("pdf could not be saved", type = "error")
          }
        )
        
        # Attempt to extract text from the pdf
        wh.text = pdf_text(input$wh.file$datapath)
        wh.text = unlist(strsplit(wh.text, "\n"))
        wh.text = gsub("^[[:space:]]+|[[:space:]]+$", "", wh.text)
        wh.text = wh.text[wh.text != ""]
        
        # If we didn't get any text, it's a scanned image and we have to use
        # Textract
        if(length(wh.text) == 0) {
          textract = textract()
          resp = textract$start_document_text_detection(
            DocumentLocation = list(
              S3Object = list(Bucket = "worship-song-database",
                              Name = paste("observing_congregational_hymnody",
                                           filename, sep = "/"))
            )
          )
          count = 0
          while(count <= 60 &&
                (!exists("result") || result$JobStatus == "IN_PROGRESS")) {
            Sys.sleep(1)
            result = textract$get_document_text_detection(JobId = resp$JobId)
            count = count + 1
          }
          if(result$JobStatus == "SUCCEEDED") {
            wh.text = c()
            get.results = T
            while(get.results) {
              temp.wh.text =  purrr::map_chr(result$Blocks,
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
              wh.text = c(wh.text, temp.wh.text[temp.wh.text != ""])
              if(length(result$NextToken) == 0) {
                get.results = F
              } else {
                result = textract$get_document_text_detection(JobId = resp$JobId,
                                                              NextToken = result$NextToken)
              }
            }
          }
        }
        
        # Create a dataframe
        wh.df = data.frame(WorshipDate = rep(strftime(file.worship.date,
                                                      "%Y-%m-%d"),
                                             length(wh.text)),
                           RawLine = wh.text)
        
        # Write the dataframe to S3 as a csv
        filename = gsub("pdf$", "csv", filename)
        tryCatch(
          {
            if(nrow(wh.df) > 0) {
              s3write_using(wh.df, FUN = write.csv,
                            object = paste("observing_congregational_hymnody",
                                           filename, sep = "/"), row.names = F,
                            bucket = "worship-song-database")
              showNotification("csv successfully saved", type = "message")
            } else {
              showNotification("csv could not be saved", type = "error")
            }
          },
          error = function(err) {
            print(err)
            showNotification("csv could not be saved", type = "error")
          }
        )
        
      }
      
    })
    
    # When the user requests a data refresh, do that
    observeEvent(input$refresh.process.wh, {
      for(lt in names(label.table.sql)) {
        label.refresh[[lt]] = T
      }
      for(sn in c("process.wh.congregation.id", "process.wh.date")) {
        selector.refresh[[sn]] = T
      }
      for(rt in names(reference.table.info)) {
        reference.refresh[[rt]] = T
      }
      worship.history.processing$refresh = T
    })
    
    # When the user requests to mark all records as processed, do that
    observeEvent(input$mark.all.processed.wh, {
      worship.history.processing$table = worship.history.processing$table %>%
        mutate(Processed = T)
      worship.history.processing$changes$edit = c(worship.history.processing$changes$edit,
                                                  worship.history.processing$table$WorshipHistoryID)
    })
    
    # Populate processing table (and refresh as needed)
    observeEvent(
      {
        worship.history.processing$refresh
        input$process.wh.congregation.id
        input$process.wh.date
      },
      {
        worship.history.processing$table = populate.processing.table(och.con(),
                                                                     input$process.wh.congregation.id,
                                                                     input$process.wh.date)
        worship.history.processing$refresh = NULL
      }
    )
    output$process.worship.history = renderRHandsontable({
      create.worship.history.hot(worship.history.processing$table, label.tables,
                                 input$dimension[1], input$dimension[2],
                                 input$process.worship.history_select$select$r)
    })

    # When the user makes a change to the songbook processing table, update the
    # underlying table accordingly
    observeEvent(input$process.worship.history, {
      update.worship.history.hot(input$process.worship.history$changes,
                                 worship.history.processing)
    })

    # When the user clicks the "Save worship history" button, attempt to save
    # the changes made by the user to the database (and refresh related tables)
    observeEvent(input$save.worship.history, {
      save.worship.history.table(worship.history.processing,
                                 input$process.wh.congregation.id,
                                 input$process.wh.date, label.tables, och.con())
      for(sn in c("process.wh.congregation.id", "process.wh.date",
                  "song.count.time")) {
        selector.refresh[[sn]] = T
      }
      worship.history.processing$refresh = T
      for(st in c("song.counts", "congregation.counts", "date.counts",
                  "congregation.counts.map")) {
        summary.refresh[[st]] = T
      }
    })

    # Summary tables
    purrr::walk(
      names(summary.table.info),
      function(st) {
        observeEvent(summary.refresh[[st]], {
          summary.tables[[st]] = populate.summary.table(st, och.con(),
                                                        list(song.count.time = input$song.count.time))
          summary.refresh[[st]] = NULL
        })
        walk(
          summary.table.info[[st]]$input.dependencies,
          function(id) {
            observeEvent(input[[id]], {
              summary.refresh[[st]] = T
            })
          }
        )
        if(summary.table.info[[st]]$type == "table") {
          output[[st]] = renderDT({
            temp.dt = summary.tables[[st]] %>%
              mutate(across(matches("SongLabel"),
                            function(x) { gsub("\n", "<br/>", x )})) %>%
              datatable(options = list(searching = F, paging = F, info = F,
                                       columnDefs = list(list(visible = F,
                                                              targets = summary.table.info[[st]]$hidden.columns))),
                        rownames = F, escape = F)
            if(st == "song.counts") {
              temp.dt = temp.dt %>%
                formatStyle("Restoration", target = "row",
                            backgroundColor = styleEqual("Y", "yellow"))
            }
            temp.dt
          })
        } else if(st == "date.counts") {
          output[[st]] = renderPlot({
            if(!is.null(summary.tables[[st]])) {
              summary.tables[[st]] %>%
                mutate(Year = as.numeric(substr(WorshipDate, 1, 4)),
                       WorshipDate = paste("2000",
                                           substr(WorshipDate,
                                                  5, nchar(WorshipDate)),
                                           sep = ""),
                       WorshipDate = ymd(WorshipDate)) %>%
                ggplot(aes(x = WorshipDate, y = TotalCongregations)) +
                geom_bar(stat = "identity") +
                scale_x_date(date_labels = "%b") +
                facet_grid(Year ~ .) +
                expand_limits(y = 0)
            }
          })
        } else if(st == "congregation.counts.map") {
          usmap = get_stamenmap(bbox = c(bottom = 24.5, top = 49.5,
                                         right = -66, left = -125),
                                zoom = 4, maptype = "toner-background")
          output[[st]] = renderPlot({
            ggmap(usmap) +
              geom_point(data = summary.tables[[st]],
                         aes(x = Longitude, y = Latitude, size = TotalDates),
                         color = "green", alpha = 0.5, shape = 16) +
              theme(legend.position = "bottom")
          })
        }
      }
    )

  })

  # Disconnect from the database when we're done
  session$onSessionEnded(function(){
    if(!is.null(isolate(och.con()))) {
      dbDisconnect(isolate(och.con()))
    }
  })
  
}

#### Run ####

# Run the application 
shinyApp(ui = ui, server = server)

