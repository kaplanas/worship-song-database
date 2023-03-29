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
    lapply(summary.table.sql, function(summary.table) { NULL })
  )
  summary.refresh = do.call(
    "reactiveValues",
    lapply(summary.table.sql, function(summary.table) { T })
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
                           selected = current.selected)
          } else if(sel.info$type == "date") {
            min.date = NULL
            max.date = NULL
            if(length(selector.list[[sn]]) > 0) {
              min.date = min(selector.list[[sn]])
              max.date = max(selector.list[[sn]])
            }
            airDatepickerInput(sn, sel.info$label, value = min.date,
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
      
      # Determine the filename
      filename = reference.tables$congregations %>%
        filter(CongregationID == input$wh.file.congregation.id) %>%
        pull(FolderName)
      filename = paste(filename, "/", filename, "_",
                       strftime(Sys.time(), "%Y%m%d%H%M%S"), sep = "")
      
      # Spreadsheets
      if(input$wh.file.type == "Spreadsheet") {
        
        # Read in as a dataframe
        wh.file.df = read_excel(input$wh.file$datapath,
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
          wh.file.df = wh.file.df %>%
            filter(!(as.Date(WorshipDate) %in% existing.dates))
        }
        
        # Add the correct suffix to the filename
        filename = paste(filename, ".csv", sep = "")
        
      }
      
      # Write the data to a csv in the S3 bucket
      file.saved = F
      tryCatch(
        {
          s3write_using(wh.file.df, FUN = write.csv,
                        object = paste("observing_congregational_hymnody",
                                       filename, sep = "/"),
                        bucket = "worship-song-database", row.names = F)
          showNotification("File successfully saved", type = "message")
        },
        error = function(err) {
          print(err)
          showNotification("Unable to save file", type = "error")
        }
      )
      
    })
    
    # When the user requests a refresh of the processing selectors, do that
    observeEvent(input$refresh.process.wh, {
      for(sn in c("process.wh.congregation.id", "process.wh.date")) {
        selector.refresh[[sn]] = T
      }
      worship.history.processing$refresh = T
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
      for(sn in c("process.wh.congregation.id", "process.wh.date")) {
        selector.refresh[[sn]] = T
      }
      worship.history.processing$refresh = T
      summary.refresh$songbook.counts = T
    })

    # Summary tables
    purrr::walk(
      names(summary.table.sql),
      function(st) {
        observeEvent(summary.refresh[[st]], {
          summary.tables[[st]] = populate.summary.table(st, och.con())
          summary.refresh[[st]] = NULL
        })
        output[[st]] = renderDT({
          summary.tables[[st]] %>%
            datatable(options = list(searching = F, paging = F, info = F),
                      rownames = F)
        })
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

