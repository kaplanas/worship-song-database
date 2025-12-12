library(shiny)
library(shinyjs)
library(RMySQL)
library(tidyverse)
library(date)
library(glue)
library(aws.ec2metadata)
library(rhandsontable)
library(DT)
library(aws.s3)
library(readr)
library(paws)


#### Useful stuff ####

# Render html in selectize
selectize.html.render = '{item: function(item, escape) {
                                  return "<div>" + item.label + "</div>";
                                },
                          option: function(item, escape) {
                                    return "<div>" + item.label + "</div>";
                                  }
                          }'

# Code in other files
source("label_tables.R", local = T)
source("selectors.R", local = T)
source("tables.R", local = T)
source("alternative_tunes.R", local = T)
source("songbooks.R", local = T)
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
page.title = "Worship Song Database"
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
                            ')),
                      textInput("wsdb.username", "Username"),
                      passwordInput("wsdb.password", "Password"),
                      actionButton("wsdb.log.in", label = "Log in"))

# Define UI
ui <- navbarPage(
  page.title,
  login.page,
  tables.page,
  alternative.tunes.page,
  songbooks.page,
  summary.page,
  useShinyjs()
)

#### Server ####

# Define server logic
server <- function(input, output, session) {
  
  # AWS profile
  Sys.setenv(AWS_PROFILE = "wsdb_manager_test")
  
  # Database connection
  wsdb.con = reactiveVal(NULL)
  dynamo.db = reactiveVal(NULL)
  
  # Reactive label tables
  label.tables = do.call(
    "reactiveValues",
    lapply(label.table.sql, function(label.table) { NULL })
  )
  label.refresh = do.call(
    "reactiveValues",
    lapply(label.table.sql, function(label.table) { T })
  )
  
  # Track when to update selector lists
  selector.refresh = do.call(
    "reactiveValues",
    lapply(selector.sql, function(selector) { T })
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
  
  # Reactive form tables
  form.tables = do.call(
    "reactiveValues",
    lapply(form.table.info, function(form.table) { NULL })
  )
  form.refresh = do.call(
    "reactiveValues",
    lapply(form.table.info, function(form.table) { T })
  )
  form.row = do.call(
    "reactiveValues",
    lapply(set_names(names(form.table.info)), function(form.table) {
      data.frame(matrix(ncol = nrow(form.table.info[[form.table]]$columns),
                        nrow = 0,
                        dimnames = list(NULL,
                                        form.table.info[[form.table]]$columns$column.name)))
    })
  )
  
  # Reactive alternative tune tables
  alt.tables = do.call(
    "reactiveValues",
    lapply(alt.table.info, function(alt.table) { NULL })
  )
  alt.refresh = do.call(
    "reactiveValues",
    lapply(alt.table.info, function(alt.table) { T })
  )
  alt.changes = do.call(
    "reactiveValues",
    lapply(
      alt.table.info,
      function(alt.table) {
        list(edit = c(), insert = F, delete = F)
      }
    )
  )
  
  # Reactive processing table
  songbook.processing = reactiveValues(
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
  observeEvent(input$wsdb.log.in, {
    
    # Try to create a connection; if this fails, set the connection to null
    tryCatch(
      {
        wsdb.con(dbConnect(MySQL(), user = input$wsdb.username,
                           password = input$wsdb.password,
                           host = "worship-song-db.cluster-cugagm0whbfr.us-west-2.rds.amazonaws.com",
                           port = 3306))
        dbGetQuery(wsdb.con(), "SET NAMES utf8")
        showNotification("Login successful", type = "message")
      },
      error = function(err) {
        print(err)
        if(!is.null(wsdb.con())) {
          dbDisconnect(wsdb.con())
        }
        wsdb.con(NULL)
        showNotification("Login failed", type = "error")
        return()
      }
    )
    
    # Connection to DynamoDB
    dynamo.db(dynamodb())
    
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
        
        # Determine which form tables are related to this label table
        related.form.tables = c()
        for(ft in names(form.table.info)) {
          if(lt %in% form.table.info[[ft]]$columns$key.table) {
            related.form.tables = c(related.form.tables, ft)
          }
        }
        
        # Populate the label table (and refresh as needed)
        observeEvent(label.refresh[[lt]], {
          label.tables[[lt]] = populate.label.table(lt, wsdb.con(),
                                                    reference.tables)
          label.refresh[[lt]] = NULL
          for(rt in related.reference.tables) {
            reference.refresh[[rt]] = T
          }
          for(ft in related.form.tables) {
            form.refresh[[ft]] = T
          }
        })
        
      }
    )
    
    # Selector lists
    purrr::walk(
      names(selector.sql),
      function(selector.name) {
        observeEvent(selector.refresh[[selector.name]], {
          update.selector(selector.name, wsdb.con(), session,
                          selector.refresh[[selector.name]])
          selector.refresh[[selector.name]] = NULL
        })
      }
    )
    
    # Reference tables
    purrr::walk(
      names(reference.table.info),
      function(rt) {
        
        # Populate tables (and refresh as needed)
        observeEvent(reference.refresh[[rt]], {
          reference.tables[[rt]] = populate.reference.table(rt, wsdb.con(),
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
          save.reference.table(rt, wsdb.con(), dynamo.db(), reference.tables,
                               label.tables, reference.changes)
          reference.refresh[[rt]] = T
          for(lt in reference.table.info[[rt]]$related.label.tables) {
            label.refresh[[lt]] = T
          }
          for(s in reference.table.info[[rt]]$related.selectors) {
            selector.refresh[[s]] = T
          }
          if(reference.table.info[[rt]]$related.processing.table) {
            songbook.processing$refresh = T
          }
        })
        
      }
    )
    
    # Form tables
    purrr::walk(
      names(form.table.info),
      function(ft) {
        
        # Populate tables (and refresh as needed)
        observeEvent(form.refresh[[ft]], {
          form.tables[[ft]] = populate.form.table(ft, wsdb.con(), label.tables)
          form.refresh[[ft]] = NULL
        })
        
        # Populate management pages (and refresh as needed)
        observeEvent(
          {
            input[[paste("manage", ft, "id", sep = ".")]]
            form.tables[[ft]]
          },
          {
            form.row[[ft]] = get.form.row(ft, form.tables,
                                          input[[paste("manage", ft, "id",
                                                       sep = ".")]])
            populate.form.page(ft, form.row[[ft]], session)
          }
        )
        
        # Render selector lists and uneditable columns
        for(i in 2:nrow(form.table.info[[ft]]$columns)) {
          local({
            col.info = form.table.info[[ft]]$columns[i,]
            if(!is.na(col.info$key.table)) {
              observeEvent(label.tables[[col.info$key.table]], {
                selector.list = list()
                if(!is.null(label.tables[[col.info$key.table]])) {
                  selector.list = label.tables[[col.info$key.table]] %>%
                    column_to_rownames(col.info$key.label) %>%
                    t() %>%
                    as.data.frame() %>%
                    as.list()
                }
                current.selection = input[[col.info$element.id]]
                updateSelectizeInput(session, col.info$element.id,
                                     choices = selector.list,
                                     selected = current.selection, server = T)
              })
            } else if(!col.info$editable) {
              if(col.info$type == "html") {
                output[[col.info$element.id]] = renderUI({
                  tags$div(tags$b(col.info$form.label), tags$br(),
                           HTML(form.row[[ft]][[col.info$column.name]]))
                })
              } else {
                output[[col.info$element.id]] = renderUI({
                  tags$div(tags$b(col.info$form.label), tags$br(),
                           form.row[[ft]][[col.info$column.name]])
                })
              }
            }
          })
        }
        
        # When the user clicks the "Save" button, attempt to write the table to
        # the database and update related tables/selectors
        observeEvent(input[[paste("save", ft, sep = ".")]], {
          row.elements = lapply(
            set_names(form.table.info[[ft]]$columns$element.id,
                      form.table.info[[ft]]$columns$column.name),
            function(eid) { input[[eid]] }
          )
          if(ft == "lyrics") {
            row.elements$FileName = NA
            if(!is.null(input$lyrics.new.file)) {
              row.elements$FileName = input$lyrics.new.file$name
              row.elements$FileContents = read_file(input$lyrics.new.file$datapath)
            }
          }
          save.form.table(ft, row.elements,
                          input[[paste("manage", ft, "id", sep = ".")]],
                          wsdb.con(), label.tables, session)
          form.refresh[[ft]] = T
          for(lt in form.table.info[[ft]]$related.label.tables) {
            label.refresh[[lt]] = T
          }
          for(s in form.table.info[[ft]]$related.selectors) {
            new.id = input[[paste("manage", ft, "id", sep = ".")]]
            if(new.id == -1) {
              new.id = dbGetQuery(wsdb.con(),
                                  "SELECT LAST_INSERT_ID() AS NEW_ID")$NEW_ID[1]
            }
            selector.refresh[[s]] = new.id
          }
          if(form.table.info[[ft]]$related.processing.table) {
            songbook.processing$refresh = T
          }
          if(ft == "lyrics") {
            reset(id = "lyrics.new.file")
          }
        })
        
        # When the user clicks the "Delete" button, attempt to delete the row
        # and update related tables/selectors
        observeEvent(input[[paste("delete", ft, sep = ".")]], {
          delete.form.table(ft, input[[paste("manage", ft, "id", sep = ".")]],
                            wsdb.con())
          form.refresh[[ft]] = T
          for(lt in form.table.info[[ft]]$related.label.tables) {
            label.refresh[[lt]] = T
          }
          for(s in form.table.info[[ft]]$related.selectors) {
            selector.refresh[[s]] = T
          }
          if(form.table.info[[ft]]$related.processing.table) {
            songbook.processing$refresh = T
          }
        })
        
      }
    )
    
    # Alternative tune tables
    purrr::walk(
      names(alt.table.info),
      function(at) {
        
        # Populate tables (and refresh as needed)
        observeEvent(
          {
            alt.refresh[[at]]
            input$alt.by.song.id
            input$alt.by.metrical.psalm.id
            input$alt.by.tune.id
          },
          {
            alt.tables[[at]] = populate.alternative.tunes.table(at, wsdb.con(),
                                                                input$alt.by.song.id,
                                                                input$alt.by.metrical.psalm.id,
                                                                input$alt.by.tune.id)
            alt.refresh[[at]] = NULL
          }
        )
        
        # Render as handsontable
        output[[at]] = renderRHandsontable({
          create.alternative.tunes.hot(alt.tables[[at]], at, label.tables,
                                       input$dimension[1], input$dimension[2])
        })
        
        # When the user makes an update through the UI, propagate the changes to
        # the table
        observeEvent(input[[at]]$change, {
          update.alternative.tunes.hot(at, input[[at]]$change, alt.tables,
                                       alt.changes)
        })
        
        # When the user request suggestions, retrieve them and add them to the
        # table
        observeEvent(input[[paste("suggest", at, sep = ".")]], {
          alt.tables[[at]] = bind_rows(
            alt.tables[[at]],
            suggest.alternative.tunes(at, wsdb.con(), input$alt.by.song.id,
                                      input$alt.by.metrical.psalm.id,
                                      input$alt.by.tune.id)
          )
          alt.changes[[at]]$insert = T
        })
        
        # When the user clicks the "Save" button, attempt to write the table to
        # the database and update related tables/selectors
        observeEvent(input[[paste("save", at, sep = ".")]], {
          need.to.refresh = F
          if(alt.changes[[at]]$insert) {
            need.to.refresh = T
          }
          save.alternative.tunes.table(at, wsdb.con(), alt.tables, label.tables,
                                       alt.changes, input$alt.by.song.id,
                                       input$alt.by.metrical.psalm.id,
                                       input$alt.by.tune.id)
          if(need.to.refresh) {
            alt.refresh[[at]] = T
          }
        })
        
      }
    )
    
    # Populate processing table (and refresh as needed)
    observeEvent(
      {
        songbook.processing$refresh
        input$process.songbook.id
        input$process.songbook.volume.id
      },
      {
        songbook.processing$table = populate.processing.table(wsdb.con(),
                                                              input$process.songbook.id,
                                                              input$process.songbook.volume.id)
        songbook.processing$refresh = NULL
      }
    )
    output$process.songbook = renderRHandsontable({
      create.songbook.hot(songbook.processing$table, label.tables,
                          input$dimension[1], input$dimension[2])
    })

    # When the user makes a change to the songbook processing table, update the
    # underlying table accordingly
    observeEvent(input$process.songbook, {
      update.songbook.hot(input$process.songbook$changes, songbook.processing)
    })

    # When the user clicks the "Save songbook" button, attempt to save the
    # changes made by the user to the database (and refresh related tables)
    observeEvent(input$save.songbook, {
      save.songbook.table(songbook.processing, input$process.songbook.id,
                          input$process.songbook.volume.id, label.tables,
                          wsdb.con())
      songbook.processing$refresh = T
      reference.refresh$songbook.entries = T
      summary.refresh$songbook.counts = T
    })

    # Summary tables
    purrr::walk(
      names(summary.table.sql),
      function(st) {
        observeEvent(summary.refresh[[st]], {
          summary.tables[[st]] = populate.summary.table(st, wsdb.con())
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
    if(!is.null(isolate(wsdb.con()))) {
      dbDisconnect(isolate(wsdb.con()))
    }
  })
  
}

#### Run ####

# Run the application 
shinyApp(ui = ui, server = server)

