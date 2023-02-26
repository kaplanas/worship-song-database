library(shiny)
library(shinyjs)
library(RMySQL)
library(tidyverse)
library(date)
library(glue)
library(readxl)
library(aws.s3)
library(aws.ec2metadata)
library(rhandsontable)

#### Useful stuff ####

# Code in other files
source("tables.R", local = T)
source("returns.R", local = T)
source("utilities.R", local = T)

# Management tables
manage.table.info = list(hymnologists = manage.hymnologists.info,
                         hymnologist.returns = manage.hymnologist.returns.info)
for(manage.table in names(manage.table.info)) {
  manage.info = manage.table.info[[manage.table]]
  sql = paste(manage.info$columns$column.name, collapse = ", ")
  sql = paste("SELECT ", sql, " FROM ", manage.info$table, " ORDER BY ",
              paste(manage.info$sort, collapse = ", "), sep = "")
  manage.table.info[[manage.table]]$populate.sql = sql
  sql = manage.info$columns %>%
    filter(editable) %>%
    mutate(update = paste(column.name, " = {", column.name, "}",
                          sep = "")) %>%
    pull(update) %>%
    paste(collapse = ", ") %>%
    paste("UPDATE ", manage.info$table, " SET ", ., " WHERE ", manage.info$key,
          " = {", manage.info$key, "}", sep = "")
  manage.table.info[[manage.table]]$update.sql = sql
  sql = paste(
    "INSERT INTO ", manage.info$table, "(",
    manage.info$columns %>%
      filter(editable) %>%
      pull(column.name) %>%
      paste(collapse = ", "),
    ") VALUES (",
    manage.info$columns %>%
      filter(editable) %>%
      mutate(column.name = paste("{", column.name, "}", sep = "")) %>%
      pull(column.name) %>%
      paste(collapse = ", "),
    ")", sep = ""
  )
  manage.table.info[[manage.table]]$insert.sql = sql
  sql = paste("DELETE FROM ", manage.info$table, " WHERE ", manage.info$key,
              " NOT IN ({keys*})", sep = "")
  manage.table.info[[manage.table]]$delete.sql = sql
}

# Selector info
selector.info = list(return.file.hymnologist = return.file.hymnologist.info,
                     process.return.hymnologist = process.return.hymnologist.info)

# Utility table info
utility.table.info = list(song.labels = song.labels.info)

#### UI ####

# Elements of the UI
page.title = "Lasting Hymns Project"
login.page = tabPanel("Log in",
                      textInput("lhp.username", "Username"),
                      passwordInput("lhp.password", "Password"),
                      actionButton("lhp.log.in", label = "Log in"))

# Define UI
ui <- navbarPage(
  page.title,
  login.page,
  tables.page,
  returns.page
)

#### Server ####

# Define server logic
server <- function(input, output, session) {
  
  # Database connection
  lhp.con = reactiveVal(NULL)
  
  # Keep track of reactive stuff
  tables = reactiveValues()
  for(manage.table in names(manage.table.info)) {
    tables[[manage.table]] = NULL
  }
  for(utility.table in names(utility.table.info)) {
    tables[[utility.table]] = NULL
  }
  tables$process.return = NULL
  
  # When the user attempts to log in, attempt to create a connection and
  # populate the tables
  observeEvent(input$lhp.log.in, {
    
    # Try to create a connection; if this fails, set the connection to null
    tryCatch(
      {
        lhp.con(dbConnect(MySQL(), user = input$lhp.username,
                          password = input$lhp.password,
                          host = "localhost", port = 3306))
        dbGetQuery(lhp.con(), "SET NAMES utf8")
        showNotification("Login successful", type = "message")
      },
      error = function(err) {
        print(err)
        if(!is.null(lhp.con())) {
          dbDisconnect(lhp.con())
        }
        lhp.con(NULL)
        showNotification("Login failed", type = "error")
        return()
      }
    )
    
    # If connected, try to populate management tables; if a query fails, set the
    # content of the table to null
    if(!is.null(lhp.con())) {
      purrr::walk(
        names(manage.table.info),
        function(manage.table) {
          manage.info = manage.table.info[[manage.table]]
          tryCatch(
            {
              tables[[manage.table]] = dbGetQuery(lhp.con(),
                                                  manage.info$populate.sql)
            },
            error = function(err) {
              print(err)
              tables[[manage.table]] = NULL
              output[[manage.table]] = renderRHandsontable(NULL)
            },
            finally = {
              if(!is.null(tables[[manage.table]])) {
                output[[manage.table]] = renderRHandsontable({
                  temp.df = tables[[manage.table]]
                  rhandsontable(temp.df, rowHeaders = F) %>%
                    hot_cols(columnSorting = F) %>%
                    hot_col(col = which(!manage.info$columns$editable),
                            readOnly = T)
                })
              } else {
                output[[manage.table]] = renderRHandsontable(NULL)
              }
            }
          )
        }
      )
    } else {
      purrr::walk(
        names(manage.table.info),
        function(manage.table) {
          tables[[manage.table]] = NULL
          output[[manage.table]] = renderRHandsontable(NULL)
        }
      )
    }
    
    # When the user makes a change to a management table, update the underlying
    # table accordingly
    purrr::walk(
      names(manage.table.info),
      function(manage.table) {
        manage.info = manage.table.info[[manage.table]]
        observeEvent(input[[manage.table]], {
          change = input[[manage.table]]$changes
          if(change$event == "afterChange") {
            if(!is.null(change$changes)) {
              r = change$changes[[1]][[1]] + 1
              c = change$changes[[1]][[2]] + 1
              tables[[manage.table]][r,c] = change$changes[[1]][[4]]
            }
          } else if(change$event == "afterCreateRow") {
            tables[[manage.table]][nrow(tables[[manage.table]]) + 1,] = NA
          } else if(change$event == "afterRemoveRow") {
            r = change$ind + 1
            tables[[manage.table]] = tables[[manage.table]][-r,]
          }
        })
      }
    )
    
    # When the user clicks the "Save" button, attempt to write the table to the
    # database
    purrr::walk(
      names(manage.table.info),
      function(manage.table) {
        observeEvent(input[[paste("save", manage.table, sep = ".")]], {
          
          # Info about this table
          manage.info = manage.table.info[[manage.table]]
          
          # Keep track of whether all updates were successful
          successful.updates = T
          
          # Create sql to update changed rows
          sql = tables[[manage.table]] %>%
            filter(!is.na(.data[[manage.info$key]])) %>%
            glue_data_sql(manage.info$update.sql, .con = lhp.con())
          
          # Attempt to update changed rows
          for(s in sql) {
            tryCatch(
              {
                dbGetQuery(lhp.con(), s)
              },
              error = function(err) {
                print(err)
                successful.updates = F
              }
            )
          }
          
          # Attempt to insert new rows
          if(any(is.na(tables[[manage.table]][[manage.info$key]]))) {
            sql = tables[[manage.table]] %>%
              filter(is.na(.data[[manage.info$key]])) %>%
              glue_data_sql(manage.info$insert.sql, .con = lhp.con())
            for(s in sql) {
              tryCatch(
                {
                  dbGetQuery(lhp.con(), s)
                },
                error = function(err) {
                  print(err)
                  successful.updates = F
                }
              )
            }
          }
          
          # Create sql to delete rows
          sql = glue_sql(manage.info$delete.sql,
                         keys = tables[[manage.table]][[manage.info$key]],
                         .con = lhp.con())
          
          # Attempt to delete rows
          tryCatch(
            {
              dbGetQuery(lhp.con(), sql)
            },
            error = function(err) {
              print(err)
              successful.updates = F
            }
          )
          
          # If anything went wrong, display a notice
          if(!successful.updates) {
            showNotification("Some changes may not have been saved", type = "error")
          }
          
          # Update table to reflect database
          tryCatch(
            {
              tables[[manage.table]] = dbGetQuery(lhp.con(),
                                                  manage.info$populate.sql)
            },
            error = function(err) {
              print(err)
              tables[[manage.table]] = NULL
            }
          )
          
        })
      }
    )
    
    # If connected, try to populate utility tables; if a query fails, set the
    # content of the table to null
    if(!is.null(lhp.con())) {
      purrr::walk(
        names(utility.table.info),
        function(utility.table) {
          tryCatch(
            {
              tables[[utility.table]] = dbGetQuery(lhp.con(),
                                                   utility.table.info[[utility.table]]$sql)
            },
            error = function(err) {
              print(err)
              tables[[utility.table]] = NULL
            }
          )
        }
      )
    } else {
      purrr::walk(
        names(utility.table.info),
        function(utility.table) {
          tables[[utility.table]] = NULL
        }
      )
    }
    
    # If connected, try to render selector lists
    if(!is.null(lhp.con())) {
      purrr::walk(
        names(selector.info),
        function(selector.name) {
          selector.list = list()
          tryCatch(
            {
              selector.list = dbGetQuery(lhp.con(),
                                         selector.info[[selector.name]]$sql) %>%
                column_to_rownames("SelectorDisplay") %>%
                t() %>%
                as.data.frame() %>%
                as.list()
            },
            error = function(err) {
              print(err)
              selector.list = list()
            },
            finally = {
              updateSelectInput(session, selector.name, choices = selector.list)
            }
          )
        }
      )
    }
    
    # If connected, try to populate processing table; if a query fails, set the
    # content of the table to null
    if(!is.null(lhp.con())) {
      tryCatch(
        {
          sql = glue_sql(process.return.info$select.sql,
                         .con = lhp.con())
          tables$process.return = dbGetQuery(lhp.con(), sql)
        },
        error = function(err) {
          print(err)
          tables$process.return = NULL
          output$process.return = renderRHandsontable(NULL)
        },
        finally = {
          if(!is.null(tables$process.return)) {
            output$process.return = renderRHandsontable(
              rhandsontable(tables$process.return[,process.return.info$columns$displayed] %>%
                              mutate(Processed = Processed == 1),
                            rowHeaders = NULL) %>%
                hot_context_menu(allowRowEdit = F, allowColEdit = F) %>%
                hot_cols(colWidths = process.return.info$columns$width[process.return.info$columns$displayed],
                         columnSorting = F) %>%
                hot_col(col = "Processed", type = "checkbox",
                        halign = "htCenter") %>%
                hot_col(col = "Label", type = "dropdown", strict = T,
                        renderer = "html", source = tables$song.labels$Label) %>%
                hot_col(col = "Label",
                        renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
                hot_col(col = which(!process.return.info$columns$editable[process.return.info$columns$displayed]),
                        readOnly = T)
            )
          } else {
            output$process.return = renderRHandsontable(NULL)
          }
        }
      )
    } else {
      tables$process.return = NULL
      output$process.return = renderRHandsontable(NULL)
    }

  })
  
  # When the user makes a change to the return processing table, update the
  # underlying table accordingly
  observeEvent(input$process.return, {
    change = input$process.return$changes
    if(change$event == "afterChange") {
      if(!is.null(change$changes)) {
        r = change$changes[[1]][[1]] + 1
        c = which(process.return.info$columns$displayed)[change$changes[[1]][[2]] + 1]
        tables$process.return[r,c] = change$changes[[1]][[4]]
      }
    }
  })
  
  # When the user clicks the "Save return" button, attempt to save the changes
  # made by the user to the database
  observeEvent(input$save.return, {
    
    # Keep track of whether all updates were successful
    successful.updates = T
    
    # Create sql to update changed rows
    sql = tables$process.return %>%
      left_join(tables$song.labels, by = "Label") %>%
      dplyr::select(HymnologistReturnID, SongID, Processed) %>%
      glue_data_sql(process.return.info$update.sql,
                    .con = lhp.con())
    
    # Attempt to update changed rows
    for(s in sql) {
      tryCatch(
        {
          dbGetQuery(lhp.con(), s)
        },
        error = function(err) {
          print(err)
          successful.updates = F
        }
      )
    }
    
    # If anything went wrong, display a notice
    if(!successful.updates) {
      showNotification("Some changes may not have been saved", type = "error")
    }
    
    # Update table to reflect database
    tryCatch(
      {
        tables$process.return = sql = glue_sql(process.return.info$select.sql,
                                               .con = lhp.con())
        tables$process.return = dbGetQuery(lhp.con(), sql)
      },
      error = function(err) {
        print(err)
        tables$process.return = NULL
      }
    )
    
  })
  
  # When the user selects a hymnologist to process a return for, update the
  # table with return results
  observeEvent(input$process.return.hymnologist, {
    if(!is.null(lhp.con())) {
      sql = glue_sql(process.return.info$select.sql, .con = lhp.con())
      tables$process.return = dbGetQuery(lhp.con(), sql)
    }
  })
  
  # When the user uploads a file, write its contents as a csv to the S3 bucket
  # and add the filename to the table of hymnologists
  observeEvent(input$upload.return.file, {
    
    # Read in the data
    return.file.df = read_excel(input$return.file$datapath, col_names = "Song")
    
    # Determine the filename
    filename = tables$manage.hymnologists %>%
      filter(HymnologistID == input$return.file.hymnologist) %>%
      mutate(filename = paste(HymnologistID,
                              tolower(gsub("[^A-Za-z]", "", FirstName)),
                              tolower(gsub("[^A-Za-z]", "", LastName)),
                              sep = "_"),
             filename = paste(filename, "csv", sep = ".")) %>%
      pull(filename)
    
    # Write the data to a csv in the S3 bucket
    file.saved = F
    tryCatch(
      {
        sql = glue_sql("UPDATE lhp.hymnologists
                        SET FileName = {file.name}
                        WHERE HymnologistID = {hymnologist.id}",
                       file.name = filename,
                       hymnologist.id = input$return.file.hymnologist,
                       .con = lhp.con())
        dbGetQuery(lhp.con(), sql)
        filename.written = T
        
      },
      error = function(err) {
        print(err)
        showNotification("Unable to update filename", type = "error")
      },
      finally = {
        if(filename.written) {
          tryCatch(
            {
              tables$hymnologists = dbGetQuery(lhp.con(),
                                               manage.table.info$hymnologists$populate.sql)
            },
            error = function(err) {
              print(err)
              tables$hymnologists = NULL
            }
          )
          tryCatch(
            {
              s3write_using(return.file.df, FUN = write.csv,
                            object = paste("lasting_hymns_project", filename,
                                           sep = "/"),
                            bucket = "worship-song-database", row.names = F)
              showNotification("File successfully saved", type = "message")
            },
            error = function(err) {
              print(err)
              showNotification("Unable to save file", type = "error")
            }
          )
        }
      }
    )
    
  })

  # Disconnect from the database when we're done
  session$onSessionEnded(function(){
    if(!is.null(isolate(lhp.con()))) {
      dbDisconnect(isolate(lhp.con()))
    }
  })
  
}

#### Run ####

# Run the application 
shinyApp(ui = ui, server = server)

