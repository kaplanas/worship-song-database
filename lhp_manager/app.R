library(shiny)
library(shinyjs)
library(RMySQL)
library(tidyverse)
library(date)
library(DT)
library(glue)
library(readxl)
library(aws.s3)
library(aws.ec2metadata)
library(rhandsontable)

#### Useful stuff ####

# Code in other files
source("tables.R", local = T)
source("returns.R", local = T)

# Table info
table.info = list(manage_hymnologists = manage.hymnologists.info)
populate.table.sql = map(
  table.info,
  function(table.info) {
    sql = paste(table.info$columns$column.name, collapse = ", ")
    sql = paste("SELECT", sql, "FROM", table.info$table)
    return(sql)
  }
)
update.table.sql = map(
  table.info,
  function(table.info) {
    sql = table.info$columns %>%
      filter(editable) %>%
      mutate(update = paste(column.name, " = {", column.name, "}",
                            sep = "")) %>%
      pull(update) %>%
      paste(collapse = ", ") %>%
      paste("UPDATE ", table.info$table, " SET ", ., " WHERE ", table.info$key,
            " = {", table.info$key, "}", sep = "")
    return(sql)
  }
)
insert.table.sql = map(
  table.info,
  function(table.info) {
    sql = paste(
      "INSERT INTO ", table.info$table, "(",
      table.info$columns %>%
        filter(editable) %>%
        pull(column.name) %>%
        paste(collapse = ", "),
      ") VALUES (",
      table.info$columns %>%
        filter(editable) %>%
        mutate(column.name = paste("{", column.name, "}", sep = "")) %>%
        pull(column.name) %>%
        paste(collapse = ", "),
      ")", sep = ""
    )
    return(sql)
  }
)
delete.table.sql = map(
  table.info,
  function(table.info) {
    sql = paste("DELETE FROM ", table.info$table, " WHERE ", table.info$key,
                " NOT IN ({keys*})", sep = "")
    return(sql)
  }
)
edit.buttons = list(manage_hymnologists = "hymnologists")

# Selector info
selector.info = list(return_file_hymnologist = return.file.hymnologist.info,
                     process_return_hymnologist = process.return.hymnologist.info)

# Utility table info
utility.table.info = list(song.labels = song.labels.info)

# Process table info
process.table.info = list(process_return = process.return.info)

#### UI ####

# Elements of the UI
page.title = "Lasting Hymns Project"
login.page = tabPanel("Log in",
                      textInput("lhp_username", "Username"),
                      passwordInput("lhp_password", "Password"),
                      actionButton("lhp_log_in", label = "Log in"))

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
  for(table.name in names(populate.table.sql)) {
    tables[[table.name]] = NULL
  }
  for(process.table.name in names(process.table.info)) {
    tables[[process.table.name]] = NULL
  }
  for(utility.table.name in names(utility.table.info)) {
    tables[[utility.table.name]] = NULL
  }
  
  # When the user attempts to log in, attempt to create a connection and
  # populate the tables
  observeEvent(input$lhp_log_in, {
    
    # Try to create a connection; if this fails, set the connection to null
    tryCatch(
      {
        lhp.con(dbConnect(MySQL(), user = input$lhp_username,
                          password = input$lhp_password,
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
      for(table.name in names(populate.table.sql)) {
        tryCatch(
          {
            tables[[table.name]] = dbGetQuery(lhp.con(),
                                              populate.table.sql[[table.name]])
          },
          error = function(err) {
            print(err)
            tables[[table.name]] = NULL
            output[[table.name]] = renderDT(NULL)
          },
          finally = {
            if(!is.null(tables[[table.name]])) {
              output[[table.name]] = renderDT(
                datatable(tables[[table.name]], rownames = F,
                          selection = list(mode = "single", target = "row"),
                          editable = list(target = "row",
                                          disable = list(columns =
                                                           which(!table.info[[table.name]]$columns$editable) - 1)),
                          options = list(paging = F, searching = F, info = F,
                                         order = table.info[[table.name]]$sort)) %>%
                  formatStyle(columns = which(!table.info[[table.name]]$columns$editable),
                              backgroundColor = "lightgray")
              )
            } else {
              output[[table.name]] = renderDT(NULL)
            }
          }
        )
      }
    } else {
      for(table.name in names(populate.table.sql)) {
        tables[[table.name]] = NULL
        output[[table.name]] = renderDT(NULL)
      }
    }
    
    # If connected, try to populate utility tables; if a query fails, set the
    # content of the table to null
    if(!is.null(lhp.con())) {
      for(utility.table.name in names(utility.table.info)) {
        tryCatch(
          {
            tables[[utility.table.name]] = dbGetQuery(lhp.con(),
                                                      utility.table.info[[utility.table.name]]$sql)
          },
          error = function(err) {
            print(err)
            tables[[utility.table.name]] = NULL
          }
        )
      }
    } else {
      for(utility.table.name in names(utility.table.info)) {
        tables[[utility.table.name]] = NULL
      }
    }
    
    # If connected, try to render selector lists
    if(!is.null(lhp.con())) {
      for(selector.name in names(selector.info)) {
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
    }
    
    # If connected, try to populate processing tables; if a query fails, set the
    # content of the table to null
    if(!is.null(lhp.con())) {
      for(process.table.name in names(process.table.info)) {
        tryCatch(
          {
            sql = glue_sql(process.table.info[[process.table.name]]$select_sql,
                           .con = lhp.con())
            tables[[process.table.name]] = dbGetQuery(lhp.con(), sql)
          },
          error = function(err) {
            print(err)
            tables[[process.table.name]] = NULL
            output[[process.table.name]] = renderRHandsontable(NULL)
          },
          finally = {
            if(!is.null(tables[[process.table.name]])) {
              output[[process.table.name]] = renderRHandsontable({
                rhandsontable(tables[[process.table.name]] %>%
                                mutate(Processed = Processed == 1) %>%
                                dplyr::select(process.table.info[[process.table.name]]$displayed.cols),
                              rowHeaders = NULL) %>%
                  hot_cols(colWidths = c(300, 300, 70)) %>%
                  hot_col(col = "Processed", type = "checkbox",
                          halign = "htCenter") %>%
                  hot_col(col = "Label", type = "dropdown", strict = T,
                          renderer = "html", source = tables$song_labels$Label) %>%
                  hot_col(col = "Label",
                          renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
                  hot_col(col = which(!(process.table.info[[process.table.name]]$displayed.cols
                                        %in% process.table.info[[process.table.name]]$editable.cols)),
                          readOnly = T)
              })
            } else {
              output[[process.table.name]] = renderDT(NULL)
            }
          }
        )
      }
    } else {
      for(process.table.name in names(process.table.info)) {
        tables[[process.table.name]] = NULL
        output[[process.table.name]] = renderRHandsontable(NULL)
      }
    }

  })
  
  # When the user clicks an "Add" button, add a row to the table
  for(table.name in names(edit.buttons)) {
    observeEvent(input[[paste("add", edit.buttons[[table.name]], sep = "_")]], {
      tables[[table.name]][nrow(tables[[table.name]]) + 1,] = NA
    })
  }
  
  # When the user clicks the "Delete" button, delete the selected row from the
  # table
  for(table.name in names(edit.buttons)) {
    observeEvent(input[[paste("delete", edit.buttons[[table.name]],
                              sep = "_")]], {
      tables[[table.name]] = tables[[table.name]][-input[[paste(table.name,
                                                                "rows_selected",
                                                                sep = "_")]],]
    })
  }
  
  # When the user edits a cell, store the edits in the table (but not edits to
  # uneditable columns; they should be set to uneditable anyway, but the
  # redundancy here is nice)
  for(table.name in names(edit.buttons)) {
    input.name = paste(table.name, "cell_edit", sep = "_")
    observeEvent(input[[input.name]], {
      r = input[[input.name]]$row
      c = input[[input.name]]$col
      tables[[table.name]][r[1],c[which(table.info[[table.name]]$columns$editable) + 1]] =
        na_if(input[[input.name]]$value[table.info[[table.name]]$columns$editable],
              "")
    })
  }
  
  # When the user clicks the "Save" button, attempt to write the table to the
  # database
  for(table.name in names(edit.buttons)) {
    observeEvent(input[[paste("save", edit.buttons[[table.name]],
                              sep = "_")]], {

      # Keep track of whether all updates were successful
      successful.updates = T

      # Create sql to update changed rows
      sql = tables[[table.name]] %>%
        filter(!is.na(.data[[table.info[[table.name]]$key]])) %>%
        glue_data_sql(update.table.sql[[table.name]], .con = lhp.con())
      
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
      if(any(is.na(tables[[table.name]][[table.info[[table.name]]$key]]))) {
        sql = tables[[table.name]] %>%
          filter(is.na(.data[[table.info[[table.name]]$key]])) %>%
          glue_data_sql(insert.table.sql[[table.name]], .con = lhp.con())
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
      sql = glue_sql(delete.table.sql[[table.name]],
                     keys = tables[[table.name]][[table.info[[table.name]]$key]],
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
          tables[[table.name]] = dbGetQuery(lhp.con(),
                                            populate.table.sql[[table.name]])
        },
        error = function(err) {
          print(err)
          tables[[table.name]] = NULL
        }
      )
      
    })
  }
  
  # When the user uploads a file, write its contents as a csv to the S3 bucket
  # and add the filename to the table of hymnologists
  observeEvent(input$upload_return_file, {
    
    # Read in the data
    return.file.df = read_excel(input$return_file$datapath, col_names = "Song")
    
    # Determine the filename
    filename = tables$manage_hymnologists %>%
      filter(HymnologistID == input$return_file_hymnologist) %>%
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
                       hymnologist.id = input$return_file_hymnologist,
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
              tables$manage_hymnologists = dbGetQuery(lhp.con(),
                                                      populate.table.sql$manage_hymnologists)
            },
            error = function(err) {
              print(err)
              tables$manage_hymnologists = NULL
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
  
  # When the user selects a hymnologist to process a return for, update the
  # table with return results
  observeEvent(input$process_return_hymnologist, {
    if(!is.null(lhp.con())) {
      sql = glue_sql(process.table.info$process_return$select_sql, .con = lhp.con())
      tables$process_return = dbGetQuery(lhp.con(), sql)
    }
  })
  
  # When the user makes a change to the return processing table, update the
  # underlying table accordingly
  observeEvent(input$process_return, {
    for(change in input$process_return$changes) {
      if(is.list(change)) {
        r = change[[1]][[1]] + 1
        c = process.table.info$process_return$displayed.cols[change[[1]][[2]] + 1]
        tables$process_return[r,c] = change[[1]][[4]]
      }
    }
  })
  
  # When the user clicks the "Save return" button, attempt to save the changes
  # made by the user to the database
  observeEvent(input$save_return, {
    
    # Keep track of whether all updates were successful
    successful.updates = T
    
    # Create sql to update changed rows
    sql = tables$process_return %>%
      left_join(tables$song_labels, by = "Label") %>%
      dplyr::select(HymnologistReturnID, SongID, Processed) %>%
      glue_data_sql(process.table.info$process_return$update_sql,
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
        tables$process_return = sql = glue_sql(process.table.info$process_return$select_sql,
                                               .con = lhp.con())
        tables$process_return = dbGetQuery(lhp.con(), sql)
      },
      error = function(err) {
        print(err)
        tables$process_return = NULL
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

