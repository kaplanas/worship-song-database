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
library(DT)

#### Useful stuff ####

# Code in other files
source("tables.R", local = T)
source("returns.R", local = T)
source("summary.R", local = T)
source("utilities.R", local = T)

# ggplot theme
theme_set(theme_bw())

# Utility table info
utility.table.info = list(hymnologist.labels = hymnologist.labels.info,
                          song.labels = song.labels.info)

# Selector info
selector.info = list(return.file.hymnologist = return.file.hymnologist.info,
                     process.return.hymnologist = process.return.hymnologist.info)

# Management tables
manage.table.info = list(hymnologists = manage.hymnologists.info,
                         hymnologist.returns = manage.hymnologist.returns.info)
for(manage.table in names(manage.table.info)) {
  manage.info = manage.table.info[[manage.table]]
  sql.cols = paste(manage.info$columns$column.name, collapse = ", ")
  sql = paste("SELECT ", sql.cols, " FROM ", manage.info$table, " ORDER BY ",
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

#### UI ####

# Elements of the UI
page.title = "Lasting Hymns Project"
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
                      textInput("lhp.username", "Username"),
                      passwordInput("lhp.password", "Password"),
                      actionButton("lhp.log.in", label = "Log in"))

# Define UI
ui <- navbarPage(
  page.title,
  login.page,
  tables.page,
  returns.page,
  summary.page
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
  tables$lhp.summary = NULL
  
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
    
    # If connected, try to populate utility tables; if a query fails, set the
    # content of the table to null
    purrr::walk(
      names(utility.table.info),
      function(utility.table) {
        populate.utility.table(utility.table, lhp.con(), tables)
      }
    )
    
    # If connected, try to render selector lists
    if(!is.null(lhp.con())) {
      purrr::walk(
        names(selector.info),
        function(selector.name) {
          update.selector(selector.name, lhp.con(), session)
        }
      )
    }
    
    # If connected, try to populate management tables; if a query fails, set the
    # content of the table to null
      purrr::walk(
        names(manage.table.info),
        function(manage.table) {
          populate.management.table(manage.table, lhp.con(), tables)
          manage.info = manage.table.info[[manage.table]]
          output[[manage.table]] = renderRHandsontable({
            temp.df = tables[[manage.table]]
            if(is.null(temp.df)) {
              rhandsontable(NULL)
            } else {
              temp.table = rhandsontable(temp.df, rowHeaders = F,
                                         overflow = "visible",
                                         width = input$dimension[1] * 0.8,
                                         height = input$dimension[2] * 0.8) %>%
                hot_context_menu(allowColEdit = F) %>%
                hot_cols(colWidths = manage.info$columns$width,
                         fixedColumnsLeft = manage.info$fixed.cols,
                         columnSorting = F)
              for(i in 1:nrow(manage.info$columns)) {
                col.info = manage.info$columns[i,]
                temp.table = temp.table %>%
                  hot_col(col = i, readOnly = !col.info$editable,
                          type = col.info$type,
                          halign = case_when(col.info$type == "checkbox" ~ "htCenter",
                                             col.info$type == "numeric" ~ "htRight",
                                             T ~ "htLeft"))
                if(col.info$html) {
                  temp.table = temp.table %>%
                    hot_col(col = i, format = "html",
                            renderer = htmlwidgets::JS("safeHtmlRenderer"))
                }
                if(col.info$type == "dropdown") {
                  temp.table = temp.table %>%
                    hot_col(col = i,
                            source = tables[[col.info$key.table]][[col.info$key.label]])
                }
              }
              temp.table
            }
          })
        }
      )
    
    # When the user makes a change to a management table, update the underlying
    # table accordingly
    purrr::walk(
      names(manage.table.info),
      function(manage.table) {
        
        # Get table info
        manage.info = manage.table.info[[manage.table]]
        
        # Update the table when it's changed
        observeEvent(input[[manage.table]], {
          
          # What kind of change happened?
          change = input[[manage.table]]$changes
          
          # If it was an edit, update the edited cell
          if(change$event == "afterChange") {
            if(!is.null(change$changes)) {
              r = change$changes[[1]][[1]] + 1
              c = change$changes[[1]][[2]] + 1
              tables[[manage.table]][r,c] = change$changes[[1]][[4]]
            }
          }
          
          # If it was a new row, insert an empty row
          else if(change$event == "afterCreateRow") {
            tables[[manage.table]][nrow(tables[[manage.table]]) + 1,] = NA
          }
          
          # If it was a deleted row, remove that row
          else if(change$event == "afterRemoveRow") {
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
          
          # Get raw data to use for update (mapping labels back to IDs)
          temp.df = tables[[manage.table]]
          for(i in 1:nrow(manage.info$columns)) {
            if(!is.na(manage.info$columns$key.table[i])) {
              temp.df = temp.df %>%
                left_join(tables[[manage.info$columns$key.table[i]]],
                          by = manage.info$columns$key.label[i])
            }
          }
          temp.df = temp.df[,manage.info$columns$column.name]
          
          # Create sql to update changed rows
          sql = temp.df %>%
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

          # Display a notice about whether anything went wrong
          if(successful.updates) {
            showNotification("Changes saved", type = "message")
          } else {
            showNotification("Some changes may not have been saved",
                             type = "error")
          }

          # Update table to reflect database
          populate.management.table(manage.table, lhp.con(), tables)
          
          # Update related tables and selectors
          for(related in manage.info$related.utility.tables) {
            populate.utility.table(related, lhp.con(), tables)
          }
          for(related in manage.info$related.selectors) {
            update.selector(related, lhp.con(), session)
          }
          for(related in manage.info$related.management.tables) {
            populate.management.table(related, lhp.con(), tables)
          }
          if(manage.info$related.processing.table) {
            populate.processing.table(lhp.con(), tables,
                                      input$process.return.hymnologist)
          }
          
        })
      }
    )
    
    # If connected, try to populate processing table; if the query fails, set
    # the content of the table to null
    populate.processing.table(lhp.con(), tables,
                              input$process.return.hymnologist)
    if(!is.null(tables$process.return)) {
      output$process.return = renderRHandsontable(
        rhandsontable(tables$process.return[,process.return.info$columns$displayed] %>%
                        mutate(Processed = Processed == 1),
                      width = input$dimension[1] * 0.75,
                      height = input$dimension[2] * 0.8,
                      rowHeaders = NULL, overflow = "visible") %>%
          hot_context_menu(allowRowEdit = F, allowColEdit = F) %>%
          hot_cols(colWidths = process.return.info$columns$width[process.return.info$columns$displayed],
                   columnSorting = F) %>%
          hot_col(col = "Processed", type = "checkbox",
                  halign = "htCenter") %>%
          hot_col(col = "SongLabel", type = "dropdown", strict = T,
                  renderer = "html", source = tables$song.labels$SongLabel) %>%
          hot_col(col = "SongLabel",
                  renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
          hot_col(col = which(!process.return.info$columns$editable[process.return.info$columns$displayed]),
                  readOnly = T)
      )
    } else {
      output$process.return = renderRHandsontable(NULL)
    }
    
    # If connected, try to populate summary table; if the query fails, set the
    # content of the table to null
    populate.summary.table(lhp.con(), tables)
    if(!is.null(tables$lhp.summary)) {
      output$lhp.summary = renderDT(
        tables$lhp.summary %>%
          dplyr::select(Song, Hymnologists, `Songs of Faith and Praise`,
                        `Praise for the Lord`,
                        `Psalms, Hymns, and Spiritual Songs`,
                        `Hymns for Worship`, `The Paperless Hymnal`,
                        Christmas, Nation) %>%
          datatable(options = list(searching = F, paging = F, info = F,
                                   columnDefs = list(list(visible = F,
                                                          targets = c(7, 8)))),
                    rownames = F) %>%
          formatStyle("Christmas", target = "row",
                      backgroundColor = styleEqual("Y", "palegreen")) %>%
          formatStyle("Nation", target = "row",
                      backgroundColor = styleEqual("Y", "lightcoral")),
      )
      output$lhp.histogram = renderPlot(
        tables$lhp.summary %>%
          mutate(Category = case_when(Christmas == "Y" ~ "Christmas",
                                      Nation == "Y" ~ "Nation",
                                      T ~ "None"),
                 prop = Hymnologists / TotalHymnologists) %>%
          group_by(prop, Category) %>%
          summarise(n.songs = n()) %>%
          ggplot(aes(x = prop, y = n.songs, fill = Category)) +
          geom_bar(stat = "identity") +
          scale_x_continuous(labels = scales::percent_format(),
                             breaks = seq(0, 1, 0.2)) +
          scale_fill_manual(values = c("palegreen3", "firebrick1", "gray50")) +
          labs(x = "Percent of hymnologists", y = "Number of songs")
      )
      output$lhp.violinplot = renderPlot(
        tables$lhp.summary %>%
          filter(Year > 0) %>%
          mutate(Category = case_when(Christmas == "Y" ~ "Christmas",
                                      Nation == "Y" ~ "Nation",
                                      T ~ "None"),
                 prop = Hymnologists / TotalHymnologists,
                 prop.bin = cut(prop, breaks = seq(0, 1, 0.1),
                                labels = paste(seq(0, 90, 10), "-",
                                               c(seq(9, 89, 10), 100),
                                               "%", sep = ""))) %>%
          ggplot(aes(x = prop.bin, y = Year)) +
          geom_violin(color = NA, fill = "gray90") +
          geom_point(aes(color = Category, alpha = Category),
                     position = position_jitter(width = 0.1)) +
          scale_color_manual(values = c("palegreen3", "firebrick1", "black")) +
          scale_alpha_manual(values = c(1, 1, 0.3)) +
          labs(x = "Percent of hymnologists", y = "Year composed")
      )
    } else {
      output$lhp.summary = renderDT(NULL)
      output$lhp.histogram = renderPlot(NULL)
    }

  })
  
  # When the user uploads a file, write its contents as a csv to the S3 bucket
  # and add the filename to the table of hymnologists; also update the
  # hymnologist management table and selector
  observeEvent(input$upload.return.file, {
    
    # Read in the data
    return.file.df = read_excel(input$return.file$datapath, col_names = "Song")
    
    # Determine the filename
    filename = tables$hymnologists %>%
      filter(HymnologistID == input$return.file.hymnologist) %>%
      mutate(filename = paste(HymnologistID,
                              tolower(gsub("[^A-Za-z]", "", FirstName)),
                              tolower(gsub("[^A-Za-z]", "", LastName)),
                              sep = "_"),
             filename = paste(filename, "csv", sep = ".")) %>%
      pull(filename)
    
    # Write the data to a csv in the S3 bucket; update the management table and
    # selectors accordingly
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
          populate.management.table("hymnologists", lhp.con(), tables)
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
          purrr::walk(
            c("return.file.hymnologist", "process.return.hymnologist"),
            function(selector.name) {
              update.selector(selector.name, lhp.con(), session)
            }
          )
        }
      }
    )
    
  })
  
  # When the user requests a refresh of the list of not-yet-processed returns,
  # update the list of hymnologists
  observeEvent(input$refresh.process.return.hymnologist, {
    if(!is.null(lhp.con())) {
      update.selector("process.return.hymnologist", lhp.con(), session)
    }
  })
  
  # When the user selects a hymnologist to process a return for, update the
  # table with return results
  observeEvent(input$process.return.hymnologist, {
    populate.processing.table(lhp.con(), tables,
                              input$process.return.hymnologist)
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
  # made by the user to the database (and refresh corresponding tables in the
  # app)
  observeEvent(input$save.return, {
    
    # Keep track of whether all updates were successful
    successful.updates = T
    
    # Create sql to update changed rows
    sql = tables$process.return %>%
      left_join(tables$song.labels, by = "SongLabel") %>%
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
    if(successful.updates) {
      showNotification("Changes saved", type = "message")
    } else {
      showNotification("Some changes may not have been saved", type = "error")
    }
    
    # Update table to reflect database
    populate.processing.table(lhp.con(), tables,
                              input$process.return.hymnologist)
    
    # Update management table to reflect database
    populate.management.table("hymnologist.returns", lhp.con(), tables)
    
    # Update summary table to reflect database
    populate.summary.table(lhp.con(), tables)
    
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

