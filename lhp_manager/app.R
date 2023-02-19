library(shiny)
library(shinyjs)
library(RMySQL)
library(tidyr)
library(date)
library(dplyr)
library(DT)
library(glue)

#### UI ####

# Elements of the UI
page.title = "Lasting Hymns Project"
source("tables.R", local = T)
tables.page = tabPanel("Manage tables",
                       useShinyjs(),
                       navlistPanel(
                         manage.hymnologists,
                         well = F,
                         widths = c(2, 10)
                       )
)

# Define UI
ui <- navbarPage(
  page.title,
  tabPanel("Log in",
           textInput("lhp_username", "Username"),
           passwordInput("lhp_password", "Password"),
           actionButton("lhp_log_in", label = "Log in")),
  tables.page
)

#### Server ####

# Define server logic
server <- function(input, output, session) {
  
  # Database connection
  lhp.con = reactiveVal(NULL)
  
  # Keep track of reactive stuff
  tables = reactiveValues()
  tables$hymnologists = NULL
  
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
        if(!is.null(lhp.con())) {
          dbDisconnect(lhp.con())
        }
        lhp.con(NULL)
        showNotification("Login failed", type = "error")
        return()
      }
    )
    
    # If connected, try to populate tables; if the query fails, set the content
    # of the tables to null
    if(!is.null(lhp.con())) {
      tryCatch(
        {
          tables$hymnologists = dbGetQuery(lhp.con(),
                                           "SELECT HymnologistID, LastName,
                                                   FirstName, Created, Updated
                                            FROM lhp.hymnologists")
        },
        error = function(err) {
          tables$hymnologists = NULL
          output$manage_hymnologists = renderDT(NULL)
        },
        finally = {
          if(!is.null(tables$hymnologists)) {
            output$manage_hymnologists = renderDT(
              datatable(tables$hymnologists, rownames = F,
                        selection = list(mode = "single", target = "row"),
                        editable = list(target = "row",
                                        disable = list(columns = c(0, 3, 4))),
                        options = list(paging = F, searching = F, info = F)) %>%
                formatStyle(columns = c(1, 4, 5),
                            backgroundColor = "lightgray"))
          } else {
            output$manage_hymnologists = renderDT(NULL)
          }
        }
      )
    } else {
      tables$hymnologists = NULL
      output$manage_hymnologists = renderDT(NULL)
    }
    
  })
  
  # When the user clicks the "Add" button, add a row to the table
  observeEvent(input$add_hymnologist, {
    tables$hymnologists[nrow(tables$hymnologists) + 1,] = NA
  })
  
  # When the user clicks the "Delete" button, delete the selected row from the
  # table
  observeEvent(input$delete_hymnologist, {
    tables$hymnologists = tables$hymnologists[-input$manage_hymnologists_rows_selected,]
  })
  
  # When the user edits a cell, store the edits in the table (but not edits to
  # uneditable columns; they should be set to uneditable anyway, but the
  # redundancy here is nice)
  observeEvent(input$manage_hymnologists_cell_edit, {
    r = input$manage_hymnologists_cell_edit$row
    c = input$manage_hymnologists_cell_edit$col
    tables$hymnologists[r,c[2:3]+1] = input$manage_hymnologists_cell_edit$value[2:3]
  })
  
  # When the user clicks the "Save" button, attempt to write the table to the
  # database
  observeEvent(input$save_hymnologists, {
    
    # Attempt to update changed rows
    sql = tables$hymnologists %>%
      filter(!is.na(HymnologistID)) %>%
      glue_data_sql("UPDATE lhp.hymnologists
                     SET LastName = {LastName},
                         FirstName = {FirstName}
                     WHERE HymnologistID = {HymnologistID}",
                    .con = lhp.con())
    successful.updates = T
    for(s in sql) {
      tryCatch(
        {
          dbGetQuery(lhp.con(), s)
        },
        error = function(err) {
          successful.updates = F
        }
      )
    }
    
    # Attempt to insert new rows
    if(any(is.na(tables$hymnologists$HymnologistID))) {
      sql = tables$hymnologists %>%
        filter(is.na(HymnologistID)) %>%
        glue_data_sql("INSERT INTO lhp.hymnologists
                       (LastName, FirstName)
                       VALUES
                       ({LastName}, {FirstName})",
                      .con = lhp.con())
      for(s in sql) {
        tryCatch(
          {
            dbGetQuery(lhp.con(), s)
          },
          error = function(err) {
            successful.updates = F
          }
        )
      }
    }
    
    # Attempt to delete rows
    sql = glue_sql("DELETE FROM lhp.hymnologists
                    WHERE HymnologistID NOT IN ({hymnologist.ids*})",
                   hymnologist.ids = tables$hymnologists$HymnologistID,
                   .con = lhp.con())
    tryCatch(
      {
        dbGetQuery(lhp.con(), sql)
      },
      error = function(err) {
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
        tables$hymnologists = dbGetQuery(lhp.con(),
                                         "SELECT HymnologistID, LastName, FirstName,
                                                 Created, Updated
                                          FROM lhp.hymnologists")
      },
      error = function(err) {
        tables$hymnologists = NULL
      }
    )
  })

  # Disconnect from the database
  session$onSessionEnded(function(){
    if(!is.null(isolate(lhp.con()))) {
      dbDisconnect(isolate(lhp.con()))
    }
  })
  
}

#### Run ####

# Run the application 
shinyApp(ui = ui, server = server)

