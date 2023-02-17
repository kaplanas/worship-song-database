library(shiny)
library(shinyjs)
library(RMySQL)
library(tidyr)
library(date)
library(dplyr)
library(DT)
library(glue)

#### Useful initial settings ####

source("database_connections.R", local = T)

#### UI ####

# Elements of the UI
page.title = "Lasting Hymns Project"
source("lhp.R", local = T)
lhp.page = tabPanel("Manage tables",
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
  lhp.page
)

#### Server ####

# Define server logic
server <- function(input, output, session) {
  
  # Keep track of changing tables
  tables <- reactiveValues()
  tables$hymnologists <- dbGetQuery(lhp.con,
                                    "SELECT HymnologistID, LastName, FirstName,
                                            Created, Updated
                                     FROM lhp.hymnologists")
  
  # Manage hymnologists
  output$manage_hymnologists <- renderDT(
    datatable(tables$hymnologists, rownames = F, selection = 'none',
              editable = list(target = 'row',
                              disable = list(columns = c(0, 3, 4))),
              options = list(paging = F, searching = F, info = F)) %>%
      formatStyle(columns = c(1, 4, 5), backgroundColor = "lightgray")
  )
  observeEvent(input$add_hymnologist, {
    tables$hymnologists[nrow(tables$hymnologists) + 1,] = NA
  })
  observeEvent(input$manage_hymnologists_cell_edit, {
    r = input$manage_hymnologists_cell_edit$row
    c = input$manage_hymnologists_cell_edit$col
    tables$hymnologists[r,c[2:3]+1] <<- input$manage_hymnologists_cell_edit$value[2:3]
  })
  observeEvent(input$save_hymnologists, {
    sql = tables$hymnologists %>%
      filter(!is.na(HymnologistID)) %>%
      glue_data_sql("UPDATE lhp.hymnologists
                     SET LastName = {LastName},
                         FirstName = {FirstName}
                     WHERE HymnologistID = {HymnologistID}",
                    .con = lhp.con)
    for(s in sql) {
      dbGetQuery(lhp.con, s)
    }
    sql = tables$hymnologists %>%
      filter(is.na(HymnologistID)) %>%
      glue_data_sql("INSERT INTO lhp.hymnologists
                     (LastName, FirstName)
                     VALUES
                     ({LastName}, {FirstName})",
                    .con = lhp.con)
    for(s in sql) {
      dbGetQuery(lhp.con, s)
    }
  })
  
  # Disconnect from the database
  session$onSessionEnded(function(){
    dbDisconnect(lhp.con)
  })
  
}

#### Run ####

# Run the application 
shinyApp(ui = ui, server = server)

