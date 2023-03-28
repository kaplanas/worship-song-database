#### Code from other files ####

source("reference_tables.R", local = T)

#### Page for managing all kinds of tables ####

tables.page = tabPanel("Manage tables",
                       navlistPanel(
                         reference.tables.page,
                         well = F,
                         widths = c(2, 10)
              ))
