#### Code from other files ####

source("reference_tables.R", local = T)
source("form_tables.R", local = T)

#### Page for managing all kinds of tables ####

tables.page = tabPanel("Manage tables",
                       navlistPanel(
                         reference.tables.page,
                         form.table.info$songs$tab.panel,
                         form.table.info$lyrics$tab.panel,
                         form.table.info$tunes$tab.panel,
                         form.table.info$arrangements$tab.panel,
                         form.table.info$song.instances$tab.panel,
                         form.table.info$metrical.psalms$tab.panel,
                         well = F,
                         widths = c(2, 10)
              ))
