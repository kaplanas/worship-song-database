#### Utility functions ####

# Function that converts a record to the format required by DynamoDB and writes
# it
write.dynamo.items = function(db.con, dynamo.con, table.info, change.key) {
  if("wsf.updates" %in% names(table.info)) {
    for(write.table in names(table.info$wsf.updates$write)) {
      sql = glue_sql(table.info$wsf.updates$write[[write.table]],
                     keys = change.key, .con = db.con)
      items.df = dbGetQuery(db.con, sql)
      if(nrow(items.df) > 0) {
        item.cols = sapply(items.df, class)
        for(i in 1:nrow(items.df)) {
          item = list()
          for(field in names(item.cols)) {
            if(!is.na(items.df[[field]][i])) {
              if(item.cols[field] == "integer") {
                item[[field]] = list(N = items.df[[field]][i])
              } else if(item.cols[field] == "character") {
                item[[field]] = list(S = items.df[[field]][i])
              }
            }
          }
          dynamo.con$put_item(TableName = write.table, Item = item)
        }
      }
    }
  }
}

# Function that gets keys of items we will later delete from DynamoDB
get.dynamo.delete.keys = function(db.con, table.info, temp.df, exclude = T) {
  if("wsf.updates" %in% names(table.info) &
     "delete" %in% names(table.info$wsf.updates)) {
    key = table.info$key
    sql = paste("SELECT DISTINCT ", key, " FROM ", table.info$table,
                " WHERE ", key, " ", ifelse(exclude, "NOT ", ""), "IN (",
                paste(map(1:nrow(temp.df), function(i) { temp.df[i,key] }),
                      collapse = ", "),
                ")", sep = "")
    delete.keys = dbGetQuery(db.con, sql)
    return(delete.keys)
  }
}

# Function that deletes items from DynamoDB
delete.dynamo.items = function(dynamo.con, table.info, delete.keys) {
  if("wsf.updates" %in% names(table.info) &
     "delete" %in% names(table.info$wsf.updates)) {
    field = table.info$key
    key.class = class(delete.keys[[field]])
    key.class = list("integer" = "N", "numeric" = "N",
                     "character" = "S")[[key.class]]
    for(i in 1:nrow(delete.keys)) {
      key = list()
      key.field = list()
      key.field[[key.class]] = delete.keys[i,field]
      key[[field]] = key.field
      dynamo.con$delete_item(Key = key,
                             TableName = table.info$wsf.updates$delete)
    }                    
  }
}

# Function that deletes items from many-to-many bridge tables in DynamoDB
delete.dynamo.multi.items = function(db.con, dynamo.con, table.info, id) {
  if("wsf.updates" %in% names(table.info) &
     "multi" %in% names(table.info$wsf.updates)) {
    for(multi.table in names(table.info$wsf.updates$multi)) {
      sql = table.info$wsf.updates$multi[[multi.table]]$sql
      sql = gsub("SELECT +\\*",
                 paste("SELECT",
                       paste(table.info$wsf.updates$multi[[multi.table]]$keys,
                             collapse = ", ")),
                 sql)
      sql = glue_sql(sql, keys = id, .con = db.con)
      delete.keys = dbGetQuery(db.con, sql)
      item.cols = sapply(delete.keys, class)
      if(nrow(delete.keys) > 0) {
        for(i in 1:nrow(delete.keys)) {
          item = list()
          for(field in table.info$wsf.updates$multi[[multi.table]]$keys) {
            if(!is.na(delete.keys[[field]][i])) {
              if(item.cols[field] %in% c("integer", "numeric")) {
                item[[field]] = list(N = delete.keys[[field]][i])
              } else if(item.cols[field] == "character") {
                item[[field]] = list(S = delete.keys[[field]][i])
              }
            }
          }
          dynamo.con$delete_item(Key = item, TableName = multi.table)
        }
      }
    }
  }
}

# Function that inserts new items into many-to-many bridge tables in DynamoDB
insert.dynamo.multi.items = function(db.con, dynamo.con, table.info,
                                     change.key) {
  if("wsf.updates" %in% names(table.info) &
     "multi" %in% names(table.info$wsf.updates)) {
    for(multi.table in names(table.info$wsf.updates$multi)) {
      sql = glue_sql(table.info$wsf.updates$multi[[multi.table]]$sql,
                     keys = change.key, .con = db.con)
      items.df = dbGetQuery(db.con, sql)
      if(nrow(items.df) > 0) {
        item.cols = sapply(items.df, class)
        for(i in 1:nrow(items.df)) {
          item = list()
          for(field in names(item.cols)) {
            if(!is.na(items.df[[field]][i])) {
              if(item.cols[field] %in% c("integer", "numeric")) {
                item[[field]] = list(N = items.df[[field]][i])
              } else if(item.cols[field] == "character") {
                item[[field]] = list(S = items.df[[field]][i])
              }
            }
          }
          dynamo.con$put_item(TableName = multi.table, Item = item)
        }
      }
    }
  }
}

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
