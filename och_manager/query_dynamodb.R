# Function that queries DynamoDB and returns the results as a table.
query.dynamodb = function(db, table.name, index.name = NULL,
                          expression.attribute.values, key.condition.expression,
                          projection.expression = NULL,
                          filter.expression = NULL, start.key = NULL) {
  results = db$query(TableName = table.name, IndexName = index.name,
                     ProjectionExpression = projection.expression,
                     ExpressionAttributeValues = expression.attribute.values,
                     KeyConditionExpression = key.condition.expression,
                     FilterExpression = filter.expression,
                     ExclusiveStartKey = start.key)
  df = map_dfr(
    results$Items,
    function(item) {
      map(
        item,
        function(field) {
          if(length(field$N) > 0) {
            as.numeric(field$N)
          } else if(length(field$S) > 0) {
            field$S
          } else if(length(field$BOOL) > 0) {
            as.logical(field$BOOL)
          }
        }
      )
    }
  )
  if(length(results$LastEvaluatedKey) > 0) {
    next.page.df = query.dynamodb(db, table.name = table.name,
                                  index.name = index.name,
                                  expression.attribute.values = expression.attribute.values,
                                  key.condition.expression = key.condition.expression,
                                  projection.expression = projection.expression,
                                  filter.expression = filter.expression,
                                  start.key = results$LastEvaluatedKey)
    return(bind_rows(df, next.page.df))
  } else {
    return(df)
  }
}

# Function that scans DynamoDB and returns the results as a table.
scan.dynamodb = function(db, table.name, index.name = NULL,
                         expression.attribute.values = NULL,
                         projection.expression = NULL, filter.expression = NULL, 
                         start.key = NULL) {
  results = db$scan(TableName = table.name, IndexName = index.name,
                    ExpressionAttributeValues = expression.attribute.values,
                    ProjectionExpression = projection.expression,
                    FilterExpression = filter.expression,
                    ExclusiveStartKey = start.key)
  df = map_dfr(
    results$Items,
    function(item) {
      map(
        item,
        function(field) {
          if(length(field$N) > 0) {
            as.numeric(field$N)
          } else if(length(field$S) > 0) {
            field$S
          } else if(length(field$BOOL) > 0) {
            as.logical(field$BOOL)
          }
        }
      )
    }
  )
  if(length(results$LastEvaluatedKey) > 0) {
    next.page.df = scan.dynamodb(db, table.name = table.name,
                                 index.name = index.name,
                                 expression.attribute.values = expression.attribute.values,
                                 projection.expression = projection.expression,
                                 filter.expression = filter.expression,
                                 start.key = results$LastEvaluatedKey)
    return(bind_rows(df, next.page.df))
  } else {
    return(df)
  }
}

# Function that gets the dates for the current congregation from DynamoDB
get.current.dates = function(db, username) {
  if(!is.null(username)) {
    tryCatch(
      {
        dates.df = query.dynamodb(db, table.name = "och_dates",
                                  expression.attribute.values = list(`:c` = list(S = username)),
                                  key.condition.expression = "Congregation = :c",
                                  projection.expression = "WorshipDate, AnyUnprocessed, AnyNotes")
        if(nrow(dates.df) > 0) {
          dates.df = dates.df %>%
            mutate(worship.date = ymd(WorshipDate)) %>%
            arrange(worship.date)
        } else {
          dates.df = NULL
        }
        showNotification("Retrieved worship history dates", type = "message")
        return(dates.df)
      },
      error = function(err) {
        print(err)
        showNotification(err, type = "error")
      }
    )
  }
}
