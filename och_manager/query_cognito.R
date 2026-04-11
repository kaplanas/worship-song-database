# Function that gets a list of Cognito users
get.users = function(cognito.session, user.pool.id, date.placeholder,
                     current.user, pagination.token = NULL) {
  results = cognito.session$list_users(UserPoolId = user.pool.id,
                                       PaginationToken = pagination.token)
  attributes.of.interest = c("name", "custom:service", "custom:city",
                             "custom:state", "custom:size", "custom:praise",
                             "custom:women", "custom:share")
  df = map_dfr(
    results$Users,
    function(user) {
      temp.df = data.frame(congregation = user$Username)
      for(attribute in user$Attributes) {
        if(attribute$Name %in% attributes.of.interest) {
          col.name = gsub(".*:", "", attribute$Name)
          if(attribute$Name %in% c("custom:share")) {
              temp.df[[col.name]] = as.logical(as.numeric(attribute$Value))
          } else if(attribute$Name %in% c("custom:praise", "custom:women")) {
            if(attribute$Value == date.placeholder) {
              temp.df[[col.name]] = NA
            } else {
              temp.df[[col.name]] = ymd(attribute$Value)
            }
          } else {
            temp.df[[col.name]] = attribute$Value
          }
        }
      }
      if(ncol(temp.df) == 1) {
        temp.df$name = NA_character_
      }
      for(a in c("name", "service", "city", "state", "size")) {
        if(!(a %in% colnames(temp.df))) {
          temp.df[[a]] = NA_character_
        }
      }
      for(a in c("praise", "women", "share")) {
        if(!(a %in% colnames(temp.df))) {
          temp.df[[a]] = NA
        }
      }
      temp.df %>%
        mutate(congregation.label = gsub("Church( of Christ)?( (at|in))?", "",
                                         name),
               congregation.label = gsub("^The ", "", congregation.label),
               congregation.label = str_trim(congregation.label),
               congregation.label = if_else(is.na(service), congregation.label,
                                            paste(congregation.label, service,
                                                  sep = ", ")),
               congregation.label = if_else(is.na(city), congregation.label,
                                            paste(congregation.label, " (",
                                                  city, ", ", state, ")",
                                                  sep = "")),
               is.me = congregation == current.user)
    }
  )
  if(length(results$PaginationToken) > 0) {
    next.page.df = get.users(cognito.session, user.pool.id, date.placeholder,
                             current.user, results$PaginationToken)
    return(bind_rows(df, next.page.df))
  } else {
    return(df)
  }
}
