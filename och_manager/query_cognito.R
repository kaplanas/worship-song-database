# Function that gets a list of Cognito users
get.users = function(cognito.session, user.pool.id, date.placeholder,
                     current.user, pagination.token = NULL) {
  results = cognito.session$list_users(UserPoolId = user.pool.id,
                                       AttributesToGet = list("name",
                                                              "custom:city",
                                                              "custom:state",
                                                              "custom:size",
                                                              "custom:praise",
                                                              "custom:women",
                                                              "custom:share"),
                                       PaginationToken = pagination.token)
  df = map_dfr(
    results$Users,
    function(user) {
      temp.df = data.frame(congregation = user$Username)
      for(attribute in user$Attributes) {
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
      if(!("city" %in% colnames(temp.df))) {
        temp.df$city = NA_character_
      }
      temp.df %>%
        mutate(congregation.label = gsub("Church( of Christ)?( (at|in))?", "",
                                         name),
               congregation.label = gsub("^The ", "", congregation.label),
               congregation.label = str_trim(congregation.label),
               congregation.label = if_else(is.na(city), congregation.label,
                                            paste(congregation.label, " (",
                                                  city, ", ", state, ")",
                                                  sep = "")))
    }
  )
  if(length(results$PaginationToken) > 0) {
    next.page.df = get.users(cognito.session, user.pool.id, date.placeholder,
                             current.user, results$PaginationToken)
    return(bind_rows(df, next.page.df))
  } else {
    df %>%
      filter(share) %>%
      dplyr::select(-c("share")) %>%
      mutate(is.me = congregation == current.user) %>%
      return()
  }
}
