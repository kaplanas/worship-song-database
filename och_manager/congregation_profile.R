# Management page
congregation.page = tabPanel(
  "Congregation info",
  fluidRow(
    column(2, actionButton("save.congregation", "Save changes")),
    column(5, selectInput("congregation.sharing",
                          "Share worship history data with other congregations?",
                          c("Yes", "Anonymously", "No"), selected = "No",
                          width = "100%"))
  ),
  fluidRow(
    column(4,
           textInput("congregation.name", "Congregation name", width = "100%")),
    column(3,
           textInput("congregation.service",
                     "Which Sun. AM service? (if applicable)", width = "100%")),
    column(2,
           textInput("congregation.email", "Congregation email", width = "100%")),
    column(3,
           textInput("congregation.website", "Congregation website", width = "100%"))
  ),
  fluidRow(
    column(5, textInput("congregation.street", "Street", width = "100%")),
    column(3, textInput("congregation.city", "City", width = "100%")),
    column(2, textInput("congregation.state", "State", width = "100%")),
    column(2, textInput("congregation.zip", "ZIP", width = "100%"))
  ),
  fluidRow(
    column(4,
           selectInput("congregation.size", "Congregation size",
                       c("[not specified]", "0-30", "31-100", "101-200",
                         "200-500", "501-1000", "1000+"))),
    column(4,
           airDatepickerInput("congregation.praise",
                              "Praise team since (if applicable)",
                              dateFormat = "MMM d, yyyy")),
    column(4,
           airDatepickerInput("congregation.women",
                              "Women leading worship since (if applicable)",
                              dateFormat = "MMM d, yyyy"))
  )
)

# Map between Cognito names for various fields and input IDs
attribute.ids = list("name" = "congregation.name",
                     "custom:service" = "congregation.service",
                     "email" = "congregation.email",
                     "website" = "congregation.website",
                     "custom:street" = "congregation.street",
                     "custom:city" = "congregation.city",
                     "custom:state" = "congregation.state",
                     "custom:zip" = "congregation.zip",
                     "custom:size" = "congregation.size",
                     "custom:praise" = "congregation.praise",
                     "custom:women" = "congregation.women")

# Function that updates the UI with the attributes retrieved from Cognito
update.attribute.ui = function(attributes, session) {
  temp.share = F
  temp.anonymous = F
  for(attribute in attributes) {
    if(attribute$Name %in% names(attribute.ids)) {
      input.id = attribute.ids[[attribute$Name]]
      if(attribute$Name == "custom:size") {
        updateSelectInput(session, input.id, selected = attribute$Value)
      } else if(attribute$Name %in% c("custom:praise", "custom:women") &
                attribute$Value != date.placeholder) {
        updateAirDateInput(session, input.id, value = ymd(attribute$Value))
      } else {
        updateTextInput(session, input.id, value = attribute$Value)
      }
    } else if(attribute$Name == "custom:share") {
      temp.share = as.logical(as.numeric(attribute$Value))
    } else if(attribute$Name == "custom:anonymous") {
      temp.anonymous = as.logical(as.numeric(attribute$Value))
    }
  }
  sharing.option = "No"
  if(temp.share) {
    if(temp.anonymous) {
      sharing.option = "Anonymously"
    } else {
      sharing.option = "Yes"
    }
  }
  updateSelectInput(session, "congregation.sharing", selected = sharing.option)
}

# Function that updates user attributes in Cognito based on the selected input
# options
update.attributes = function(attribute.values, sharing.value, cognito.client,
                             access.token) {
  ids = names(attribute.ids)
  names(ids) = attribute.ids
  tryCatch(
    {
      attribute.updates = list()
      attribute.deletions = c()
      for(field in names(ids)) {
        attribute.name = ids[[field]]
        if(!is.null(attribute.values[[field]])) {
          attribute.value = attribute.values[[field]]
          if(field %in% c("congregation.praise", "congregation.women")) {
            attribute.value = as.numeric(format(attribute.value, "%Y%m%d"))
          }
          if(nchar(attribute.value) > 0) {
            attribute.updates[[field]] = list(Name = attribute.name, Value = attribute.value)
          }
        } else {
          attribute.deletions = c(attribute.deletions, attribute.name)
        }
      }
      coords = data.frame(Street = attribute.values$congregation.street,
                          City = attribute.values$congregation.city,
                          State = attribute.values$congregation.state,
                          ZIP = attribute.values$congregation.zip) %>%
                 geocode(street = Street, city = City, state = State,
                         limit = 1, method = "census")
      if(!is.na(coords$lat[1])) {
        attribute.updates[[length(attribute.updates) + 1]] = list(Name = "custom:latitude",
                                                                  Value = coords$lat)
        attribute.updates[[length(attribute.updates) + 1]] = list(Name = "custom:longitude",
                                                                  Value = coords$long)
      }
      temp.share = F
      temp.anonymous = F
      if(sharing.value == "Yes") {
        temp.share = T
        temp.anonymous = F
      } else if(sharing.value == "Anonymously") {
        temp.share = T
        temp.anonymous = T
      }
      attribute.updates[[length(attribute.updates) + 1]] = list(Name = "custom:share",
                                                                Value = as.numeric(temp.share))
      attribute.updates[[length(attribute.updates) + 1]] = list(Name = "custom:anonymous",
                                                                Value = as.numeric(temp.anonymous))
      print(attribute.updates)
      print(attribute.deletions)
      cognito.client$update_user_attributes(UserAttributes = attribute.updates,
                                            AccessToken = access.token)
      cognito.client$delete_user_attributes(UserAttributeNames = attribute.deletions,
                                            AccessToken = access.token)
      showNotification("Update successful", type = "message")
    },
    error = function(err) {
      print(err)
      showNotification(err, type = "error")
    }
  )
}
