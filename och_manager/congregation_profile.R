# Management page
congregation.page = tabPanel(
  "Congregation info",
  fluidRow(
    column(1, actionButton("save.congregation", "Save changes")),
    column(3, checkboxInput("congregation.share",
                            "Share worship history data with other congregations"))
  ),
  fluidRow(
    column(4,
           textInput("congregation.name", "Congregation name", width = "100%")),
    column(4,
           textInput("congregation.email", "Congregation email", width = "100%")),
    column(4,
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
                     "email" = "congregation.email",
                     "website" = "congregation.website",
                     "custom:share" = "congregation.share",
                     "custom:street" = "congregation.street",
                     "custom:city" = "congregation.city",
                     "custom:state" = "congregation.state",
                     "custom:zip" = "congregation.zip",
                     "custom:size" = "congregation.size",
                     "custom:praise" = "congregation.praise",
                     "custom:women" = "congregation.women")

# Function that updates the UI with the attributes retrieved from Cognito
update.attribute.ui = function(attributes, session) {
  walk(
    attributes,
    function(attribute) {
      if(attribute$Name %in% names(attribute.ids)) {
        input.id = attribute.ids[[attribute$Name]]
        if(attribute$Name == "custom:share") {
          updateCheckboxInput(session, input.id,
                              value = as.logical(attribute$Value))
        } else if(attribute$Name == "custom:size") {
          updateSelectInput(session, input.id, selected = attribute$Value)
        } else if(attribute$Name %in% c("custom:praise", "custom:women") &
                  attribute$Value != date.placeholder) {
          updateAirDateInput(session, input.id, value = ymd(attribute$Value))
        } else {
          updateTextInput(session, input.id, value = attribute$Value)
        }
      }
    }
  )
}

# Function that updates user attributes in Cognito based on the selected input
# options
update.attributes = function(attribute.values, cognito.client, access.token) {
  ids = names(attribute.ids)
  names(ids) = attribute.ids
  tryCatch(
    {
      attribute.updates = map(
        names(ids),
        function(field) {
          attribute.name = ids[[field]]
          if(!is.null(attribute.values[[field]])) {
            attribute.value = attribute.values[[field]]
            if(field == "congregation.share") {
              attribute.value = as.numeric(attribute.value)
            } else if(field %in% c("congregation.praise",
                                   "congregation.women")) {
              attribute.value = as.numeric(format(attribute.value, "%Y%m%d"))
            }
            list(Name = attribute.name, Value = attribute.value)
          }  else {
            if(field %in% c("congregation.share", "congregation.praise",
                            "congregation.women")) {
              v = 0
              if(field %in% c("congregation.praise", "congregation.women")) {
                v = date.placeholder
              }
              list(Name = attribute.name, Value = v)
            } else {
              list(Name = attribute.name, Value = "")
            }
          }
        }
      )
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
      showNotification("about to update attributes")
      cognito.client$update_user_attributes(UserAttributes = attribute.updates,
                                            AccessToken = access.token)
      showNotification("Update successful", type = "message")
    },
    error = function(err) {
      print(err)
      showNotification(err, type = "error")
    }
  )
}
