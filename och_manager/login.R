# Login page
login.page = nav_panel(
  title = "Log in",
  tabsetPanel(
    header = tags$head(tags$script('var dimension = [0, 0];
                                   $(document).on("shiny:connected", function(e) {
                                       dimension[0] = window.innerWidth;
                                       dimension[1] = window.innerHeight;
                                       Shiny.onInputChange("dimension", dimension);
                                   });
                                   $(window).resize(function(e) {
                                       dimension[0] = window.innerWidth;
                                       dimension[1] = window.innerHeight;
                                       Shiny.onInputChange("dimension", dimension);
                                   });'),
                      tags$style(HTML(".airdatepicker-highlighted {
                                         background: #ADD8E6;
                                       }"))),
    tabPanel("Log in",
             tagQuery(textInput("existing.username", "Username"))$find("input")$addAttrs("autocomplete" = "off")$allTags(),
             passwordInput("existing.password", "Password"),
             actionButton("log.in", label = "Log in")),
    tabPanel("Change password",
             textInput("change.username", "Username"),
             passwordInput("change.old.password", "Old password"),
             passwordInput("change.new.password", "New password (min 8 characters)"),
             actionButton("change.password", label = "Change password")),
    tabPanel("Forgot password",
             textInput("forgotten.username", "Username"),
             actionButton("forgot.password", label = "Get verification code")),
    tabPanel("Reset password",
             textInput("reset.username", "Username"),
             passwordInput("reset.code", "Verification code"),
             passwordInput("reset.new.password", "New password (min 8 characters)"),
             actionButton("reset.password", label = "Reset password"))
  )
)

# Function that takes the username, client ID, and client secret and generates
# the hash needed to authenticate with Cognito
create.secret.hash = function(username, client.id, client.secret) {
  sha256(charToRaw(paste(username, client.id, sep = "")),
         charToRaw(client.secret)) %>%
    base64_encode() %>%
    return()
}

# Function that handles a change password request
change.password = function(cognito.client, username, old.password, new.password,
                           client.id, client.secret) {
  tryCatch(
    {
      secret.hash = create.secret.hash(username, client.id, client.secret)
      auth.res = cognito.client$initiate_auth(AuthFlow = "USER_PASSWORD_AUTH",
                                              AuthParameters = list(USERNAME = username,
                                                                    PASSWORD = old.password,
                                                                    SECRET_HASH = secret.hash),
                                              ClientId = client.id)
      if(length(auth.res$AuthenticationResult$AccessToken) > 0) {
        cognito.client$change_password(PreviousPassword = old.password,
                                       ProposedPassword = new.password,
                                       AccessToken = auth.res$AuthenticationResult$AccessToken)
        showNotification("Password changed", type = "message")
      }
      else if(auth.res$ChallengeName == "NEW_PASSWORD_REQUIRED") {
        cognito.client$respond_to_auth_challenge(ClientId = client.id,
                                                 ChallengeName = "NEW_PASSWORD_REQUIRED",
                                                 ChallengeResponses = list(USERNAME = username,
                                                                           NEW_PASSWORD = new.password,
                                                                           SECRET_HASH = secret.hash),
                                                 Session = auth.res$Session)
        showNotification("Password changed", type = "message")
      }
    },
    error = function(err) {
      cat(file = stderr(), err$message)
      showNotification("Password change failed", type = "error")
    }
  )
}

# Function that starts the password reset process
forgot.password = function(cognito.client, username, client.id, client.secret) {
  tryCatch(
    {
      secret.hash = create.secret.hash(username, client.id, client.secret)
      reset.result = cognito.client$forgot_password(ClientId = client.id,
                                                    SecretHash = secret.hash,
                                                    Username = username)
      showNotification("Check your email for a verification code",
                       type = "warning")
    },
    error = function(err) {
      cat(file = stderr(), err$message)
      showNotification("Sending verification code failed", type = "error")
    }
  )
}

# Function that completes the password reset process
reset.password = function(cognito.client, username, reset.code, new.password,
                          client.id, client.secret) {
  tryCatch(
    {
      secret.hash = create.secret.hash(username, client.id, client.secret)
      cognito.client$confirm_forgot_password(ClientId = client.id,
                                             Username = username,
                                             ConfirmationCode = reset.code,
                                             Password = new.password,
                                             SecretHash = secret.hash)
      showNotification("Password reset", type = "message")
    },
    error = function(err) {
      cat(file = stderr(), err$message)
      showNotification("Password reset failed", type = "error")
    }
  )
}
  
# Function that logs in and returns the authentication result
log.in = function(cognito.client, username, password, client.id,
                  client.secret) {
  secret.hash = create.secret.hash(username, client.id, client.secret)
  auth.res = cognito.client$initiate_auth(AuthFlow = "USER_PASSWORD_AUTH",
                                          AuthParameters = list(USERNAME = username,
                                                                PASSWORD = password,
                                                                SECRET_HASH = secret.hash),
                                          ClientId = client.id)
  return(auth.res)
}
