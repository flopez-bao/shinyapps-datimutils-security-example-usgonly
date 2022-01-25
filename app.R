# import libraries -----
library(shiny)
library(futile.logger)
library(shinyWidgets)

source("./functions.R")

#which users should have access to data? IN THIS CASE ONLY USG FOLKS
USG_USERS = c("agency only", "interagency only", "global Agency", "global only")

# ui -----
ui <- shinyUI(
  uiOutput("ui")
)

# server ----
server <- function(input, output, session) {
  
  #user information
  user_input  <-  reactiveValues(authenticated = FALSE,
                                 status = "",
                                 d2_session = NULL,
                                 memo_authorized = FALSE)
  
  #is the user authenticated?
  output$ui <- renderUI({
    if(user_input$authenticated == FALSE) {
      uiOutput("uiLogin")
    } else {
      uiOutput("authenticated")
    }
  })
  
  #login page with username and password
  output$uiLogin  <-  renderUI({
    
    fluidPage(
      wellPanel(
        fluidRow(
          h4("The following up was created as an example of how developers can access the datim security information of their shiny app users utilizing the package datimutils. Please login with your DATIM credentials:"),
          br()
        ),
        fluidRow(
          textInput("user_name", "Username: ", width = "500px"),
          passwordInput("password", "Password:", width = "500px"),
          actionButton("login_button", "Log in!")
        )
      )
    )
    
  })
  
  #once you login this page shows up
  output$authenticated <- renderUI({ 
    fluidPage(
      fluidRow(
        h4("Click the following button to see information about this user, session and data access:")
      ),
      fluidRow(
        column(
          br(),
          br(),
          actionButton("groupid_button", "My User Groups aka Streams"),
          br(),
          br(),
          actionButton("me_button", "What is my User Type"),
          br(),
          br(),
          actionButton("mech_cocuid_button", "My Mechanisms by Category Option Combos Id"),
          br(),     
          br(),
          actionButton("mech_id_button", "My Mechanisms by Mech Number"),
          br(),
          br(),
          actionButton("mech_name_button", "My Mechanisms by Name"),
          br(),
          width = 6
        ),
        column(
          actionButton("logout_button", "Log out of Session", style="color: #fff; background-color: #FF0000; border-color: #2e6da4"),
          width = 6
        )
      ),
      br(),
      fluidRow(
        column(12,
               wellPanel(
                 verbatimTextOutput("message")
                 ,style = "overflow-y:scroll; max-height: 400px")
        )
      ),
      br(),
      fluidRow(
        column(12,
               dataTableOutput('table')
        )
      )
      
    )  
  })
  
  #Login process
  observeEvent(input$login_button, {
    tryCatch({
      datimutils::loginToDATIM(base_url = Sys.getenv("BASE_URL"),
                               username = input$user_name,
                               password = input$password,
                               d2_session_envir = parent.env(environment())
      )
      
      # DISALLOW USER ACCESS TO THE APP-----
      
      # access data streams
      d <- getStreams(username = input$user_name, password = input$password, base_url = Sys.getenv("BASE_URL"))
      # classify a user
      d <- getUserType(d$stream)
      
      #if a user is not to be allowed deny them entry
      if (!d %in% USG_USERS) {
        
        # alert the user they cannot access the app
        sendSweetAlert(
          session,
          title = "Login failed",
          text = "You are not authorized to use this application",
          type = "error"
        )
        
        # log them out
        Sys.sleep(3)
        flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
        user_input$authenticated  <-  FALSE
        user_input$user_name <- ""
        user_input$authorized  <-  FALSE
        user_input$d2_session  <-  NULL
        d2_default_session <- NULL
        gc()
        session$reload()
      
      }
      
      # ------
    },
    # This function throws an error if the login is not successful
    error = function(e) {
      flog.info(paste0("User ", input$username, " login failed."), name = "datapack")
    }
    )
    
    if (exists("d2_default_session")) {
      if (any(class(d2_default_session) == "d2Session")) {
        user_input$authenticated  <-  TRUE
        user_input$d2_session  <-  d2_default_session$clone()
        d2_default_session <- NULL
        
        
        # Need to check the user is a member of the PRIME Data Systems Group, COP Memo group, or a super user
        user_input$memo_authorized  <-
          grepl("VDEqY8YeCEk|ezh8nmc4JbX", user_input$d2_session$me$userGroups) |
          grepl(
            "jtzbVV4ZmdP",
            user_input$d2_session$me$userCredentials$userRoles
          )
        flog.info(
          paste0(
            "User ",
            user_input$d2_session$me$userCredentials$username,
            " logged in."
          ),
          name = "datapack"
        )
      }
    } else {
      sendSweetAlert(
        session,
        title = "Login failed",
        text = "Please check your username/password!",
        type = "error"
      )
    }
  })
  
  
  #show user information
  observeEvent(input$me_button, {
    d <- getStreams(username = input$user_name, password = input$password, base_url = Sys.getenv("BASE_URL"))
    d <- getUserType(d$stream)
    output$message <- renderPrint({d})
  })
  
  #show group ids data
  observeEvent(input$groupid_button, {
    
    groups_id_df <- getStreams(username = input$user_name, password = input$password, base_url = Sys.getenv("BASE_URL"))
    groups_id_df_f <- groups_id_df[!grepl("^Global|OU", groups_id_df$stream),,drop = F]
    output$message <- renderPrint({groups_id_df_f})
  })
  
  
  #show mechs by cocuid
  observeEvent(input$mech_cocuid_button, {
    
    d <- getStreams(username = input$user_name, password = input$password, base_url = Sys.getenv("BASE_URL"))
    d <- getUserType(d$stream)
    
    if(d %in% USG_USERS) {
      my_cat_ops <- getMechs(username = input$user_name, password = input$password, base_url = Sys.getenv("BASE_URL"), by= "cocuid")
      
      output$table <- renderDataTable(my_cat_ops,
                                      options = list(
                                        pageLength = 10,
                                        initComplete = I("function(settings, json) {alert('Done.');}")
                                      )
      )
    } else {
      access_denied <- paste("You are not allowed to see this information.")
      output$message <- renderPrint({ access_denied })  
    }
    
  })
  
  #show mechs by mechs id
  observeEvent(input$mech_id_button, {
    
    d <- getStreams(username = input$user_name, password = input$password, base_url = Sys.getenv("BASE_URL"))
    d <- getUserType(d$stream)
    
    if(d %in% USG_USERS) {
    
    my_cat_ops <- getMechs(username = input$user_name, password = input$password, base_url = Sys.getenv("BASE_URL"), by= "mech_id")
    
    output$table <- renderDataTable(my_cat_ops,
                                    options = list(
                                      pageLength = 10,
                                      initComplete = I("function(settings, json) {alert('Done.');}")
                                    )
    )
    } else {
      access_denied <- paste("You are not allowed to see this information.")
      output$message <- renderPrint({ access_denied })  
      
    }
    
  })
  
  #show mechs by name
  observeEvent(input$mech_name_button, {
    
    d <- getStreams(username = input$user_name, password = input$password, base_url = Sys.getenv("BASE_URL"))
    d <- getUserType(d$stream)
    
    if(d %in% USG_USERS) {
      
    
    my_cat_ops <- getMechs(username = input$user_name, password = input$password, base_url = Sys.getenv("BASE_URL"), by= "mech_name")
    
    output$table <- renderDataTable(my_cat_ops,
                                    options = list(
                                      pageLength = 10,
                                      initComplete = I("function(settings, json) {alert('Done.');}")
                                    )
    )
    } else {
      access_denied <- paste("You are not allowed to see this information.")
      output$message <- renderPrint({ access_denied })    
    }
    
    
  })
  
  
  
  #logout process
  observeEvent(input$logout_button, {
    flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
    user_input$authenticated  <-  FALSE
    user_input$user_name <- ""
    user_input$authorized  <-  FALSE
    user_input$d2_session  <-  NULL
    d2_default_session <- NULL
    gc()
    session$reload()
  })
}
shinyApp(ui, server)

