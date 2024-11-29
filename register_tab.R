library(shiny)
library(shinyjs)

# Import functions from the authentication logic module
source("auth_logic.R")


# Registration Page
register_tab <- tabPanel(
  "Register",
  fluidPage(
    useShinyjs(),
    tags$div(
      class = "login-register-page",
      h1("Register", class = "header"),
      
      # Email Input Section
      textInput("register_email", "Email:", placeholder = "Enter your email address"),
      actionButton("send_otp_btn", "Enter Mail", class = "btn-primary"),
      
      # OTP Input Section (hidden initially)
      hidden(
        tags$div(
          id = "otp_section",
          textInput("otp_input", "Enter OTP:", placeholder = "Enter the OTP sent to your email"),
          actionButton("verify_otp_btn", "Verify OTP", class = "btn-primary"),
          textOutput("otp_status")  # Display OTP status (success or failure)
        )
      ),
      
      # Username and Password Section
      textInput("register_user", "Username:", placeholder = "Create your username"),
      passwordInput("register_pass", "Password:", placeholder = "Create your password"),
      actionButton("register_btn", "Register", class = "btn-primary"),
      actionButton("go_to_login", "Back to Login", class = "btn-primary"),
      textOutput("register_status")  # Registration status output
    )
  )
)

# Shiny Server Logic for the Registration Page
server <- function(input, output, session) {
  # Handle OTP sending
  observeEvent(input$send_otp_btn, {
    email <- input$register_email
    if (is_valid_email(email)) {
      otp <- send_otp(email)  # Call function from the auth_logic.R module
      session$userData$generated_otp <- otp  # Store the OTP in session
      shinyjs::show("otp_section")  # Reveal the OTP section
      showNotification("OTP has been sent to your email.", type = "message")
    } else {
      showNotification("Invalid email address.", type = "error")
    }
  })
  
  # Handle OTP verification
  observeEvent(input$verify_otp_btn, {
    entered_otp <- input$otp_input
    if (verify_otp(entered_otp, session$userData$generated_otp)) {
      output$otp_status <- renderText("OTP verified successfully!")
      showNotification("OTP verified successfully!", type = "message")
    } else {
      output$otp_status <- renderText("Invalid OTP. Please try again.")
      showNotification("Invalid OTP.", type = "error")
    }
  })
  
  # Handle User Registration
  observeEvent(input$register_btn, {
    username <- input$register_user
    password <- input$register_pass
    
    if (nzchar(username) && nzchar(password)) {
      output$register_status <- renderText("Registration successful!")
      showNotification("User registered successfully!", type = "message")
    } else {
      output$register_status <- renderText("Please fill in all fields.")
      showNotification("Please fill in all fields to register.", type = "error")
    }
  })
}
