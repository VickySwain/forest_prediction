library(shiny)
library(DT)
library(colourpicker)  # Make sure this is loaded
library(ggplot2)
library(RSQLite)
library(treemap)
library(shinyjs)  # Add this for JavaScript-based tab switching

# Set up SQLite database
db_path <- "user_database.sqlite"
db <- dbConnect(SQLite(), dbname = db_path)

# Create a users table if it doesn't exist
dbExecute(db, "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, username TEXT UNIQUE, password TEXT)")

# Placeholder for user data (temporary in-memory storage)
user_data <- reactiveVal(data.frame(
  username = character(),
  password = character(),
  stringsAsFactors = FALSE
))


# Sample dataset (use your actual forestfires dataset if it exists)
forestfires <- read.csv("D:/RStudio/Project/forestfires.csv")
library(shiny)
library(shinyjs)

# Simulated function to send OTP to an email (replace with real email-sending logic)
send_otp <- function(email) {
  otp <- sprintf("%06d", sample(0:999999, 1))  # Generate a random 6-digit OTP
  # Logic to send email here (e.g., using mailR/sendmailR or an API)
  message(sprintf("OTP sent to %s: %s", email, otp))  # Simulate sending the email
  return(otp)
}

# Global variable to store OTP for verification (in production, use session-specific storage)
stored_otp <- reactiveVal(NULL)

# Define the UI
ui <- navbarPage(
  id = "navbar",  # Adding an ID for the navbarPage so that it can be updated in server
  title = "Forest Fire Prediction Dashboard",
  
  
  # Set Home as the selected tab by default
  selected = "Home",  # Add this line
  
  # Include the dark theme CSS
  tags$head(
    tags$style(HTML("
    body {
      background-image: url('background.jpg'); 
      background-size: cover;
      background-attachment: fixed;
      color: white; /* Update general text to white */
      font-family: 'Open Sans', Arial, sans-serif; /* Modern font */
    }
    .navbar {
      background-color: transparent !important; /* Transparent background */
      border: none; /* Remove border */
    }
    .navbar-brand, .navbar-nav .nav-link {
      color: #FFA500 !important; /* Orange for navigation text */
      font-weight: bold;
      text-transform: uppercase;
    }
    .navbar-nav .nav-link:hover {
      color: #FFD700 !important; /* Golden yellow on hover */
    }
    .navbar-nav .active {
      border-bottom: 2px solid #FFA500; /* Highlight the active tab */
    }
    h1 {
      color: #FFA500; /* Orange for homepage heading */
      text-align: center;
      padding: 10px;
      font-weight: bold;
      font-size: 2.5em;
      margin-bottom: 20px;
      text-transform: uppercase;
    }
    .sidebar {
      background-color: #f5f5f5; /* Light gray sidebar */
      padding: 15px;
      border-radius: 8px;
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1); /* Subtle shadow */
      color: #333333;
    }
    .output-box {
    background-color: #d3d3d3 !important; /* Light grey background */
    color: #333333 !important; /* Dark grey text */
    padding: 15px;
    border-radius: 8px;
    text-align: center;
    font-size: 1.2em;
    margin-top: 20px;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1); /* Subtle shadow */
  }
    .plot-area {
      background-color: #d3d3d3; /* White plot area */
      padding: 15px;
      border-radius: 8px;
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1); /* Subtle shadow */
    }
    .footer {
      background-color: #ffffff; /* White footer */
      color: #FFA500; /* Accent blue */
      padding: 10px;
      text-align: center;
      font-size: 1em;
      position: fixed;
      bottom: 0;
      width: 100%;
    }
    .landing-page {
      text-align: center;
      padding: 50px;
      color: white; /* Update general text to white */
    }
    .landing-page .header {
      font-size: 3em;
      font-weight: bold;
      color: #FFA500; /* Orange for heading */
      margin-bottom: 20px;
    }
    .landing-page .subheader {
      font-size: 1.2em;
      margin-bottom: 40px;
      color: white; /* Change subheader text to white */
    }
    .landing-page .btn-primary {
      background-color: #FFA500; /* Orange for buttons */
      border: none;
      font-size: 1.2em;
      padding: 15px 30px;
      border-radius: 8px;
      text-transform: uppercase;
      color: white;
    }
    .landing-page .btn-primary:hover {
      background-color: #CC8400; /* Darker orange on hover */
    }
    .logout-btn {
    position: absolute;
    top: 10px;
    right: 10px;
    background-color: #FF0000 !important; /* Red button */
    border: none;
    color: white;
    font-weight: bold;
    font-size: 1em;
    padding: 10px 20px;
    border-radius: 5px;
    text-transform: uppercase;
  }
  .logout-btn:hover {
    background-color: #CC0000 !important; /* Darker red on hover */
  }
    .features-section {
      margin-top: 50px;
      display: flex;
      justify-content: center;
      gap: 30px;
    }
    .feature-card {
    background-color: #d3d3d3 !important; /* Light gray background */
    color: #333333 !important; /* Dark gray text */
    padding: 20px;
    border-radius: 8px;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1); /* Subtle shadow */
    text-align: left;
    width: 30%;
  }
  .feature-card h4 {
    color: #FFA500 !important; /* Orange for headings in feature cards */
    margin-bottom: 10px;
  }
    .about-page h4 {
      color: #FFA500; /* Orange for subheadings in About page */
      font-weight: bold;
    }
    .about-page p, .about-page ul {
      color: white; /* White text for the body in About page */
    }
    .login-register-page {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  height: 80vh; /* Adjust height to vertically center */
}

.input-fields {
  width: 300px; /* Adjust width of the input fields */
  margin-bottom: 15px;
}

.input-fields input {
  width: 100%; /* Ensure the input boxes take the full width of the wrapper */
}

.btn-primary {
  background-color: #FFA500 !important; /* Orange buttons */
  border: none;
  font-size: 1.2em;
  padding: 5px 10px;
  border-radius: 4px;
  text-transform: uppercase;
  color: white;
}

.btn-primary:hover {
  background-color: #CC8400 !important; /* Darker orange on hover */
}
.username-display {
        position: absolute;
        top: 10px;
        right: 10px;
        color: #FFA500;
        font-weight: bold;
        font-size: 1em;
      }

                    "))
  ),
  
  
  # Registration Page
  tabPanel(
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
            actionButton("verify_otp_btn", "Get OTP", class = "btn-primary"),
            textOutput("otp_status")  # Display OTP status (success or failure)
          )
        ),
        
        # Username and Password Section
        textInput("register_user", "Username:", placeholder = "Create your username"),
        passwordInput("register_pass", "Password:", placeholder = "Create your password"),
        actionButton("register_btn", "Register", class = "btn-primary"),
        actionButton("go_to_login", "Back to Login", class = "btn-primary"),
        textOutput("register_status")
      )
    )
  ),
  
  # Login Page
  tabPanel(
    "Login",
    fluidPage(
      useShinyjs(),
      tags$div(
        class = "login-register-page",
        h1("Login", class = "header"),
        textInput("login_user", "Username:", placeholder = "Enter your username"),
        passwordInput("login_pass", "Password:", placeholder = "Enter your password"),
        actionButton("login_btn", "Login", class = "btn-primary"),
        actionButton("go_to_register", "Register", class = "btn-primary"),
        textOutput("login_status")
      )
    )
  ),
  
  
  # Landing Page Tab
  tabPanel(
    "Home",
    fluidPage(
      useShinyjs(),
      tags$div(class = "landing-page",
               # Add top right buttons
               tags$div(
                 class = "btn-corner",
                 actionButton("go_to_login", "Login", class = "btn-primary"),
                 actionButton("go_to_register", "Register", class = "btn-primary")
               ),
               tags$div(id = "username-display", class = "username-display"),
               tags$div(class = "header", "Welcome to Forest Fire Prediction Dashboard"),
               tags$div(class = "subheader", 
                        "An interactive tool to analyze environmental factors and predict fire risks."),
               
               # Existing buttons
               actionButton("go_to_dashboard", "Explore Dashboard", class = "btn-primary"),
               actionButton("go_to_about", "About", class = "btn-primary"),
               # Features Section (remains the same)
               tags$div(class = "features-section",
                        tags$div(class = "feature-card",
                                 h4("Data Visualization"),
                                 p("Create stunning scatter plots, line charts, bar graphs, heatmaps, and treemaps.")
                        ),
                        tags$div(class = "feature-card",
                                 h4("Predictive Modeling"),
                                 p("Use our simple model to predict outcomes based on your data inputs.")
                        ),
                        tags$div(class = "feature-card",
                                 h4("Interactive Insights"),
                                 p("Explore data in an interactive table and visualize correlations.")
                        )
               ),
               
               
      )
    )
  ),
  
  # Dashboard Tab 
  tabPanel("Dashboard",
           sidebarLayout(
             sidebarPanel(
               class = "sidebar",
               h4("Customize the Plot", style = "color: #FFA500; font-weight: bold;"),
               p("Select variables and appearance settings to visualize the data.", 
                 style = "color: #FFA500; font-style: italic;"),
               
               radioButtons("plot_type", "Choose Plot Type:", 
                            choices = c("Scatter Plot", "Line Chart", "Bar Chart", "Heatmap", "Treemap","3D Plot"), 
                            selected = "Scatter Plot"),
               
               selectInput("x_var", "Select X-axis variable:", 
                           choices = names(forestfires), selected = "temp"),
               
               conditionalPanel(
                 condition = "input.plot_type == 'Scatter Plot' || input.plot_type == 'Line Chart' || input.plot_type == 'Heatmap'",
                 selectInput("y_var", "Select Y-axis variable:", 
                             choices = names(forestfires), selected = "rain")
               ),
               
               colourpicker::colourInput("plot_color", "Plot Color:", value = "#FFA500"),
               
               conditionalPanel(
                 condition = "input.plot_type == 'Scatter Plot'",
                 sliderInput("point_size", "Point Size:", 
                             min = 1, max = 5, value = 3)
               ),
               
               checkboxInput("show_data", "Show Data Table", value = TRUE)
             ),
             
             mainPanel(
               tags$div(class = "plot-area", plotOutput("mainPlot"),
                        actionButton("logout_btn", "Logout", class = "btn-primary")
               ),
               conditionalPanel(
                 condition = "input.show_data == true",
                 tags$div(class = "output-box", dataTableOutput("dataTable"))
               )
             )
             
             
           ),
           # Prediction input box (remains the same)
           tags$div(class = "output-box",
                    numericInput("predict_input_home", "Enter X Value for Prediction:", value = 20),
                    actionButton("predict_btn_home", "Predict"),
                    textOutput("predictionResult_home")
           )
  ),
  
  # About Tab
  tabPanel("About",
           fluidPage(
             h3("About This Web Application", style = "color: #FFA500;"),
             p("Welcome to the Forest Fire Prediction Dashboard, an interactive tool designed to help researchers, environmentalists, and anyone interested in understanding and predicting the risk of forest fires based on various environmental variables. This web application combines predictive modeling with dynamic visualizations to assist in exploring, analyzing, and interpreting forest fire-related data."),
             br(),
             h4("What This Web Application Does", style = "color: #FFA500; font-weight: bold;"),
             p("The Forest Fire Prediction Dashboard provides users with the following key features:"),
             tags$ul(
               tags$li("Data Visualization: Create and explore multiple types of plots and charts, including scatter plots, line charts, bar charts, heatmaps, and treemaps."),
               tags$li("Predictive Modeling: The app includes a simple linear regression model to predict forest fire outcomes based on environmental data."),
               tags$li("Interactive Data Exploration: The application offers an interactive data table for users to sort, filter, and explore individual data points."),
               tags$li("Educational Insights: Designed to help users understand the relationship between environmental data and forest fire risk.")
             ),
             br(),
             h4("Purpose of the Web Application", style = "color: #FFA500; font-weight: bold;"),
             p("The Forest Fire Prediction Dashboard was created to achieve the following goals:"),
             tags$ul(
               tags$li("Assist Researchers and Environmentalists: Provide an interactive platform to visualize and predict forest fire risk."),
               tags$li("Predict Forest Fires Using Data: Help prevent or mitigate the impact of wildfires by predicting future fire risks."),
               tags$li("Educate the Public: Raise awareness about the causes of forest fires and how they can be predicted."),
               tags$li("Empower Data-Driven Decision Making: Support data-driven decision-making with insights drawn from environmental data.")
             ),
             br(),
             h4("Developers Behind the Web Application", style = "color: #FFA500; font-weight: bold;"),
             p("The web application was developed by:"),
             tags$ul(
               tags$li("Vicky Swain – Creator of the Web Application"),
               tags$li("Shreyansh Sharma – Creator of the Dashboard")
             ),
             br(),
             h4("Conclusion", style = "color: #FFA500; font-weight: bold;"),
             p("The Forest Fire Prediction Dashboard is a versatile tool that helps predict forest fire risk and educates users about environmental factors that contribute to forest fires. It was developed by Vicky Swain and Shreyansh Sharma to support efforts in forest conservation and fire prevention."),
             br(),
             p("Forest Fire Prediction Dashboard - Created by Shreyansh Sharma and Vicky Swain.", style = "text-align: center; color: #FFA500;")
           )
  )
)

# Define the Server Logic
server <- function(input, output, session) {
  
  # Reactive value to track login status
  is_logged_in <- reactiveVal(FALSE)
  logged_in_user <- reactiveVal("")
  
  # Registration logic
  observeEvent(input$register_btn, {
    username <- input$register_user
    password <- input$register_pass
    
    users <- user_data()
    if (username %in% users$username) {
      output$register_status <- renderText("Username already exists.")
    } else if (username == "" || password == "") {
      output$register_status <- renderText("Please fill in all fields.")
    } else {
      query <- "INSERT INTO users (username, password) VALUES (?, ?)"
      tryCatch({
        dbExecute(db, query, params = list(username, password))
        output$register_status <- renderText("Registration successful! Please log in.")
        updateTabsetPanel(session, "navbar", selected = "Login")
      }, error = function(e) {
        output$register_status <- renderText("Username already exists. Please choose another.")
      })
    }
  })
  
  # Login logic
  observeEvent(input$login_btn, {
    username <- input$login_user
    password <- input$login_pass
    
    query <- "SELECT * FROM users WHERE username = ? AND password = ?"
    user <- dbGetQuery(db, query, params = list(username, password))
    
    if (nrow(user) > 0) {
      is_logged_in(TRUE)
      logged_in_user(username)
      output$login_status <- renderText("")
      updateTabsetPanel(session, "navbar", selected = "Home")
    } else {
      output$login_status <- renderText("Invalid username or password.")
    }
  })
  
  # Logout logic
  observeEvent(input$logout_btn, {
    is_logged_in(FALSE)
    logged_in_user("")  # Clear the username
    shinyjs::html("username-display", "")  # Remove the username display
    updateTabsetPanel(session, "navbar", selected = "Login")
  })
  
  # Observe changes to the selected tab
  observe({
    if (input$navbar == "Dashboard" && !is_logged_in()) {
      showModal(modalDialog(
        tags$div(
          style = "text-align: center; color: red; font-size: 18px; font-weight: bold;",
          tags$h3("Access Denied"),
          p("You must log in or register to access the Dashboard.")
        ),
        easyClose = TRUE,
        footer = NULL
      ))
      updateTabsetPanel(session, "navbar", selected = "Login")  # Redirect to Login
    }
  })
#Otp Logic ->
  library(shiny)
  library(shinyjs)
  library(sendgridr)  # For sending email via SendGrid
  
  # Generate a random OTP function
  generate_otp <- function() {
    return(sample(100000:999999, 1))  # Generate a 6-digit OTP
  }
  
  # Store OTP temporarily (in-memory or use a reactiveValue for session)
  otp_storage <- reactiveVal(NULL)
  
  server <- function(input, output, session) {
    
    # When the "Enter Mail" button is clicked, send OTP
    observeEvent(input$send_otp_btn, {
      email <- input$register_email
      if (email == "") {
        output$otp_status <- renderText("Please enter a valid email address.")
      } else {
        # Generate OTP
        otp <- generate_otp()
        
        # Store OTP temporarily (you could use a more secure solution for production)
        otp_storage(otp)
        
        # Send OTP email using SendGrid or your chosen service
        # Set your SendGrid API Key and from/to email addresses
        sendgrid_api_key <- "YOUR_SENDGRID_API_KEY"
        from_email <- "your-email@example.com"
        to_email <- email
        subject <- "Your OTP Code for Registration"
        body <- paste("Your OTP code is:", otp)
        
        # Send email via SendGrid
        tryCatch({
          send_email(
            api_key = sendgrid_api_key,
            from = from_email,
            to = to_email,
            subject = subject,
            content = body
          )
          output$otp_status <- renderText("OTP has been sent to your email.")
          
          # Show OTP input section
          shinyjs::show("otp_section")
        }, error = function(e) {
          output$otp_status <- renderText("Failed to send OTP. Please try again.")
        })
      }
    })
    
    # When the "Verify OTP" button is clicked, check if OTP is correct
    observeEvent(input$verify_otp_btn, {
      entered_otp <- input$otp_input
      if (entered_otp == "") {
        output$otp_status <- renderText("Please enter the OTP.")
      } else if (as.numeric(entered_otp) == otp_storage()) {
        output$otp_status <- renderText("OTP Verified Successfully!")
        
        # Now, the user can proceed to fill in the rest of the registration form
        # For example, show username and password input fields
        shinyjs::show("register_user")
        shinyjs::show("register_pass")
        
        # Disable OTP section after successful verification
        shinyjs::hide("otp_section")
      } else {
        output$otp_status <- renderText("Invalid OTP. Please try again.")
      }
    })
    
    # Registration logic (after OTP verification)
    observeEvent(input$register_btn, {
      username <- input$register_user
      password <- input$register_pass
      
      if (username == "" || password == "") {
        output$register_status <- renderText("Please fill in all fields.")
      } else {
        # Here you can proceed with saving the user details to the database or any other storage
        # For example, using SQLite or another database
        # Add your database logic here for storing the user
        
        output$register_status <- renderText("Registration successful!")
      }
    })
    
    # Navigation between Register and Login
    observeEvent(input$go_to_login, {
      updateTabsetPanel(session, "navbar", selected = "Login")
    })
  }
  
  
  
  # Navigation between Login and Registration
  observeEvent(input$go_to_register, {
    updateTabsetPanel(session, "navbar", selected = "Register")
  })
  
  observeEvent(input$go_to_login, {
    updateTabsetPanel(session, "navbar", selected = "Login")
  })
  
  # Home page prediction logic
  observeEvent(input$predict_btn_home, {
    prediction_value <- input$predict_input_home * 2.5  # Example prediction logic
    output$predictionResult_home <- renderText({
      paste("Predicted value: ", prediction_value)
    })
  })
  
  # Switch to Dashboard tab when the Explore Dashboard button is clicked
  observeEvent(input$go_to_dashboard, {
    updateTabsetPanel(session, "navbar", selected = "Dashboard")
  })
  
  # Switch to About tab when the About button is clicked
  observeEvent(input$go_to_about, {
    updateTabsetPanel(session, "navbar", selected = "About")
  })
  
  # Dashboard plot rendering based on user input
  output$mainPlot <- renderPlot({
    if (input$plot_type == "Scatter Plot") {
      ggplot(forestfires, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(color = input$plot_color, size = input$point_size) +
        theme_minimal() +
        ggtitle("Scatter Plot")
    } else if (input$plot_type == "Line Chart") {
      ggplot(forestfires, aes_string(x = input$x_var, y = input$y_var)) +
        geom_line(color = input$plot_color, size = input$point_size) +
        theme_minimal() +
        ggtitle("Line Chart")
    } else if (input$plot_type == "Bar Chart") {
      ggplot(forestfires, aes_string(x = input$x_var, y = input$y_var)) +
        geom_bar(stat = "identity", fill = input$plot_color) +
        theme_minimal() +
        ggtitle("Bar Chart")
    } else if (input$plot_type == "Heatmap") {
      ggplot(forestfires, aes_string(x = input$x_var, y = input$y_var)) +
        geom_tile(aes(fill = ..count..)) +
        scale_fill_gradient(low = "white", high = input$plot_color) +
        theme_minimal() +
        ggtitle("Heatmap")
    } else if (input$plot_type == "Treemap") {
      # Example of a simple treemap (you might need additional libraries to handle it fully)
      treemap_data <- data.frame(
        category = sample(c("A", "B", "C", "D"), 100, replace = TRUE),
        value = rnorm(100)
      )
      treemap(treemap_data, index = c("category"), vSize = "value", vColor = "value", draw = TRUE)
    }
    else if (input$plot_type == "3D Plot") {
      # 3D Plot using plotly
      plot_ly(forestfires, x = ~temp, y = ~rain, z = ~wind, type = "scatter3d", mode = "markers",
              marker = list(color = input$plot_color, size = input$point_size)) %>%
        layout(scene = list(xaxis = list(title = input$x_var),
                            yaxis = list(title = input$y_var),
                            zaxis = list(title = "Wind")))
    }
  })
  
  # Data table rendering
  output$dataTable <- renderDataTable({
    forestfires
  })
}

# Run the Application
shinyApp(ui = ui, server = server)