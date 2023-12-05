library(shiny)

# Define UI for application that calculates roots
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Root Detective"),
  
  # Instructions or Information Modal
  modalDialog(
    title = "Welcome to Root Detective!",
    "This app helps you compare roots of functions using Bisection, Newton-Raphson, and Secant methods. Input your function and define the bounds to see how each method performs.",
    easyClose = TRUE,
    footer = NULL
  ),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Lower and Upper Bound Input
      numericInput("lower_bound", "Lower Bound:", value = 2),
      numericInput("upper_bound", "Upper Bound:", value = 3),
      
      # Text input for the function
      textInput("function_input", "Function:", value = "x^2 - 4"),
      
      # Math input buttons
      actionButton("btn_pi", "π", title = "Insert Pi"),
      actionButton("btn_sqrt", "√", title = "Square Root Function"),
      actionButton("btn_sin", "sin", title = "Sine Function"),
      actionButton("btn_cos", "cos", title = "Cosine Function"),
      actionButton("btn_tan", "tan", title = "Tangent Function"),
      actionButton("btn_ln", "ln", title = "Natural Log Function"),
      actionButton("btn_log", "log", title = "Logarithm Function"),
      br(),
      actionButton("submit", "Calculate Roots")
    ),
    
    # Main panel for displaying results
    mainPanel(
      # Output: Plot from ggplot2
      plotOutput("plot"),
      
      # Output for Comparing Methods
      h5("Method Comparison:"),
      dataTableOutput("method_comparison")
    )
  )
))
