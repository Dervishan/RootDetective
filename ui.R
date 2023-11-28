library(shiny)

# Define UI for application that calculates roots
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Root Detective"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Algorithm selection input
      selectInput("algorithm", "Choose the algorithm:",
                  choices = c("Fixed-Point" = "fixedpoint",
                              "Newton-Raphson" = "newtonraphson",
                              "Bisection" = "bisection")),
      
      # Conditional panel for Fixed-Point and Newton-Raphson
      conditionalPanel(
        condition = "input.algorithm != 'bisection'",
        numericInput("initial_guess", "Initial Guess:", value = 1)
      ),
      
      # Conditional panel for Bisection
      conditionalPanel(
        condition = "input.algorithm == 'bisection'",
        numericInput("lower_bound", "Lower Bound:", value = 1),
        numericInput("upper_bound", "Upper Bound:", value = 2)
      ),
      
      # Text input for the function
      textInput("function_input", "Function:", value = "x^2 - 4"),
      
      # Math input buttons
      actionButton("btn_pi", "π"),
      actionButton("btn_sqrt", "√"),
      actionButton("btn_deg", "deg"),
      actionButton("btn_sin", "sin"),
      actionButton("btn_cos", "cos"),
      actionButton("btn_tan", "tan"),
      actionButton("btn_ln", "ln"),
      actionButton("btn_log", "log"),
      br(),
      actionButton("submit", "Calculate Root"),
    ),
    
    
    # Main panel for displaying results
    mainPanel(
      # Output: Plot from ggplot2
      plotOutput("plot")
    )
  )
))
