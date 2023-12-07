library(shiny)

# Define UI for application that calculates roots
shinyUI(fluidPage(
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  
  # Include some custom CSS for styling the buttons
  tags$head(
    tags$style(HTML("
      .math-button-row {
        display: flex;
        justify-content: space-between;
        margin-bottom: 5px;
      }
      .math-button {
        flex-grow: 1;
        margin-right: 5px;
      }
      .math-button:last-child {
        margin-right: 0;
      }
      .calc_button {
        display: flex;
        justify-content: center;
      }
    "))
  ),
  
  # Application title
  titlePanel("Root Detective"),
  
  # Instructions or Information Modal
  modalDialog(
    title = "Welcome to Root Detective!",
    "This app helps you find roots of functions using various methods. Select a method and input the required parameters.",
    easyClose = TRUE,
    footer = NULL
  ),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Dropdown for selecting the method
      selectInput("method_select", "Select Method:", 
                  choices = c("Bisection", "Newton-Raphson", "Secant", "Uniroot")),
      
      # Text input for the function
      textInput("function_input", "Function:", value = "x^2 - 4"),
      
      # Conditional Panels for method-specific inputs
      conditionalPanel(
        condition = "input.method_select == 'Bisection' || input.method_select == 'Uniroot'",
        numericInput("lower_bound", "Lower Bound:", value = 2),
        numericInput("upper_bound", "Upper Bound:", value = 3)
      ),
      conditionalPanel(
        condition = "input.method_select == 'Newton-Raphson'",
        numericInput("initial_guess", "Initial Guess:", value = 2.5)
      ),
      conditionalPanel(
        condition = "input.method_select == 'Secant'",
        numericInput("x0", "x0:", value = 2),
        numericInput("x1", "x1:", value = 3)
      ),
      
      # Common inputs for all methods
      conditionalPanel(
        condition = "input.method_select == 'Newton-Raphson' || input.method_select == 'Secant' || input.method_select == 'Uniroot'",
        numericInput("tolerance", "Tolerance:", value = 1e-6),
        numericInput("iteration_count", "Max Iterations:", value = 100),
      ),
      # Math input buttons
      div(class = "math-buttons",
          # Math input buttons styled in rows
          div(class = "math-button-row",
              actionButton("btn_plus", "+", class = "math-button", title = "Add"),
              actionButton("btn_sub", "-", class = "math-button", title = "Subtract"),
              actionButton("btn_x", "x", class = "math-button", title = "Multiply"),
              actionButton("btn_div", "÷", class = "math-button", title = "Divide")
          ),
          div(class = "math-button-row",
              actionButton("btn_sin", "sin", class = "math-button", title = "Sine Function"),
              actionButton("btn_cos", "cos", class = "math-button", title = "Cosine Function"),
              actionButton("btn_tan", "tan", class = "math-button", title = "Tangent Function"),
              actionButton("btn_ln", "ln", class = "math-button", title = "Natural Log Function")
          ),
          div(class = "math-button-row",
              actionButton("btn_pi", "π", class = "math-button", title = "Insert Pi"),
              actionButton("btn_power", "^", class = "math-button", title = "Power"),
              actionButton("btn_sqrt", "√", class = "math-button", title = "Square Root Function"),
              actionButton("clear_input", "Clear", class = "math-button", title = "Clear Input")
          ),
      ),
      br(),
      
      # Calculate Button
      # actionButton("calculate", "Calculate")
      # Dynamic UI for Calculate Button
      div(class = "calc_button",
        uiOutput("dynamic_calculate_button")
        )
    ),
    
    # Main panel for displaying results
    mainPanel(
      tabsetPanel(id = "method_tabs",
        # Tab for each method
        tabPanel("Newton-Raphson", plotOutput("plot_newton"),"The root of f(x)", textOutput("root_newton"), dataTableOutput("table_newton"), value = "newton_tab"),
        tabPanel("Bisection", plotOutput("plot_bisection"),"The root of f(x)", textOutput("root_bisection"), dataTableOutput("table_bisection"), value = "bisection_tab"),
        tabPanel("Secant", plotOutput("plot_secant"),"The root of f(x)", textOutput("root_secant"), dataTableOutput("table_secant"), value = "secant_tab"),
        tabPanel("Uniroot", plotOutput("plot_uniroot"),"The root of f(x)", textOutput("root_uniroot"), dataTableOutput("table_uniroot"), value = "uniroot_tab"),
        
        # Tab for comparing methods
        tabPanel("Comparison", DT::dataTableOutput("comparisonTable"))
      )
    )
  )
))
