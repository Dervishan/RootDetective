library(shiny)
library(ggplot2)
library(DT)

source('newtonraphson2.r')
source('bisection.r')
source('secant.r')

# Define server logic required to calculate roots
shinyServer(function(input, output, session) {
  
  # Reactive values to store the results
  results <- reactiveVal(list())
  
  # Function to handle math input button clicks
  observeEvent(input$btn_pi, {
    updateTextInput(session, "function_input", value = paste0(input$function_input, "pi"))
  })
  observeEvent(input$btn_sqrt, {
    updateTextInput(session, "function_input", value = paste0(input$function_input, "sqrt()"))
  })
  observeEvent(input$btn_deg, {
    updateTextInput(session, "function_input", value = paste0(input$function_input, "deg()"))
  })
  observeEvent(input$btn_sin, {
    updateTextInput(session, "function_input", value = paste0(input$function_input, "sin()"))
  })
  observeEvent(input$btn_cos, {
    updateTextInput(session, "function_input", value = paste0(input$function_input, "cos()"))
  })
  observeEvent(input$btn_tan, {
    updateTextInput(session, "function_input", value = paste0(input$function_input, "tan()"))
  })
  observeEvent(input$btn_ln, {
    updateTextInput(session, "function_input", value = paste0(input$function_input, "ln()"))
  })
  observeEvent(input$btn_log, {
    updateTextInput(session, "function_input", value = paste0(input$function_input, "log()"))
  })
  
  # Reactive event for the submit button
  observeEvent(input$submit, {
    # Validate the function input
    if (input$function_input == "") {
      return(showNotification("Please enter a function", type = "error"))
    }
    
    # Safe evaluation of the user's function
    safeEval <- function(str) {
      tryCatch(eval(parse(text = str)), error = function(e) NA)
    }
    func <- safeEval(paste0("function(x) {", input$function_input, "}"))
    
    # Check if the function is valid
    if (is.function(func)) {
      # Calculate roots using all three methods
      results(list(
        bisection = bisection(func, input$lower_bound, input$upper_bound),
        newtonraphson = newtonraphson(func, (input$lower_bound + input$upper_bound) / 2), # Assuming average as initial guess
        secant = secant(func, input$lower_bound, input$upper_bound)
      ))
    } else {
      showNotification("Invalid function", type = "error")
    }
  }, ignoreInit = TRUE)
  
  # Render plot output
  output$plot <- renderPlot({
    result_data <- results()
    if (is.null(result_data)) return()
    
    
    # Define the range for the x-axis
    x_range <- seq(input$lower_bound - 1, input$upper_bound + 1, length.out = 1000)
    
    # Evaluate the function over the range
    y_values <- sapply(x_range, function(x) eval(parse(text = paste0("(", input$function_input, ")", sep = ""))))
    
    
    # Create a data frame for ggplot
    plot_data <- data.frame(x = x_range, y = y_values)
    
    # Create the ggplot
    ggplot(plot_data, aes(x, y)) +
      geom_line() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point(aes(x = result_data$bisection, y = 0), color = "blue", size = 4) +
      geom_point(aes(x = result_data$newtonraphson, y = 0), color = "green", size = 4) +
      geom_point(aes(x = result_data$secant, y = 0), color = "red", size = 4) +
      labs(title = "Function Plot and Identified Roots",
           subtitle = paste("Bisection (Blue), Newton-Raphson (Green), Secant (Red)"),
           x = "x",
           y = "f(x)") +
      theme_minimal()
  })
  
  # Render method comparison table
  output$method_comparison <- renderDataTable({
    result_data <- results()
    
    pad_with_empty_string <- function(x, max_length) {
      length(x) <- max_length
      x[is.na(x)] <- "" # Replace NA with empty string
      return(x)
    }
    
    # Example: Assuming result_data is a list of lists with each method's results
    if (is.null(result_data)) return()
    
    # Find the maximum length across all vectors
    max_length <- max(sapply(result_data, function(x) length(unlist(x))))
    
    # Create a data frame with equal-length columns, filling with empty strings as needed
    method_comparison <- data.frame(
      Method = pad_with_empty_string(c("Bisection", "Newton-Raphson", "Secant"), max_length),
      Root = pad_with_empty_string(c(result_data$bisection$root, result_data$newtonraphson$root, result_data$secant$root), max_length),
      Speed = pad_with_empty_string(c(result_data$bisection$speed, result_data$newtonraphson$speed, result_data$secant$speed), max_length),
      Error = pad_with_empty_string(c(result_data$bisection$error, result_data$newtonraphson$error, result_data$secant$error), max_length),
      Efficiency = pad_with_empty_string(c(result_data$bisection$efficiency, result_data$newtonraphson$efficiency, result_data$secant$efficiency), max_length),
      Reliability = pad_with_empty_string(c(result_data$bisection$reliability, result_data$newtonraphson$reliability, result_data$secant$reliability), max_length)
    )
    
    
    # Display the data frame as a table
    datatable(method_comparison)
  })
  
})
