library(shiny)
library(ggplot2)

source('fixedpoint.r')
source('newtonraphson.r')
source('bisection.r')

# Define server logic required to calculate roots
shinyServer(function(input, output, session) {
  
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
    # Check if the function input is empty
    if (input$function_input == "") {
      return()
    }
    
    # Parse the user's function safely
    safeEval <- function(str) {
      tryCatch(eval(parse(text = str)), error = function(e) NA)
    }
    
    # Define the function from input
    func <- safeEval(paste0("function(x) {", input$function_input, "}"))
    
    if (is.na(func)) {
      # Could display an error message to the user here
      return()
    }
    
    # Perform the calculation based on the selected algorithm
    result <- switch(input$algorithm,
                     fixedpoint = fixedpoint(func, as.numeric(input$initial_guess)),
                     newtonraphson = newtonraphson(func, as.numeric(input$initial_guess)),
                     bisection = bisection(func, as.numeric(input$lower_bound), as.numeric(input$upper_bound))
                     # ... other cases
    )
    
    # Output plot based on the selected algorithm and inputs
    output$plot <- renderPlot({
      if (is.null(result)) {
        # Return an error plot or message
        return()
      }
      
      # Create a plot of the function and the found root
      plot_data <- data.frame(x = seq(min(c(input$lower_bound, input$initial_guess, input$upper_bound)) - 1,
                                      max(c(input$lower_bound, input$initial_guess, input$upper_bound)) + 1, by = 0.1))
      plot_data$y <- sapply(plot_data$x, func)
      
      ggplot(plot_data, aes(x, y)) +
        geom_line() +
        geom_point(aes(x = result, y = 0), color = "red") +
        labs(title = "Function Plot and Root",
             x = "x",
             y = "f(x)") +
        theme_minimal()
    })
  }, ignoreInit = TRUE) # ignoreInit = TRUE prevents the event from firing on app launch
  
})
