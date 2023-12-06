library(shiny)
library(ggplot2)
library(DT)
library('spuRs')

source('secant.R')
source('bisection.R')
source('newtonraphson.r')

# Define server logic required to calculate roots
shinyServer(function(input, output, session) {
  
  # Reactive values to store the results
  results <- reactiveValues()
  results$algorithmData <- data.frame(Method = character(), 
                                      Input1 = character(), 
                                      Input2 = character(), 
                                      ExecutionTime = numeric(), 
                                      Iterations = integer(), 
                                      stringsAsFactors = FALSE)
  
  # Function to handle math input button clicks
  observeEvent(input$btn_plus, {
    updateTextInput(session, "function_input", value = paste0(input$function_input, "+"))
  })
  observeEvent(input$btn_sub, {
    updateTextInput(session, "function_input", value = paste0(input$function_input, "-"))
  })
  observeEvent(input$btn_x, {
    updateTextInput(session, "function_input", value = paste0(input$function_input, "*"))
  })
  observeEvent(input$btn_div, {
    updateTextInput(session, "function_input", value = paste0(input$function_input, "/"))
  })
  observeEvent(input$btn_pi, {
    updateTextInput(session, "function_input", value = paste0(input$function_input, "pi"))
  })
  observeEvent(input$btn_power, {
    updateTextInput(session, "function_input", value = paste0(input$function_input, "^"))
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
  observeEvent(input$clear_input, {
    updateTextInput(session, "function_input", value = "")
  })
  
  # Dynamic UI for Calculate Button
  output$dynamic_calculate_button <- renderUI({
    switch(input$method_select,
           "Bisection" = actionButton("calculate_bisection", "Calculate Bisection"),
           "Newton-Raphson" = actionButton("calculate_newton", "Calculate Newton-Raphson"),
           "Secant" = actionButton("calculate_secant", "Calculate Secant"),
           "Uniroot" = actionButton("calculate_uniroot", "Calculate Uniroot")
    )
  })
  
  # Reactive event for Bisection method button
  observeEvent(input$calculate_bisection, {
    # Validate the function input
    if (!isTruthy(input$function_input)) {
      return(showNotification("Please enter a function for Bisection", type = "error"))
    }
    if (input$lower_bound >= input$upper_bound) {
      return(showNotification("Lower bound cannot be higher than upper bound", type = "error"))
    }
    # [Function preparation and evaluation logic for Bisection]
    results$Bisection <- bisectionEval(
      input$function_input, 
      input$lower_bound, 
      input$upper_bound, 
      input$tolerance
    )
    
    results$algorithmData <- rbind(results$algorithmData, 
                                   data.frame(Method = "Bisection", 
                                              Input1 = paste(input$lower_bound),
                                              Input2 = paste(input$upper_bound), 
                                              ExecutionTime = results$Bisection$time, 
                                              Iterations = results$Bisection$iter))
    
    # Switch to the Bisection tab
    updateTabsetPanel(session, "method_tabs", selected = "bisection_tab")
    
  })
  
  # Reactive event for Newton-Raphson method button
  observeEvent(input$calculate_newton, {
    # Validate the function input
    if (!isTruthy(input$function_input)) {
      return(showNotification("Please enter a function for Newton-Raphson", type = "error"))
    }
    # [Function preparation and evaluation logic for Newton-Raphson]
    results$NewtonRaphson <- newtonRaphsonEval(
      input$function_input, 
      input$initial_guess,
      input$tolerance, 
      input$iteration_count
    )
    
    results$algorithmData <- rbind(results$algorithmData, 
                                   data.frame(Method = "Newton-Raphson", 
                                              Input1 = paste(input$initial_guess), 
                                              Input2 = paste(""), 
                                              ExecutionTime = results$NewtonRaphson$time, 
                                              Iterations = results$NewtonRaphson$iter))
    
    # Switch to the Newton-Raphson tab
    updateTabsetPanel(session, "method_tabs", selected = "newton_tab")
    
  })
  
  # Reactive event for Secant method button
  observeEvent(input$calculate_secant, {
    # Validate the function input
    if (!isTruthy(input$function_input)) {
      return(showNotification("Please enter a function for Secant", type = "error"))
    }
    
    # [Function preparation and evaluation logic for Secant]
    results$Secant <- secantEval(
      input$function_input, 
      input$x0, input$x1,
      input$tolerance, 
      input$iteration_count
    )
    
    results$algorithmData <- rbind(results$algorithmData, 
                                   data.frame(Method = "Secant", 
                                              Input1 = paste(input$x0), 
                                              Input2 = paste(input$x1), 
                                              ExecutionTime = results$Secant$time, 
                                              Iterations = results$Secant$iter))
    
    # Switch to the Secant tab
    updateTabsetPanel(session, "method_tabs", selected = "secant_tab")
  })
  
  # Reactive event for Uniroot method button
  observeEvent(input$calculate_uniroot, {
    # Validate the function input
    if (!isTruthy(input$function_input)) {
      return(showNotification("Please enter a function for Uniroot", type = "error"))
    }
    
    start_time <- Sys.time() # Start timing
    # [Function preparation and evaluation logic for Uniroot]
    results$Uniroot <- unirootEval(
      input$function_input, 
      input$lower_bound, 
      input$upper_bound, 
      input$tolerance, 
      input$iteration_count
    )
    
    end_time <- Sys.time() # End timing
    execution_time <- end_time - start_time # Calculate execution time
    
    results$algorithmData <- rbind(results$algorithmData, 
                                   data.frame(Method = "Uniroot", 
                                              Input1 = paste(input$lower_bound),
                                              Input2 = paste(input$upper_bound), 
                                              ExecutionTime = execution_time, 
                                              Iterations = results$Uniroot$iter))
    # Switch to the Uniroot tab
    updateTabsetPanel(session, "method_tabs", selected = "uniroot_tab")
    
  })
  
  # Function to safely evaluate user-defined function
  safeEval <- function(str) {
    tryCatch(eval(parse(text = str)), error = function(e) NA)
  }
  
  # Functions for each method
  bisectionEval <- function(funcStr, lower, upper, tol) {
    func <- safeEval(paste0("function(x) {", funcStr, "}"))
    if (func(lower)*func(upper>0)) {
      showNotification("f(xl)f(xr)>0", type = "error")
    }
    if (is.function(func)) {
      return(bisection(func, lower, upper, tol))
    } else {
      return(NA)
    }
  }
  
  newtonRaphsonEval <- function(funcStr, initial, tol, maxIter) {
    # Create an expression from the user's input
    #funcExpr <- parse(text = funcStr)
    # Use D() to get the derivative expression and convert it to a function
    #derivExpr <- D(funcExpr, "x")
    # Convert the derivative expression to a string
    #f_primevar <- function(x) eval(derivExpr, envir = list(x = x))
    
    func <- safeEval(paste0("function(x) {", funcStr, "}"))
    #f_prime <- safeEval(paste0("function(x) {", f_primevar, "}"))
    if (is.function(func)) {
      return(newtonraphson(func, initial, tol, maxIter))
    } else {
      return(NA)
    }
  }
  
  secantEval <- function(funcStr, x0, x1, tol, maxIter) {
    func <- safeEval(paste0("function(x) {", funcStr, "}"))
    if (is.function(func)) {
      return(secant(func, x0, x1, tol, maxIter))
    } else {
      return(NA)
    }
  }
  
  unirootEval <- function(funcStr, lower, upper, tol, maxIter) {
    func <- safeEval(paste0("function(x) {", funcStr, "}"))
    if (is.function(func)) {
      result <- tryCatch(uniroot(func, c(lower, upper), tol = tol, maxiter = maxIter), error = function(e) list(root = NA, message = e$message))
      return(result)
    } else {
      return(NA)
    }
  }
  
  # Function to create a plot for a given method
  createMethodPlot <- function(methodResult, methodName) {
    
    userFunc <- safeEval(paste0("function(x) {", input$function_input, "}"))
    
    # Generate values for plotting
    x_vals <- seq(input$lower_bound, input$upper_bound, length.out = 100)
    y_vals <- sapply(x_vals, userFunc)
    
    # Create the ggplot
    p <- ggplot(data.frame(x = x_vals, y = y_vals), aes(x, y)) +
      geom_line() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = paste0(methodName, " Method"), subtitle = input$function_input, 
           x = "x", y = "f(x)", caption = paste("tol =", input$tolerance)) +
      theme_minimal()
    
    # Add root line if valid
    if (!is.na(methodResult$root)) {
      p <- p + geom_vline(xintercept = methodResult$root, linetype = "dotted", color = "blue")
    }
    
    return(p)
  }
  
  
  # Render plot for Bisection method
  output$plot_bisection <- renderPlot({
    if(isTruthy(results$Bisection)){
      createMethodPlot(results$Bisection, "Bisection")
    }
  })
  
  # Render plot for Newton-Raphson method
  output$plot_newton <- renderPlot({
    if(isTruthy(results$NewtonRaphson)){
      createMethodPlot(results$NewtonRaphson, "Newton-Raphson")
    }
  })
  
  # Render plot for Secant method
  output$plot_secant <- renderPlot({
    if(isTruthy(results$Secant)){
      createMethodPlot(results$Secant, "Secant")
    }
  })
  
  # Render plot for Uniroot method
  output$plot_uniroot <- renderPlot({
    if(isTruthy(results$Uniroot)){
      createMethodPlot(results$Uniroot, "Uniroot")
    }
  })
  
  
  
  # Render method comparison table
  output$comparisonTable <- renderDataTable({
    print(results$algorithmData)
    results$algorithmData
  })
  
  # [Any additional server logic]
})
