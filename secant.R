secant <- function(f, x0, x1, tol = 1e-9, n = 500, true_root = NULL) {
  start_time <- Sys.time() # Start timing
  
  if (is.function(f) == FALSE) {
    stop('f is not a function')
  }
  
  for (i in 1:n) {
    denominator <- (f(x1) - f(x0))
    if (denominator == 0) {
      return(list(root = NA, speed = NA, error = NA, efficiency = NA, reliability = NA)) # Prevent division by zero
    }
    
    x2 <- x1 - f(x1) * (x1 - x0) / denominator # Calculate the new x value
    
    if (abs(x2 - x1) < tol) { # Check for convergence
      end_time <- Sys.time() # End timing
      
      # Calculate metrics
      speed <- end_time - start_time
      error <- if (!is.null(true_root)) abs(x2 - true_root) else NA
      efficiency <- 1 / i # Efficiency could be inversely related to the number of iterations
      reliability <- if (!is.null(true_root)) 1 - error else NA # Reliability could be inversely related to error
      
      return(list(root = x2, speed = as.numeric(speed), error = error, efficiency = efficiency, reliability = reliability))
    }
    
    # Update for the next iteration
    x0 <- x1
    x1 <- x2 
  }
  
  # If the method fails to converge within n iterations
  print('Too many iterations in method')
  return(list(root = NA, speed = NA, error = NA, efficiency = NA, reliability = NA))
}
