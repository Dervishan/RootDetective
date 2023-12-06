secant <- function(f, x0, x1, tol = 1e-9, n = 500) {
  start_time <- Sys.time() # Start timing
  for (i in 1:n) {
    x2 <- x1 - f(x1) / ((f(x1) - f(x0)) / (x1 - x0)) # Calculate the new x value
    if (abs(x2 - x1) < tol) { # If the difference between the new value and the previous value is small enough, end iteration and output root.
      end_time <- Sys.time() # End timing
      execution_time <- end_time - start_time # Calculate execution time
      return(data.frame(
        root = x2, 
        time = execution_time, 
        iter = i))
    }
    # If the root was not determined in the previous iteration, update the values and proceed to the next iteration.
    x0 <- x1
    x1 <- x2 
  }
}