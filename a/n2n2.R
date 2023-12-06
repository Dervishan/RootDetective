newton.raphson <- function(f, a, b, tol = 1e-5, n = 1000, true_root = NULL) {
  require(numDeriv) # Package for computing f'(x)
  
  start_time <- Sys.time() # Start timing
  
  x0 <- a # Set start value to supplied lower bound
  iterations <- numeric(n) # Initialize for iteration results
  
  # Check the upper and lower bounds to see if approximations result in 0
  fa <- f(a)
  if (fa == 0.0) {
    return(list(root = a, speed = NA, error = NA, efficiency = NA, reliability = NA))
  }
  
  fb <- f(b)
  if (fb == 0.0) {
    return(list(root = b, speed = NA, error = NA, efficiency = NA, reliability = NA))
  }
  
  for (i in 1:n) {
    dx <- genD(func = f, x = x0)$D[1] # First-order derivative f'(x0)
    if (dx == 0) break # Prevent division by zero
    x1 <- x0 - (f(x0) / dx) # Calculate next value x1
    iterations[i] <- x1 # Store x1
    
    # Once the difference between x0 and x1 becomes sufficiently small, output the results.
    if (abs(x1 - x0) < tol) {
      end_time <- Sys.time() # End timing
      root.approx <- x1
      
      # Calculate metrics
      speed <- end_time - start_time
      error <- if (!is.null(true_root)) abs(root.approx - true_root) else NA
      efficiency <- 1 / i # Example: efficiency could be inversely related to the number of iterations
      reliability <- if (!is.null(true_root)) 1 - error else NA # Example: reliability could be inversely related to error
      
      return(list(root = root.approx, speed = as.numeric(speed), error = error, efficiency = efficiency, reliability = reliability))
    }
    # If Newton-Raphson has not yet reached convergence set x1 as x0 and continue
    x0 <- x1
  }
  
  print('Too many iterations in method')
  return(list(root = NA, speed = NA, error = NA, efficiency = NA, reliability = NA))
}
