# program spuRs/resources/scripts/newtonraphson.r
# loadable spuRs function

newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
  start_time <- Sys.time() # Start timing
  # Newton_Raphson algorithm for solving ftn(x)[1] == 0
  # we assume that ftn is a function of a single variable that returns
  # the function value and the first derivative as a vector of length 2
  #
  # x0 is the initial guess at the root
  # the algorithm terminates when the function value is within distance
  # tol of 0, or the number of iterations exceeds max.iter
  # initialise
  x <- x0
  fx <- ftn(x)
  dx <- (ftn(x + 1e-5) - ftn(x - 1e-5)) / (2 * 1e-5)

  iter <-  0
  
  # continue iterating until stopping conditions are met
  while ((abs(fx) > tol) && (iter < max.iter)) {
    x <- x - fx/dx
    fx <- ftn(x)
    iter <- iter + 1
  }
  end_time <- Sys.time() # End timing
  execution_time <- end_time - start_time # Calculate execution time
  
  # output depends on success of algorithm
  if (abs(fx) > tol) {
    cat("Algorithm failed to converge\n")
    return(data.frame(
      root = NULL, 
      time = execution_time, 
      iter = iter))
  } else {
    cat("Algorithm converged\n")
    return(data.frame(
      root = x, 
      time = execution_time, 
      iter = iter))
  }
}
