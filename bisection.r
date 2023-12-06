bisection <- function(ftn, x.l, x.r, tol = 1e-9){
  start_time <- Sys.time() # Start timing
  f.l <- ftn(x.l)
  f.r <- ftn(x.r)
  if (f.l == 0) {
    end_time <- Sys.time() # End timing
    execution_time <- end_time - start_time # Calculate execution time
    return(data.frame(
      root = x.l, 
      time = execution_time, 
      iter = 0))
  } else if (f.r == 0) {
    end_time <- Sys.time() # End timing
    execution_time <- end_time - start_time # Calculate execution time
    return(data.frame(
      root = x.r, 
      time = execution_time, 
      iter = 0))
  } else if (f.l * f.r > 0) {
    cat("error: ftn(x.l) * ftn(x.r) > 0 \n")
    return(NULL)
  }
  n <- 0 # successively refine x.l and x.r
  while ((x.r - x.l) > tol){
    x.m <- (x.l + x.r)/2
    f.m <- ftn(x.m)
    if (f.m == 0) {
      end_time <- Sys.time() # End timing
      execution_time <- end_time - start_time # Calculate execution time
      return(data.frame(
        root = x.m, 
        time = execution_time, 
        iter = n))
    } else if (f.l * f.m < 0) {
      x.r <- x.m
      f.r <- f.m
    } else {
      x.l <- x.m
      f.l <- f.m
    }
    n <- n + 1
  }
  
  end_time <- Sys.time() # End timing
  execution_time <- end_time - start_time # Calculate execution time
  return(data.frame(
    root = (x.l + x.r)/2, 
    time = execution_time, 
    iter = n)) # return (approximate) root
}