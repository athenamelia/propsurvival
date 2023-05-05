#' Generate Monte Carlo simulation
#'
#' Generate 10,000 simulated data set, fit survival models and return estimates
#' @param num.iter number of iterations
#' @param beta0 for treatment effect generation; determines imbalance level
#' @param cores the number of cores for parallelization
#' @return Monte Carlo simulation estimates
#' @export

generate_mc <- function(num.iter = 10000, beta0 = -3.45, cores = 1) {
  results = matrix(NA, num.iter, 22) # iterations x estimates
  for(iter in 1:num.iter){
    set.seed(iter) 
    o.data <- simulate_data(beta0 = beta0)
    results[iter, ] <- unlist(fit_models(o.data))
  }
  parallel::mclapply(1:num.iter, simulate_data, mc.cores = cores)
  
  results <- as.data.frame(results)
  return(results)
} 


