#' Generate Monte Carlo simulation
#'
#' Generate 10,000 simulated data set, fit survival models and return estimates
#' @param num.iter number of iterations 
#' @return Monte Carlo simulation estimates
#' @export

generate_mc <- function(num.iter = 10000) {
  results = matrix(NA, num.iter, 22) # iterations x estimates
  for(iter in 1:num.iter){
    set.seed(iter) 
    o.data <- simulate_data(beta0 = -3.45)
    results[iter, ] <- unlist(fit_models(o.data))
  }
  
  results <- as.data.frame(results)
  return(results)
} 