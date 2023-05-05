#' Bias of theta
#'
#' Obtain bias of treatment effect estimates
#' @param results_list list of results
#' @param true_theta true value of theta for evaluation
#' @return theta bias
#' @export

get_bias <- function(results_list, true_theta = log(1)) {
  
  bias <- get_theta(results_list) - true_theta
  
  return(round(bias, 3))
}


