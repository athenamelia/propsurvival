#' Bias of theta
#'
#' Obtain bias of treatment effect estimates
#' @param results_list list of results
#' @return theta bias
#' @export

get_se <- function(results_list, true_theta = log(1)) {
  
  bias <- get_theta(results_list) - true_theta
  
  return(bias)
}


