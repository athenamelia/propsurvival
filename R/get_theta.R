#' Mean theta
#'
#' Obtain mean of treatment effect estimates
#' @param results_list list of results
#' @return mean theta
#' @export

get_theta <- function(results_list) {
  
  theta.list <- results_list[c("crude.theta", "match.theta", "adjusted.theta",
                     "adjusted.ps.theta", "match.adjusted.theta", 
                     "stratified.theta", "stratified.adjusted.theta",
                     "weight.theta", "weight.adjusted.theta")]
  
  theta <- unlist(lapply(theta.list, FUN = mean))
  
  return(theta)
}