#' SD theta
#'
#' Obtain standard deviation of treatment effect estimates
#' @param results_list list of results
#' @return SD theta
#' @export

get_sd <- function(results_list) {
  
  theta.list <- results_list[c("crude.theta", "match.theta", "adjusted.theta",
                               "adjusted.ps.theta", "match.adjusted.theta", 
                               "stratified.theta", "stratified.adjusted.theta",
                               "weight.theta", "weight.adjusted.theta")]
  
  sd <- unlist(lapply(theta.list, FUN = sd))
  
  return(sd)
}