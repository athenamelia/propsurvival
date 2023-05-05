#' MSE of theta
#'
#' Obtain MSE treatment effect estimates
#' @param results_list list of results
#' @param true_theta true value of theta for evaluation
#' @return theta MSE
#' @export

get_mse <- function(results_list, true_theta = log(1)) {
  theta.list <- results_list[c("crude.theta", "match.theta", "match.robust.theta", 
                               "adjusted.theta", "adjusted.ps.theta", "match.adjusted.theta", 
                               "match.robust.adjusted.theta", "stratified.theta", "stratified.adjusted.theta",
                               "weight.theta", "weight.adjusted.theta")]
  
  mse.calc <- function(x) mean((x - true_theta)^2)
  mse <- unlist(lapply(theta.list, FUN = mse.calc))
  
  return(round(mse, 3)) 
}









