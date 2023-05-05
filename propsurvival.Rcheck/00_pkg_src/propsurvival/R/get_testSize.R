#' Test size
#'
#' Obtain percentage of simulated data set leading to incorrect rejection of H0
#' @param results_list list of results
#' @param true_theta true value of theta for evaluation
#' @return test size
#' @importFrom stats pnorm
#' @importFrom stats predict
#' @export

get_testSize <- function(results_list, true_theta = log(1)) {
  theta.list <- results_list[c("crude.theta", "match.theta", "match.robust.theta", 
                               "adjusted.theta", "adjusted.ps.theta", "match.adjusted.theta", 
                               "match.robust.adjusted.theta", "stratified.theta", "stratified.adjusted.theta",
                               "weight.theta", "weight.adjusted.theta")]
  
  se.list <- results_list[c("crude.se", "match.se", "match.robust.se", "adjusted.se",
                            "adjusted.ps.se", "match.adjusted.se", "match.robust.adjusted.se",
                            "stratified.se", "stratified.adjusted.se",
                            "weight.se", "weight.adjusted.se")]
  
  z.score <- mapply("/",theta.list,se.list, SIMPLIFY = FALSE)
  absZ <- lapply(z.score, FUN = abs)
  
  p.two.sided <- function(x) 2*(1-pnorm(x))
  pval <- lapply(absZ, FUN = p.two.sided) 
  
  get.test.size <- function(x) mean(x < 0.05)
  test.size <- lapply(pval, FUN = get.test.size) 
  test.size <- unlist(test.size)
  return(round(test.size, 3))
}
