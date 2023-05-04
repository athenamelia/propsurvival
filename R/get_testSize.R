#' Test size
#'
#' Obtain percentage of simulated data set leading to incorrect rejection of H0
#' @param results_list list of results
#' @return test size
#' @export

get_testSize <- function(results_list, true_theta = log(1)) {
  theta.list <- results_list[c("crude.theta", "match.theta", "adjusted.theta",
                               "adjusted.ps.theta", "match.adjusted.theta", 
                               "stratified.theta", "stratified.adjusted.theta",
                               "weight.theta", "weight.adjusted.theta")]
  
  se.list <- results_list[c("crude.se", "match.se", "adjusted.se",
                            "adjusted.ps.se", "match.adjusted.se", 
                            "stratified.se", "stratified.adjusted.se",
                            "weight.se", "weight.adjusted.se")]
  
  z.score <- mapply("/",theta.list,se.list, SIMPLIFY = FALSE)
  p.value <- lapply(z.score, FUN = pnorm)
  get.test.size <- function(x) mean(x < 0.05)
  
  test.size <- lapply(p.value, FUN = get.test.size) 
  test.size <- unlist(test.size)
  
  return(test.size)
}