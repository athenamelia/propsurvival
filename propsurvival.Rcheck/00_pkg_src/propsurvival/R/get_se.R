#' Estimated SE theta
#'
#' Obtain mean of estimated standard error of treatment effect estimates 
#' @param results_list list of results
#' @return estimated se of theta
#' @export

get_se <- function(results_list) {
  
  se.list <- results_list[c("crude.se", "match.se", "match.robust.se", "adjusted.se",
                            "adjusted.ps.se", "match.adjusted.se", "match.robust.adjusted.se",
                            "stratified.se", "stratified.adjusted.se",
                            "weight.se", "weight.adjusted.se")]
  
  se <- unlist(lapply(se.list, FUN = mean))

  return(round(se, 3))
}