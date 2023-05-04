#' Ratio of SD/SE 
#'
#' Obtain SD/SE ratio
#' @param results_list list of results
#' @return SD/SE ratio
#' @export

get_ratio <- function(results_list) {
  sd.list <- get_sd(results_list)
  se.list <- get_se(results_list)
  
  ratio <- sd.list/se.list
  return(ratio)
}