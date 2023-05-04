#' Propensity score weighting 
#'
#' Produce a weighted data set
#' @param o.data original sample
#' @return A weighted data set
#' @export

weight_data <- function(o.data) {
  ps.model <- glm(treat ~ x.1 + x.2 + x.4 + x.5 + x.7 + x.8, family = binomial(link = "logit"), data = o.data)
  o.data$ps.score <- predict(ps.model, type = "response")
  o.data$weight <- ifelse(o.data$treat == 1, 1/(ps.score), 1/(1-ps.score))
  return(o.data)
}