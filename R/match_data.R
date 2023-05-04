#' Propensity score matching
#'
#' Produce a 1:1 matched data set
#' @param o.data original sample
#' @return A 1:1 matched data set
#' @export

match_data <- function(o.data) {
  ps.model <- glm(treat ~ x.1 + x.2 + x.4 + x.5 + x.7 + x.8, family = binomial(link = "logit"), data = o.data)
  o.data$ps.score <- predict(ps.model, type = "response")

  logitPS <-  log(o.data$ps.score/(1-o.data$ps.score))
  m.out1 <- MatchIt::matchit(treat ~ x.1 + x.2 + x.4 + x.5 + x.7 + x.8,
                             method = "nearest", distance = "glm", data = o.data,
                             caliper = .2*sd(logitPS), ratio = 1, replace=FALSE)
  
  m.data <- MatchIt::match.data(m.out1)
  return(m.data)
}