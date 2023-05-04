#' Model fitting
#'
#' Fit several survival models and return estimates
#' @param o.data original sample
#' @return Model estimates
#' @export

fit_models <- function(data) {
  o.data <- weight_data(data)
  m.data <- match_data(data)
  
  # Crude
  temp <- summary(survival::coxph(Surv(surv.time, surv.status) ~ treat, data = o.data))$coefficients
  crude.theta <- temp[,'coef']
  crude.se <- temp[,'se(coef)']
  
  # Matched–naive
  temp <- summary(survival::coxph(Surv(surv.time, surv.status) ~ treat, data = m.data))$coefficients
  match.theta <- temp[,'coef']
  match.se <- temp[,'se(coef)']
  
  # Adjusted
  temp <- summary(survival::coxph(Surv(surv.time, surv.status) ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6 + treat, data = o.data))$coefficients
  adjusted.theta <- temp[,'coef'][c('treat')]
  adjusted.se <- temp[,'se(coef)'][c('treat')]
  
  # Adjusted on PS 
  temp <- summary(survival::coxph(Surv(surv.time, surv.status) ~ ps.score + treat, data = o.data))$coefficients
  adjusted.ps.theta <- temp[,'coef'][c('treat')]
  adjusted.ps.se <- temp[,'se(coef)'][c('treat')]
  
  # Matched–naive–adjusted 
  temp <- summary(survival::coxph(Surv(surv.time, surv.status) ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6 + treat, data = m.data))$coefficients
  match.adjusted.theta <- temp[,'coef'][c('treat')]
  match.adjusted.se <- temp[,'se(coef)'][c('treat')]
  
  # Stratified
  temp <- summary(survival::coxph(Surv(surv.time, surv.status) ~ treat + strata(distance), data = m.data))$coefficients
  stratified.theta <- temp[,'coef']
  stratified.se <- temp[,'se(coef)']
  
  # Stratified–adjusted
  temp <- summary(survival::coxph(Surv(surv.time, surv.status) ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6 + strata(distance) + treat, data = m.data))$coefficients
  stratified.adjusted.theta <- temp[,'coef'][c('treat')]
  stratified.adjusted.se <- temp[,'se(coef)'][c('treat')]
  
  # Weighted (IPTW) 
  temp <- summary(survival::coxph(Surv(surv.time, surv.status) ~ treat, weight = weight, data = o.data))$coefficients
  weight.theta <- temp[,'coef']
  weight.se <- temp[,'se(coef)']
  
  # Weighted–adjusted 
  temp <- summary(survival::coxph(Surv(surv.time, surv.status) ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6 + treat, weight = weight, data = o.data))$coefficients
  weight.adjusted.theta <- temp[,'coef'][c('treat')]
  weight.adjusted.se <- temp[,'se(coef)'][c('treat')]
  
  return(list(crude.theta, crude.se, match.theta, match.se, adjusted.theta, adjusted.se,
              adjusted.ps.theta, adjusted.ps.se, match.adjusted.theta, match.adjusted.se, 
              stratified.theta, stratified.se, stratified.adjusted.theta, stratified.adjusted.se,
              weight.theta, weight.se, weight.adjusted.theta, weight.adjusted.se))
}

