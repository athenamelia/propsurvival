#' Model fitting
#'
#' Fit several survival models and return estimates
#' @param data original sample
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
  
  # Matched–robust
  temp <- summary(survival::coxph(Surv(surv.time, surv.status) ~ treat + cluster(subclass), robust=TRUE, data = m.data))$coefficients
  match.robust.theta <- temp[,'coef']
  match.robust.se <- temp[,'robust se']
  
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
  
  # Matched–robust–adjusted 
  temp <- summary(survival::coxph(Surv(surv.time, surv.status) ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6 + treat + cluster(subclass), robust=TRUE, data = m.data))$coefficients
  match.robust.adjusted.theta <- temp[,'coef'][c('treat')]
  match.robust.adjusted.se <- temp[,'robust se'][c('treat')]
  
  # Stratified
  temp <- summary(survival::coxph(Surv(surv.time, surv.status) ~ treat + strata(distance), data = m.data))$coefficients
  stratified.theta <- temp[,'coef']
  stratified.se <- temp[,'se(coef)']
  
  # Stratified–adjusted
  temp <- summary(survival::coxph(Surv(surv.time, surv.status) ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6 + strata(distance) + treat, data = m.data))$coefficients
  stratified.adjusted.theta <- temp[,'coef'][c('treat')]
  stratified.adjusted.se <- temp[,'se(coef)'][c('treat')]
  
  # Weighted (IPTW) 
  temp <- summary(survival::coxph(Surv(surv.time, surv.status) ~ treat, weights = o.data$weight, data = o.data))$coefficients
  weight.theta <- temp[,'coef']
  weight.se <- temp[,'se(coef)']
  
  # Weighted–adjusted 
  temp <- summary(survival::coxph(Surv(surv.time, surv.status) ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6 + treat, weights = o.data$weight, data = o.data))$coefficients
  weight.adjusted.theta <- temp[,'coef'][c('treat')]
  weight.adjusted.se <- temp[,'se(coef)'][c('treat')]
  
  return(list(crude.theta, crude.se, match.theta, match.se, match.robust.theta, match.robust.se,
              adjusted.theta, adjusted.se, adjusted.ps.theta, adjusted.ps.se, 
              match.adjusted.theta, match.adjusted.se, match.robust.adjusted.theta, match.robust.adjusted.se,
              stratified.theta, stratified.se, stratified.adjusted.theta, stratified.adjusted.se,
              weight.theta, weight.se, weight.adjusted.theta, weight.adjusted.se))
}


