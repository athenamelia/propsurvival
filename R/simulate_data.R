#' Data simulation
#'
#' Simulate a survival data set
#' @param beta0 different levels for covariates imbalance
#' @return A simulated dataset with binary observed covariates, treatment, event time and indicator for 1000 subjects
#' @export

simulate_data <- function(beta0 = -3.45){
  N <- 1000 # Size of simulated dataset.
  
  # Beta0 = -1.28 led to match approximately 75% of treated patients (imbalance I) 
  # Beta0 = -3.45 led to 50% (imbalance II)
  beta.0.treat <- beta0
  
  # theta values: log(1) log(1.5) log(2)
  theta <- log(1)
  
  # Nine binary covariates (x1 to x9) was simulated with high, 
  # intermediate and no association with (i) treatment allocation and (ii) outcome.
  # All covariates were drawn from independent Bernoulli distributions
  x.1 <- rbinom(N, size=1, prob=0.5)
  x.2 <- rbinom(N, size=1, prob=0.5)
  x.3 <- rbinom(N, size=1, prob=0.5)
  x.4 <- rbinom(N, size=1, prob=0.5)
  x.5 <- rbinom(N, size=1, prob=0.5)
  x.6 <- rbinom(N, size=1, prob=0.5)
  x.7 <- rbinom(N, size=1, prob=0.5)
  x.8 <- rbinom(N, size=1, prob=0.5)
  x.9 <- rbinom(N, size=1, prob=0.5)
  
  ################################################################################
  
  beta.low <- log(1)
  beta.med <- log(2)
  beta.high <- log(5)
  
  # Generate treatment status for each subject.
  # logit(p) = sum B0, B1, B2, B4, B5, B7, B8
  logit.treat <- beta.0.treat + beta.high*x.1 + beta.med*x.2 +
    beta.high*x.4 + beta.med*x.5 + beta.high*x.7 + beta.med*x.8 
  
  # B(pi) is the Bernoulli distribution of parameter pi 
  p.treat <- exp(logit.treat)/(1 + exp(logit.treat))
  
  treat <- rbinom(N,1,p.treat)
  
  ################################################################################
  
  alpha.low <- log(1)
  alpha.med <- log(1.3)
  alpha.high <- log(1.8) 
  
  # Generate survival outcome for each subject
  # using an exponential distribution with hazard lambda
  linpred <- theta*treat + alpha.high*x.1 + alpha.high*x.2 +
    alpha.high*x.3 + alpha.med*x.4 + alpha.med*x.5 + alpha.med*x.6 
  
  lambda <- 0.028 # baseline hazard
  nu <- 2
  ranu <- runif(N, 0, 1)
  surv.time <- (-log(ranu)/(lambda*exp(linpred)))^(1/nu)
  
  # Survival times were right censored using a U(0, 15)
  # which led to approximately 40% censored observations
  cens.time <- runif(N,min=0,max=15)
  surv.status <- as.numeric(surv.time <= cens.time)
  sum(surv.status == 0) # should be 40%
  
  ################################################################################
  
  # Original dataset consists of X, surv.time, treat for treatment assignment
  o.data <- cbind(x.1, x.2, x.3, x.4, x.5, x.6, x.7, x.8, x.9, surv.time, surv.status, treat)
  o.data <- data.frame(o.data)
  return(o.data)
}