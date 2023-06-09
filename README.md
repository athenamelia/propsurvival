## A Monte Carlo study on propensity score methods for survival data

  Propensity score analysis are widely used in biomedical research to estimate treatment effect from survival data in observational studies. However, within the framework of popular proportional hazard models, the choice among marginal, stratified or adjusted models is not clearly defined. This report introduces an R package to conduct a Monte Carlo simulation study to compare the performance of different survival models to estimate treatment effects. In addition to the impact of matching, propensity score based weighting approach is also assessed. 
  
  Simulation results show that ignoring the paired structure of the data led to an increased test size due to an overestimated variance of the treatment effect. Among survival models considered, stratified models systematically showed poorer performance. In all cases, it is necessary to employ propensity score through matching or weighting to balance out confounders, hence obtaining an unbiased estimator of treatment effect.

  This project was inspired by the paper "Propensity score applied to survival data analysis through proportional hazards models: a Monte Carlo study" by  Gayat et al (2012). In the paper, the authors examined the performance of PS adjustment and matching, but not PS weighting which is the extended work of this project. 
  
The `propsurvival` package includes functions:

* `generate_mc.R`: conduct a monte carlo study and return results including estimates of theta and SE over 10,000 iterations for imbalance level II ($\beta_0 = -3.45$) by default. Users can change $\beta_0 = -1.28$ for imbalance level I setting. There is also an option to specify the number of cores for parallelization through parameter `cores`. By default, `cores` is 1. 

* `simulate_data.R`: simulate a data set consisting of binary observed covariates, treatment indicator, survival time and event indicator for 1000 subjects. $\beta_0 = -3.45$ for imbalance level II by default. Users can change $\beta_0 = -1.28$ for imbalance level I setting.

* `match_data.R`: produce a matched data set with 1:1 matching algorithm without replacement. Treated patients were matched to the closest control within 0.2 SD of the logit of the estimated PS

* `weight_data.R`: produce a PS weighted data set with inverse probability treatment weighting (IPTW)

* `fit_models.R`: fit several survival models and return estimates

* `get_theta.R`: obtain mean of treatment effect estimates

* `get_sd.R`: obtain standard deviation of treatment effect estimates

* `get_se.R`: obtain mean of estimated standard error of treatment effect estimates 

* `get_ratio.R`: obtain SD/SE ratio

* `get_bias.R`: obtain bias of treatment effect estimates

* `get_mse.R`: obtain mean squared error over the all simulations

* `get_testSize.R`: obtain percentage of simulated data set leading to incorrect rejection of $H_0$
