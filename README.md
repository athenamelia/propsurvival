## A Monte Carlo study on propensity score applied to survival data analysis

Propensity score analysis are widely used in biomedical research to estimate treatment effect from survival data in observational studies. However, within the framework of popular proportional hazard models, the choice among marginal, stratified or adjusted models is not clearly defined. This report introduces an R package to conduct a Monte Carlo simulation study to compare the performance of different survival models to estimate treatment effects. In addition to the impact of matching, propensity score based weighting approach is also assessed. 
  Simulation results show that ignoring the paired structure of the data led to an increased test size due to an overestimated variance of the treatment effect. Among survival models considered, stratified models systematically showed poorer performance. In all cases, it is necessary to employ propensity score through matching or weighting to balance out confounders, hence obtaining an unbiased estimator of treatment effect.

The `propsurvival` package includes functions:

* `simulate_data.R`: simulate a data set consisting of binary observed covariates, treatment indicator, survival time and event indicator for 1000 subjects. Censoring rate is about $40\%$ by default. 

* `match_data.R`: produce a 1:1 matched data set based on original sample

* `weight_data.R`: produce a PS weighted data set based on original sample

* `fit_models.R`: fit several survival models and return estimates

* `get_theta.R`: obtain mean of treatment effect estimates

* `get_sd.R`: obtain standard deviation of treatment effect estimates

* `get_se.R`: obtain mean of estimated standard error of treatment effect estimates 

* `get_ratio.R`: obtain SD$_\theta$/SE ratio

* `get_bias.R`: obtain bias of treatment effect estimates

* `get_mse.R`: obtain mean squared error over the 10,000 simulations

* `get_testSize.R`: obtain percentage of simulated data set leading to incorrect rejection of $H_0$
