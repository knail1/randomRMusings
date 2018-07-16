library(FrF2)
set.seed(1)


for FrF2:
# nruns = 5-10
  
  
  # 11.1 (stepwise, lasso question)
  #cv.glmnet, dont mgaussian, just gaussian, recommend take 2nd binary column, then scale data and add it back afterward (
  # it doesn't make sense to scale a binary column!
  # he doesn't scale in the TA class, wants us to do it
  # step is the easiest one to use
  # you can use AIC as a quality of fit 
  # direction format in the step function
  # report cv Rsquared thats best, then adjust r-sq, the worst is plain r-sq (which adds in overfit)

  # solutions standardize, vs scale the data
  #standardizing does not change t and p values, scaling does
  #

# lasso optomal lambda 4
# plot min see pic


# suggest looping over alpha (normally we just pick it)
  # for the loop choose lambda.min (but that would be circular) look at hte deviance ratio correspond to alpha.min
  # pick the highest deviance ratio
# then do the same thing we did with lasso
