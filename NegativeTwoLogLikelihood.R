NegativeTwoLogLikelihood = function(Weight = rep(1, length(Target)), Target, family = 'gaussian', Mean, DispersionParameter = NULL, TweediePower = NULL)
{
  
  if (family == 'binomial')
  {
    
    return(-2  * sum(ifelse(Mean %in% c(0, 1), 0, lgamma(Weight + 1) - lgamma(Weight * Target + 1) - lgamma(Weight * (1 - Target) + 1) + Weight * Target * log(Mean) + Weight * (1 - Target) * log(1 - Mean))))

  }
  
  if (family == 'gaussian')
  {
    
    return(sum(Weight * (Target - Mean)^2 / DispersionParameter + log(2 * pi * Weight * DispersionParameter)))
    
  }
  
  if (family == 'Gamma')
  {
    
    return(-2 * sum(Weight * ((-1) * log(Mean) - Target / Mean + log(Target) + 1) / DispersionParameter - Weight * (1 + log(DispersionParameter)) / DispersionParameter - lgamma(Weight / DispersionParameter) + Weight * log(Weight) / DispersionParameter - log(Weight * Target)))

  }
  
  if (family == 'poisson')
  {
    
    return(-2 * sum(ifelse(Mean == 0, 0, Weight * (Target * log(Weight * Mean) - Mean) - lgamma(Weight * Target + 1))))
    
  }
  
  if (family == 'tweedie')
  {
    
    if (TweediePower == 0)
    {
      
      #return(sum(Weight * (Target - Mean)^2 / DispersionParameter + log(2 * pi * Weight * DispersionParameter)))
      return(sum(Weight * (Target - Mean)^2 / DispersionParameter + log(2 * pi * Weight * DispersionParameter)))

    }
    
    if (TweediePower == 1)
    {
      
      #return(-2 * sum(ifelse(Mean == 0, 0, Weight * (Target * log(Weight * Mean) - Mean) - lgamma(Weight * Target + 1))))
      return(-2 * sum(ifelse(Mean == 0, 0, Weight * (Target * log(Weight * Mean) - Mean) - lgamma(Weight * Target + 1))))

    }
    
    if (TweediePower == 2)
    {
      
      #return(-2 * sum((-1) * Weight * log(Mean * DispersionParameter) / DispersionParameter - lgamma(Weight / DispersionParameter) + (Weight / DispersionParameter - 1) * log(Weight * Target) - Weight * Target / (Mean * DispersionParameter)))
      return(-2 * sum(Weight * ((-1) * log(Mean) - Target / Mean + log(Target) + 1) / DispersionParameter - Weight * (1 + log(DispersionParameter)) / DispersionParameter - lgamma(Weight / DispersionParameter) + Weight * log(Weight) / DispersionParameter - log(Weight * Target)))

    }
    
    else
    {
      
      return(2 * sum(Weight * (pmax(Target, 0)^(2 - TweediePower) / ((1 - TweediePower) * (2 - TweediePower)) - Target * Mean^(1 - TweediePower) / (1 - TweediePower) + Mean^(2 - TweediePower) / (2 - TweediePower))) / DispersionParameter)

    }
    
  }

}

if (FALSE)
{
  
  library(data.table)
  library(tweedie)
  library(statmod)
  
  # Set the number of observations
  
  NumberOfObservations = 1000000

  # Set the seed
    
  set.seed(666)
  
  # Initialize the weight and predictor variables
  
  DataSet = data.table(Weight = rpois(NumberOfObservations, lambda = runif(NumberOfObservations)) + 1, x = runif(NumberOfObservations))

  # Generate variables from different distributions
    
  DataSet$binomial = rbinom(n = NumberOfObservations, size = DataSet$Weight, prob = 1 / (1 + exp((-1) * (.25 + .5 * DataSet$x)))) / DataSet$Weight
  DataSet$gaussian = rnorm(n = NumberOfObservations, mean = DataSet$Weight * (.25 + .5 * DataSet$x), sd = sqrt(DataSet$Weight) * 2) / DataSet$Weight
  DataSet$Gamma = rgamma(n = NumberOfObservations, shape = DataSet$Weight / 4, scale = 4 / (.25 + .5 * DataSet$x)) / DataSet$Weight
  DataSet$poisson = rpois(n = NumberOfObservations, lambda = DataSet$Weight * exp(.25 + .5 * DataSet$x)) / DataSet$Weight
  DataSet$tweedie = rtweedie(n = NumberOfObservations, mu = DataSet$Weight * exp(.25 + .5 * DataSet$x), phi = 4 * DataSet$Weight^(1 - 1.5), power = 1.5) / DataSet$Weight

  # Check against the density functions for all but Tweedie since the deviance, not the likelihood is programmed
  
  sum((-2) * log(dbinom(x = DataSet$Weight * DataSet$binomial, size = DataSet$Weight, prob = 1 / (1 + exp((-1) * (.25 + .5 * DataSet$x))), log = FALSE))) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$binomial, family = 'binomial', Mean = 1 / (1 + exp((-1) * (.25 + .5 * DataSet$x))), DispersionParameter = NULL, TweediePower = NULL)
  
  sum((-2) * log(dnorm(x = DataSet$Weight * DataSet$gaussian, mean = DataSet$Weight * (.25 + .5 * DataSet$x), sd = sqrt(DataSet$Weight) * 2, log = FALSE))) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$gaussian, family = 'gaussian', Mean = .25 + .5 * DataSet$x, DispersionParameter = 4, TweediePower = NULL)

  sum((-2) * log(dgamma(x = DataSet$Weight * DataSet$Gamma, shape = DataSet$Weight / 4, scale = 4 / (.25 + .5 * DataSet$x), log = FALSE))) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$Gamma, family = 'Gamma', Mean = 1 / (.25 + .5 * DataSet$x), DispersionParameter = 4, TweediePower = NULL)
  
  sum((-2) * log(dpois(DataSet$Weight * DataSet$poisson, lambda = DataSet$Weight * exp(.25 + .5 * DataSet$x), log = FALSE))) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$poisson, family = 'poisson', Mean = exp(.25 + .5 * DataSet$x), DispersionParameter = NULL, TweediePower = NULL)

  # Binomial model

  InterceptModel = glm(formula = as.formula('binomial ~ 1'), family = binomial(link = 'logit'), weights = Weight, data = DataSet)
  Model = glm(formula = as.formula('binomial ~ x'), family = binomial(link = 'logit'), weights = Weight, data = DataSet)

  # Recover the deviance
  
  Model$deviance
  (NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$binomial, family = 'binomial', Mean = Model$fitted.values, DispersionParameter = NULL, TweediePower = NULL) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$binomial, family = 'binomial', Mean = DataSet$binomial, DispersionParameter = NULL, TweediePower = NULL)) * summary(Model)[['dispersion']]
  
  # Recover the difference in negative two times the log likelihood
  
  (Model$null.deviance - Model$deviance) / summary(Model)[['dispersion']]
  NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$binomial, family = 'binomial', Mean = InterceptModel$fitted.values, DispersionParameter = NULL, TweediePower = NULL) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$binomial, family = 'binomial', Mean = Model$fitted.values, DispersionParameter = NULL, TweediePower = NULL)

  # Gaussian model
  
  InterceptModel = glm(formula = as.formula('gaussian ~ 1'), family = gaussian(link = 'identity'), weights = Weight, data = DataSet)
  Model = glm(formula = as.formula('gaussian ~ x'), family = gaussian(link = 'identity'), weights = Weight, data = DataSet)

  # Recover the deviance
    
  Model$deviance
  (NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$gaussian, family = 'gaussian', Mean = Model$fitted.values, DispersionParameter = summary(Model)[['dispersion']], TweediePower = NULL) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$gaussian, family = 'gaussian', Mean = DataSet$gaussian, DispersionParameter = summary(Model)[['dispersion']], TweediePower = NULL)) * summary(Model)[['dispersion']]
  (NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$gaussian, family = 'tweedie', Mean = Model$fitted.values, DispersionParameter = summary(Model)[['dispersion']], TweediePower = 0) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$gaussian, family = 'tweedie', Mean = DataSet$gaussian, DispersionParameter = summary(Model)[['dispersion']], TweediePower = 0)) * summary(Model)[['dispersion']]
  
  # Recover the difference in negative two times the log likelihood for the same dispersion parameter
  
  (Model$null.deviance - Model$deviance) / summary(Model)[['dispersion']]
  NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$gaussian, family = 'gaussian', Mean = sum(DataSet$Weight * DataSet$gaussian) / sum(DataSet$Weight), DispersionParameter = summary(Model)[['dispersion']], TweediePower = NULL) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$gaussian, family = 'gaussian', Mean = Model$fitted.values, DispersionParameter = summary(Model)[['dispersion']], TweediePower = NULL)
  NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$gaussian, family = 'tweedie', Mean = sum(DataSet$Weight * DataSet$gaussian) / sum(DataSet$Weight), DispersionParameter = summary(Model)[['dispersion']], TweediePower = 0) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$gaussian, family = 'tweedie', Mean = Model$fitted.values, DispersionParameter = summary(Model)[['dispersion']], TweediePower = 0)

  # Recover the difference in negative two times the log likelihood for different dispersion parameters
  
  NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$gaussian, family = 'gaussian', Mean = InterceptModel$fitted.values, DispersionParameter = summary(InterceptModel)[['dispersion']], TweediePower = NULL) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$gaussian, family = 'gaussian', Mean = Model$fitted.values, DispersionParameter = summary(Model)[['dispersion']], TweediePower = NULL)
  InterceptModel$deviance / summary(InterceptModel)[['dispersion']] - Model$deviance / summary(Model)[['dispersion']] + log(summary(InterceptModel)[['dispersion']] / summary(Model)[['dispersion']]) * nrow(Model[['data']])

  # Gamma model
  
  InterceptModel = glm(formula = as.formula('Gamma ~ 1'), family = Gamma(link = 'inverse'), weights = Weight, data = DataSet)
  Model = glm(formula = as.formula('Gamma ~ x'), family = Gamma(link = 'inverse'), weights = Weight, data = DataSet)

  # Recover the deviance
  
  Model$deviance
  (NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$Gamma, family = 'Gamma', Mean = Model$fitted.values, DispersionParameter = summary(Model)[['dispersion']], TweediePower = NULL) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$Gamma, family = 'Gamma', Mean = DataSet$Gamma, DispersionParameter = summary(Model)[['dispersion']], TweediePower = NULL)) * summary(Model)[['dispersion']]
  (NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$Gamma, family = 'tweedie', Mean = Model$fitted.values, DispersionParameter = summary(Model)[['dispersion']], TweediePower = 2) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$Gamma, family = 'tweedie', Mean = DataSet$Gamma, DispersionParameter = summary(Model)[['dispersion']], TweediePower = 2)) * summary(Model)[['dispersion']]
  
  # Recover the difference in negative two times the log likelihood
  
  (Model$null.deviance - Model$deviance) / summary(Model)[['dispersion']]
  NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$Gamma, family = 'Gamma', Mean = sum(DataSet$Weight * DataSet$Gamma) / sum(DataSet$Weight), DispersionParameter = summary(Model)[['dispersion']], TweediePower = NULL) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$Gamma, family = 'Gamma', Mean = Model$fitted.values, DispersionParameter = summary(Model)[['dispersion']], TweediePower = NULL)
  NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$Gamma, family = 'tweedie', Mean = sum(DataSet$Weight * DataSet$Gamma) / sum(DataSet$Weight), DispersionParameter = summary(Model)[['dispersion']], TweediePower = 2) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$Gamma, family = 'tweedie', Mean = Model$fitted.values, DispersionParameter = summary(Model)[['dispersion']], TweediePower = 2)
  
  # Recover the difference in negative two times the log likelihood for different dispersion parameters

  InterceptModel$deviance / summary(InterceptModel)[['dispersion']] - Model$deviance / summary(Model)[['dispersion']] + 2 * sum(DataSet$Weight) * ((1 + log(summary(InterceptModel)[['dispersion']])) / summary(InterceptModel)[['dispersion']] - (1 + log(summary(Model)[['dispersion']])) / summary(Model)[['dispersion']]) + 2 * sum(lgamma(DataSet$Weight / summary(InterceptModel)[['dispersion']]) - lgamma(DataSet$Weight / summary(Model)[['dispersion']])) - 2 * sum(DataSet$Weight * log(DataSet$Weight)) * (1 / summary(InterceptModel)[['dispersion']] - 1 / summary(Model)[['dispersion']])
  NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$Gamma, family = 'Gamma', Mean = InterceptModel$fitted.values, DispersionParameter = summary(InterceptModel)[['dispersion']], TweediePower = NULL) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$Gamma, family = 'Gamma', Mean = Model$fitted.values, DispersionParameter = summary(Model)[['dispersion']], TweediePower = NULL)
  NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$Gamma, family = 'tweedie', Mean = InterceptModel$fitted.values, DispersionParameter = summary(InterceptModel)[['dispersion']], TweediePower = 2) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$Gamma, family = 'tweedie', Mean = Model$fitted.values, DispersionParameter = summary(Model)[['dispersion']], TweediePower = 2)

  # Poisson model
    
  Model = glm(formula = as.formula('poisson ~ x'), family = poisson(link = 'log'), weights = Weight, data = DataSet)

  # Recover the deviance
    
  Model$deviance
  (NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$poisson, family = 'poisson', Mean = Model$fitted.values, DispersionParameter = NULL, TweediePower = NULL) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$poisson, family = 'poisson', Mean = DataSet$poisson, DispersionParameter = NULL, TweediePower = NULL)) * summary(Model)[['dispersion']]
  (NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$poisson, family = 'tweedie', Mean = Model$fitted.values, DispersionParameter = NULL, TweediePower = 1) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$poisson, family = 'tweedie', Mean = DataSet$poisson, DispersionParameter = NULL, TweediePower = 1)) * summary(Model)[['dispersion']]
  
  # Recover the difference in negative two times the log likelihood
  
  (Model$null.deviance - Model$deviance) / summary(Model)[['dispersion']]
  NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$poisson, family = 'poisson', Mean = rep(sum(DataSet$Weight * DataSet$poisson) / sum(DataSet$Weight), nrow(DataSet)), DispersionParameter = summary(Model)[['dispersion']], TweediePower = NULL) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$poisson, family = 'poisson', Mean = Model$fitted.values, DispersionParameter = summary(Model)[['dispersion']], TweediePower = NULL)
  NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$poisson, family = 'tweedie', Mean = rep(sum(DataSet$Weight * DataSet$poisson) / sum(DataSet$Weight), nrow(DataSet)), DispersionParameter = summary(Model)[['dispersion']], TweediePower = 1) - NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$poisson, family = 'tweedie', Mean = Model$fitted.values, DispersionParameter = summary(Model)[['dispersion']], TweediePower = 1)

  # Tweedie model
  
  Model = glm(formula = as.formula('tweedie ~ x'), family = tweedie(var.power = 1.5, link.power = 0), weights = Weight, data = DataSet)
 
  # Recover the deviance
  
  Model$deviance
  NegativeTwoLogLikelihood(Weight = DataSet$Weight, Target = DataSet$tweedie, family = 'tweedie', Mean = Model$fitted.values, DispersionParameter = summary(Model)[['dispersion']], TweediePower = 1.5) * summary(Model)[['dispersion']]
  
}