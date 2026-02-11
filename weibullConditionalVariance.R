weibullConditionalVariance = function(scale = 1, shape = 1, min = 0, max = Inf)
{
  
  weibullConditionalMoment(scale = scale, shape = shape, min = min, max = max, moment = 2) - weibullConditionalMoment(scale = scale, shape = shape, min = min, max = max, moment = 1)^2

}

if (FALSE)
{
  
  # Generate a vector from the conditional distribution
  
  set.seed(666)
  
  TestVector = ConditionalWeibull(n = 1000000, scale = 2, shape = 1, min = 0, max = Inf)
  
  # Compare to the empirical estimate
  
  var(TestVector)
  weibullConditionalVariance(scale = 2, shape = 1, min = 0, max = Inf)
  2^2 * (gamma(1 + 2 / 1) - gamma(1 + 1 / 1))
  
  # Generate a vector from the conditional distribution
  
  TestVector = ConditionalWeibull(n = 1000000, scale = 2, shape = 1, min = 1, max = Inf)
  
  # Compare to the empirical estimate
  
  var(TestVector)
  weibullConditionalVariance(scale = 2, shape = 1, min = 1, max = Inf)
  
  # Generate a vector from the conditional distribution
  
  TestVector = ConditionalWeibull(n = 1000000, scale = 2, shape = 1, min = 0, max = 2)
  
  # Compare to the empirical estimate
  
  var(TestVector)
  weibullConditionalVariance(scale = 2, shape = 1, min = 0, max = 2)
  
  # Generate a vector from the conditional distribution
  
  TestVector = ConditionalWeibull(n = 1000000, scale = 2, shape = 1, min = 1, max = 2)
  
  # Compare to the empirical estimate
  
  var(TestVector)
  weibullConditionalVariance(scale = 2, shape = 1, min = 1, max = 2)

}