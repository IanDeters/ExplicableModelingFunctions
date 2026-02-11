weibullConditionalMoment = function(scale = 1, shape = 1, min = 0, max = Inf, moment = 1)
{

  # This formula is the result of noting that when conditioning on the variable being in the interval [min, max],
  # one needs to compute values of the incomplete gamma function instead of over the full set of the positive reals.
  # Next one can compute the incomplete gamma function in R using the cumulative distribution from the gamma distribution
  # multiplied by the gamma function.  Calculation of the lower incomplete gamma function for different values of the upper
  # limit of integration is illustrated below for the gamma function evaluated at x = 4.

  # pgamma(q = 1, shape = 4, rate = 1) * gamma(4)
  # pgamma(q = 2, shape = 4, rate = 1) * gamma(4)
  # pgamma(q = 3, shape = 4, rate = 1) * gamma(4)
  # pgamma(q = 4, shape = 4, rate = 1) * gamma(4)
  # pgamma(q = 5, shape = 4, rate = 1) * gamma(4)
  # pgamma(q = 10, shape = 4, rate = 1) * gamma(4)
  # pgamma(q = 20, shape = 4, rate = 1) * gamma(4)
  # pgamma(q = 40, shape = 4, rate = 1) * gamma(4)

  scale^moment * (pgamma(q = (max / scale)^shape, shape = 1 + moment / shape, rate = 1) - pgamma(q = (min / scale)^shape, shape = 1 + moment / shape, rate = 1)) * gamma(1 + moment / shape) / (pweibull(q = max, shape = shape, scale = scale) - pweibull(q = min, shape = shape, scale = scale))

}

if (FALSE)
{
  
  # Generate a vector from the conditional distribution
  
  set.seed(666)
  
  TestVector = ConditionalWeibull(n = 1000000, scale = 2, shape = 1, min = 0, max = Inf)

  # Compare to the empirical estimate
  
  mean(TestVector)
  weibullConditionalMoment(scale = 2, shape = 1, min = 0, max = Inf, moment = 1)
  gamma(1 + 1 / 1) * 2
  mean(TestVector^2)
  weibullConditionalMoment(scale = 2, shape = 1, min = 0, max = Inf, moment = 2)
  gamma(1 + 2 / 1) * 2^2

  # Generate a vector from the conditional distribution
  
  TestVector = ConditionalWeibull(n = 1000000, scale = 2, shape = 1, min = 1, max = Inf)

  # Compare to the empirical estimate
  
  mean(TestVector)
  weibullConditionalMoment(scale = 2, shape = 1, min = 1, max = Inf, moment = 1)
  mean(TestVector^2)
  weibullConditionalMoment(scale = 2, shape = 1, min = 1, max = Inf, moment = 2)

  # Generate a vector from the conditional distribution
  
  TestVector = ConditionalWeibull(n = 1000000, scale = 2, shape = 1, min = 0, max = 2)

  # Compare to the empirical estimate
  
  mean(TestVector)
  weibullConditionalMoment(scale = 2, shape = 1, min = 0, max = 2, moment = 1)
  mean(TestVector^2)
  weibullConditionalMoment(scale = 2, shape = 1, min = 0, max = 2, moment = 2)
  
  # Generate a vector from the conditional distribution
  
  TestVector = ConditionalWeibull(n = 1000000, scale = 2, shape = 1, min = 1, max = 2)
  
  # Compare to the empirical estimate
  
  mean(TestVector)
  weibullConditionalMoment(scale = 2, shape = 1, min = 1, max = 2, moment = 1)
  mean(TestVector^2)
  weibullConditionalMoment(scale = 2, shape = 1, min = 1, max = 2, moment = 2)
  
}