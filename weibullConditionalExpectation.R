weibullConditionalExpectation = function(scale = 1, shape = 1, min = 0, max = Inf)
{

  scale * (pgamma(q = (max / scale)^shape, shape = 1 + 1 / shape, rate = 1) - pgamma(q = (min / scale)^shape, shape = 1 + 1 / shape, rate = 1)) * gamma(1 + 1 / shape) / (pweibull(q = max, shape = shape, scale = scale) - pweibull(q = min, shape = shape, scale = scale))

}

if (FALSE)
{
  
  # Generate a vector from the conditional distribution
  
  set.seed(666)
  
  TestVector = ConditionalWeibull(n = 1000000, scale = 2, shape = 1, min = 0, max = Inf)

  # Compare to the empirical estimate
  
  mean(TestVector)
  weibullConditionalExpectation(scale = 2, shape = 1, min = 0, max = Inf)
  gamma(1 + 1 / 1) * 2

  # Generate a vector from the conditional distribution
  
  TestVector = ConditionalWeibull(n = 1000000, scale = 2, shape = 1, min = 1, max = Inf)

  # Compare to the empirical estimate
  
  mean(TestVector)
  weibullConditionalExpectation(scale = 2, shape = 1, min = 1, max = Inf)

  # Generate a vector from the conditional distribution
  
  TestVector = ConditionalWeibull(n = 1000000, scale = 2, shape = 1, min = 0, max = 2)

  # Compare to the empirical estimate
  
  mean(TestVector)
  weibullConditionalExpectation(scale = 2, shape = 1, min = 0, max = 2)
  
  # Generate a vector from the conditional distribution
  
  TestVector = ConditionalWeibull(n = 1000000, scale = 2, shape = 1, min = 1, max = 2)
  
  # Compare to the empirical estimate
  
  mean(TestVector)
  weibullConditionalExpectation(scale = 2, shape = 1, min = 1, max = 2)
  
}