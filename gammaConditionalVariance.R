gammaConditionalVariance = function(shape = 1, scale = 1, min = 0, max = Inf)
{

  gammaConditionalMoment(shape = shape, scale = scale, min = min, max = max, moment = 2) - gammaConditionalMoment(shape = shape, scale = scale, min = min, max = max, moment = 1)^2

}

if (FALSE)
{
  
  # Generate a vector from the conditional distribution
  
  set.seed(666)
  
  TestVector = Conditionalgamma(n = 1000000, shape = 50, scale = 2, min = 0, max = Inf)
  
  # Compare to the empirical estimate
  
  var(TestVector)
  gammaConditionalVariance(shape = 50, scale = 2, min = 0, max = Inf)
  50 * 2^2

  # Generate a vector from the conditional distribution
  
  TestVector = Conditionalgamma(n = 1000000, shape = 50, scale = 2, min = 100, max = Inf)
  
  # Compare to the empirical estimate
  
  var(TestVector)
  gammaConditionalVariance(shape = 50, scale = 2, min = 100, max = Inf)

  # Generate a vector from the conditional distribution
  
  TestVector = Conditionalgamma(n = 1000000, shape = 50, scale = 2, min = 0, max = 110)
  
  # Compare to the empirical estimate
  
  var(TestVector)
  gammaConditionalVariance(shape = 50, scale = 2, min = 0, max = 110)
  
  # Generate a vector from the conditional distribution
  
  TestVector = Conditionalgamma(n = 1000000, shape = 50, scale = 2, min = 100, max = 110)
  
  # Compare to the empirical estimate
  
  var(TestVector)
  gammaConditionalVariance(shape = 50, scale = 2, min = 100, max = 110)

}