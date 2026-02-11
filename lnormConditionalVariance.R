lnormConditionalVariance = function(meanlog = 0, sdlog = 1, min = 0, max = Inf)
{
  
  exp(2 * meanlog + 2 * sdlog^2) * (pnorm(q = (log(max) - meanlog) / sdlog - 2 * sdlog, mean = 0, sd = 1) - pnorm(q = (log(min) - meanlog) / sdlog - 2 * sdlog, mean = 0, sd = 1)) / (pnorm(q = (log(max) - meanlog) / sdlog, mean = 0, sd = 1) - pnorm(q = (log(min) - meanlog) / sdlog, mean = 0, sd = 1)) - lnormConditionalExpectation(meanlog = meanlog, sdlog = sdlog, min = min, max = max)^2

}

if (FALSE)
{
  
  # Generate a vector from the conditional distribution
  
  set.seed(666)
  
  TestVector = Conditionalrlnorm(n = 1000000, meanlog = 0, sdlog = 1, min = 0, max = Inf)
  
  # Compare to the empirical estimate
  
  var(TestVector)
  lnormConditionalVariance(meanlog = 0, sdlog = 1, min = 0, max = Inf)
  
  # Generate a vector from the conditional distribution
  
  TestVector = Conditionalrlnorm(n = 1000000, meanlog = 0, sdlog = 1, min = 1, max = Inf)

  # Compare to the empirical estimate
  
  var(TestVector)
  lnormConditionalVariance(meanlog = 0, sdlog = 1, min = 1, max = Inf)
  
  # Generate a vector from the conditional distribution
  
  TestVector = Conditionalrlnorm(n = 1000000, meanlog = 0, sdlog = 1, min = 0, max = 2)
  
  # Compare to the empirical estimate
  
  var(TestVector)
  lnormConditionalVariance(meanlog = 0, sdlog = 1, min = 0, max = 2)
  
  # Generate a vector from the conditional distribution
  
  TestVector = Conditionalrlnorm(n = 1000000, meanlog = 0, sdlog = 1, min = 1, max = 2)
  
  # Compare to the empirical estimate
  
  var(TestVector)
  lnormConditionalVariance(meanlog = 0, sdlog = 1, min = 1, max = 2)

}