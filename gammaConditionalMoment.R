gammaConditionalMoment = function(shape = 1, scale = 1, min = 0, max = Inf, moment = 1)
{

  scale^moment * apply(matrix(data = shape, nrow = max(length(shape), length(scale)), ncol = moment) + matrix(data = seq(0, moment - 1), nrow = max(length(shape), length(scale)), ncol = moment, byrow = TRUE), 1, prod) * 
  (pgamma(max / scale, shape + moment, lower.tail = TRUE) - pgamma(min / scale, shape + moment, lower.tail = TRUE)) / (pgamma(max / scale, shape, lower.tail = TRUE) - pgamma(min / scale, shape, lower.tail = TRUE))

}

if (FALSE)
{
  
  # Generate a vector from the conditional distribution
  
  set.seed(666)
  
  TestVector = Conditionalgamma(n = 1000000, shape = 50, scale = 2, min = 0, max = Inf)

  # Compare to the empirical estimate
  
  mean(TestVector)
  gammaConditionalMoment(shape = 50, scale = 2, min = 0, max = Inf, moment = 1)
  50 * 2
  mean(TestVector^2)
  gammaConditionalMoment(shape = 50, scale = 2, min = 0, max = Inf, moment = 2)
  50 * (50 + 1) * 2^2

  # Generate a vector from the conditional distribution
  
  TestVector = Conditionalgamma(n = 1000000, shape = 50, scale = 2, min = 100, max = Inf)

  # Compare to the empirical estimate
  
  mean(TestVector)
  gammaConditionalMoment(shape = 50, scale = 2, min = 100, max = Inf, moment = 1)
  mean(TestVector^2)
  gammaConditionalMoment(shape = 50, scale = 2, min = 100, max = Inf, moment = 2)

  # Generate a vector from the conditional distribution
  
  TestVector = Conditionalgamma(n = 1000000, shape = 50, scale = 2, min = 0, max = 110)
  
  # Compare to the empirical estimate
  
  mean(TestVector)
  gammaConditionalMoment(shape = 50, scale = 2, min = 0, max = 110, moment = 1)
  mean(TestVector^2)
  gammaConditionalMoment(shape = 50, scale = 2, min = 0, max = 110, moment = 2)
  
  # Generate a vector from the conditional distribution
  
  TestVector = Conditionalgamma(n = 1000000, shape = 50, scale = 2, min = 100, max = 110)
  
  # Compare to the empirical estimate
  
  mean(TestVector)
  gammaConditionalMoment(shape = 50, scale = 2, min = 100, max = 110, moment = 1)
  mean(TestVector^2)
  gammaConditionalMoment(shape = 50, scale = 2, min = 100, max = 110, moment = 2)
  
}