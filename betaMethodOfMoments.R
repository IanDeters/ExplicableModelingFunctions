betaMethodOfMoments = function(Mean, Variance)
{
  
  c((Mean * (1 - Mean) / Variance - 1) * Mean, (Mean * (1 - Mean) / Variance - 1) * (1 - Mean))

}

if (FALSE)
{
  
  # Apply the method of moments by observing that 0 < EX < 1 and 0 < VarX < E[X] * E[1 - X]
  
  shape1 = betaMethodOfMoments(.5, .2)
  shape2 = shape1[2]
  shape1 = shape1[1]
  
  # Demonstrate the efficacy of the method
  
  shape1 / (shape1 + shape2)
  shape1 * shape2 / ((shape1 + shape2)^2 * (shape1 + shape2 + 1))
  
  # Create a sample
  
  set.seed(666)

  Sample = rbeta(n = 1000000, shape1 = shape1, shape2 = shape2)
  
  # Observe the statistics
  
  mean(Sample)
  var(Sample)
  
}