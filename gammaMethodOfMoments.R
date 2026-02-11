gammaMethodOfMoments = function(Mean, Variance)
{

  c(Mean^2 / Variance, Variance / Mean)

}

if (FALSE)
{
  
  # Apply the method of moments by observing that 0 < EX < 1 and 0 < VarX < E[X] * E[1 - X]
  
  shape = gammaMethodOfMoments(100, 200)
  scale = shape[2]
  shape = shape[1]
  
  # Demonstrate the efficacy of the method
  
  shape * scale
  shape * scale^2
  
  # Create a sample
  
  set.seed(666)

  Sample = rgamma(n = 1000000, shape = shape, scale = scale)
  
  # Observe the statistics
  
  mean(Sample)
  var(Sample)
  
}