binomMethodOfMoments = function(Mean, Variance)
{
  
  c(1 - Variance / Mean, Mean^2 / (Mean - Variance))

}

if (FALSE)
{
  
  # Apply the method of moments by observing that 0 < EX < 1 and 0 < VarX < E[X] * E[1 - X]
  
  prob = binomMethodOfMoments(100, 50)
  size = prob[2]
  prob = prob[1]
  
  # Demonstrate the efficacy of the method
  
  prob * size
  prob * (1 - prob) * size
  
  # Create a sample
  
  set.seed(666)
  
  Sample = rbinom(n = 1000000, size = size, prob = prob)
  
  # Observe the statistics
  
  mean(Sample)
  var(Sample)
  
}