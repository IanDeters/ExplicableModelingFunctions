nbinomMethodOfMoments = function(Mean, Variance)
{
  
  # Determine the standard deviation parameter
  
  prob = Mean / Variance
  
  # Return the results

  c(Mean * prob / (1 - prob), prob)

}

if (FALSE)
{
  
  # Apply the method of moments

  size = nbinomMethodOfMoments(200, 400)
  prob = size[2]
  size = size[1]
  
  # Demonstrate the efficacy of the method
  
  size * (1 - prob) / prob
  size * (1 - prob) / prob^2
  
  # Create a sample
  
  set.seed(666)

  Sample = rnbinom(n = 1000000, size = size, prob = prob)

  # Observe the statistics
  
  mean(Sample)
  var(Sample)

}