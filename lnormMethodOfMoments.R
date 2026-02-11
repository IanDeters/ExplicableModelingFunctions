lnormMethodOfMoments = function(Mean, Variance)
{
  
  # Determine the standard deviation parameter
  
  sdlog = sqrt(log((Variance / Mean^2) + 1))
  
  # Return the results

  c(log(Mean) - sdlog^2 / 2, sqrt(log((Variance / Mean^2) + 1)))

}

if (FALSE)
{
  
  # Apply the method of moments

  meanlog = lnormMethodOfMoments(100, 50)
  sdlog = meanlog[2]
  meanlog = meanlog[1]
  
  # Demonstrate the efficacy of the method
  
  exp(meanlog + .5 * sdlog^2)
  exp(2 * meanlog + sdlog^2) * (exp(sdlog^2) - 1)
  
  # Create a sample
  
  set.seed(666)

  Sample = rlnorm(n = 1000000, meanlog = meanlog, sdlog = sdlog)  

  # Observe the statistics
  
  mean(Sample)
  var(Sample)

}