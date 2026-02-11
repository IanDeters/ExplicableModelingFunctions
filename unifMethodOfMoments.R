unifMethodOfMoments = function(Mean, Variance)
{

  c(Mean - sqrt(3 * Variance), Mean + sqrt(3 * Variance))

}

if (FALSE)
{
  
  # Apply the method of moments
  
  min = unifMethodOfMoments(100, 200)
  max = min[2]
  min = min[1]

  # Demonstrate the efficacy of the method
  
  .5 * (max + min)
  (max - min)^2 / 12
  
  # Create a sample
  
  set.seed(666)

  Sample = runif(n = 1000000, min = min, max = max)
  
  # Observe the statistics
  
  mean(Sample)
  var(Sample)
  
}