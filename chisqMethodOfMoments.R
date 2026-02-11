chisqMethodOfMoments = function(Mean, Variance)
{
  
  c(2 * Mean - Variance / 2, Variance / 2 - Mean)

}

if (FALSE)
{
  
  # Apply the method of moments by observing that 2 * E[X] <= VarX <= 4 * E[X]
  
  df = chisqMethodOfMoments(100, 300)
  ncp = df[2]
  df = df[1]
  
  # Demonstrate the efficacy of the method

  df + ncp
  2 * (df + 2 * ncp)

  # Create a sample
  
  set.seed(666)

  Sample = rchisq(n = 1000000, df = df, ncp = ncp)
  
  # Observe the statistics
  
  mean(Sample)
  var(Sample)
  
}