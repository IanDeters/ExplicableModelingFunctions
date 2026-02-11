fMethodOfMoments = function(Mean, Variance)
{

  d2 = 2 * Mean / (Mean - 1)

  c(2 * d2^2 * (d2 - 2) / ((d2 - 2)^2 * (d2 - 4) * Variance - 2 * d2^2), d2)

}

if (FALSE)
{
  
  # Apply the method of moments by observing that 1 < EX < 2 since it is necessary that 4 < df2 for a finite and defined
  # mean and variance.  Moreover, VARX = (EX)^2 * ((EX - 1) / (2 - EX) + 2 / (df1 * (2 - EX))).  Hence, when the variance
  # is finite, (EX)^2(EX - 1) / (2 - EX) < VARX
  
  df1 = fMethodOfMoments(1.5, 2.5)
  df2 = df1[2]
  df1 = df1[1]
  
  # Demonstrate the efficacy of the method
  
  df2 / (df2 - 2)
  2 * df2^2 * (df1 + df2 - 2) / (df1 * (df2 - 2)^2 * (df2 - 4))

  # Create a sample
  
  set.seed(666)

  Sample = rf(n = 1000000, df1 = df1, df2 = df2)
  
  # Observe the statistics
  
  mean(Sample)
  var(Sample)
  
}