Conditionalrlnorm = function(n = 1, meanlog = 0, sdlog = 1, min = 0, max = Inf)
{
  
  qlnorm(runif(n = n, min = 0, max = 1) * (plnorm(q = max, meanlog = meanlog, sdlog = sdlog) - plnorm(q = min, meanlog = meanlog, sdlog = sdlog)) + plnorm(q = min, meanlog = meanlog, sdlog = sdlog), meanlog = meanlog, sdlog = sdlog)
  
}

if (FALSE)
{
  
  # Set the seed
  
  set.seed(666)
  
  # Sample from an unconditional distribution
  
  TestVector = Conditionalrlnorm(n = 1000000, meanlog = 0, sdlog = 1, min = 0, max = Inf)
  
  # Observe the test statistics
  
  min(TestVector)
  max(TestVector)

  TestVector = data.table(Sample = TestVector[order(TestVector)])[, .(CDF = .N / length(TestVector)), by = 'Sample']
  TestVector$CDF = cumsum(TestVector$CDF)
  
  max(abs(TestVector$CDF - plnorm(q = TestVector$Sample, meanlog = 0, sdlog = 1)))
  
  # Sample from a conditional distribution bounded below by 1
  
  TestVector = Conditionalrlnorm(n = 1000000, meanlog = 0, sdlog = 1, min = 1, max = Inf)
  
  # Observe the test statistics
  
  min(TestVector)
  max(TestVector)
  
  TestVector = data.table(Sample = TestVector[order(TestVector)])[, .(CDF = .N / length(TestVector)), by = 'Sample']
  TestVector$CDF = cumsum(TestVector$CDF)
  
  max(abs(TestVector$CDF - (plnorm(q = TestVector$Sample, meanlog = 0, sdlog = 1) - plnorm(q = 1, meanlog = 0, sdlog = 1)) / (plnorm(q = Inf, meanlog = 0, sdlog = 1) - plnorm(q = 1, meanlog = 0, sdlog = 1))))
  
  # Sample from a conditional distribution bounded above by 2
  
  TestVector = Conditionalrlnorm(n = 1000000, meanlog = 0, sdlog = 1, min = 0, max = 2)
  
  # Observe the test statistics
  
  min(TestVector)
  max(TestVector)
  
  TestVector = data.table(Sample = TestVector[order(TestVector)])[, .(CDF = .N / length(TestVector)), by = 'Sample']
  TestVector$CDF = cumsum(TestVector$CDF)
  
  max(abs(TestVector$CDF - (plnorm(q = TestVector$Sample, meanlog = 0, sdlog = 1) - plnorm(q = 0, meanlog = 0, sdlog = 1)) / (plnorm(q = 2, meanlog = 0, sdlog = 1) - plnorm(q = 0, meanlog = 0, sdlog = 1))))
  
  # Sample from a conditional distribution bounded below by 1 and above by 2
  
  TestVector = Conditionalrlnorm(n = 1000000, meanlog = 0, sdlog = 1, min = 1, max = 2)
  
  # Observe the test statistics
  
  min(TestVector)
  max(TestVector)
  
  TestVector = data.table(Sample = TestVector[order(TestVector)])[, .(CDF = .N / length(TestVector)), by = 'Sample']
  TestVector$CDF = cumsum(TestVector$CDF)
  
  max(abs(TestVector$CDF - (plnorm(q = TestVector$Sample, meanlog = 0, sdlog = 1) - plnorm(q = 1, meanlog = 0, sdlog = 1)) / (plnorm(q = 2, meanlog = 0, sdlog = 1) - plnorm(q = 1, meanlog = 0, sdlog = 1))))

}