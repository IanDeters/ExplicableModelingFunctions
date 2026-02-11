Conditionalgamma = function(n = 1, shape = 1, scale = 1, min = 0, max = Inf)
{
  
  qgamma(runif(n = n, min = 0, max = 1) * (pgamma(q = max, shape = shape, scale = scale) - pgamma(q = min, shape = shape, scale = scale)) + pgamma(q = min, shape = shape, scale = scale), shape = shape, scale = scale)

}

if (FALSE)
{
  
  # Set the seed
  
  set.seed(666)
  
  # Sample from an unconditional distribution
  
  TestVector = Conditionalgamma(n = 1000000, shape = 50, scale = 2, min = 0, max = Inf)

  # Observe the test statistics
  
  min(TestVector)
  max(TestVector)

  TestVector = data.table(Sample = TestVector[order(TestVector)])[, .(CDF = .N / length(TestVector)), by = 'Sample']
  TestVector$CDF = cumsum(TestVector$CDF)
  
  max(abs(TestVector$CDF - pgamma(q = TestVector$Sample, shape = 50, scale = 2)))
  
  # Sample from a conditional distribution bounded below by 1
  
  TestVector = Conditionalgamma(n = 1000000, shape = 50, scale = 2, min = 50, max = Inf)
  
  # Observe the test statistics
  
  min(TestVector)
  max(TestVector)
  
  TestVector = data.table(Sample = TestVector[order(TestVector)])[, .(CDF = .N / length(TestVector)), by = 'Sample']
  TestVector$CDF = cumsum(TestVector$CDF)

  max(abs(TestVector$CDF - (pgamma(q = TestVector$Sample, shape = 50, scale = 2) - pgamma(q = 50, shape = 50, scale = 2)) / (pgamma(q = Inf, shape = 50, scale = 2) - pgamma(q = 50, shape = 50, scale = 2))))

  # Sample from a conditional distribution bounded above by 2
  
  TestVector = Conditionalgamma(n = 1000000, shape = 50, scale = 2, min = 0, max = 150)

  # Observe the test statistics
  
  min(TestVector)
  max(TestVector)
  
  TestVector = data.table(Sample = TestVector[order(TestVector)])[, .(CDF = .N / length(TestVector)), by = 'Sample']
  TestVector$CDF = cumsum(TestVector$CDF)
  
  max(abs(TestVector$CDF - (pgamma(q = TestVector$Sample, shape = 50, scale = 2) - pgamma(q = 0, shape = 50, scale = 2)) / (pgamma(q = 150, shape = 50, scale = 2) - pgamma(q = 0, shape = 50, scale = 2))))

  # Sample from a conditional distribution bounded below by 1 and above by 2

  TestVector = Conditionalgamma(n = 1000000, shape = 50, scale = 2, min = 50, max = 150)

  # Observe the test statistics
  
  min(TestVector)
  max(TestVector)
  
  TestVector = data.table(Sample = TestVector[order(TestVector)])[, .(CDF = .N / length(TestVector)), by = 'Sample']
  TestVector$CDF = cumsum(TestVector$CDF)

  max(abs(TestVector$CDF - (pgamma(q = TestVector$Sample, shape = 50, scale = 2) - pgamma(q = 50, shape = 50, scale = 2)) / (pgamma(q = 150, shape = 50, scale = 2) - pgamma(q = 50, shape = 50, scale = 2))))

}