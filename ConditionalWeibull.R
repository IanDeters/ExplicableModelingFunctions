ConditionalWeibull = function(n = 1, scale = 1, shape = 1, min = 0, max = Inf)
{
  
  qweibull(runif(n = n, min = 0, max = 1) * (pweibull(q = max, scale = scale, shape = shape) - pweibull(q = min, scale = scale, shape = shape)) + pweibull(q = min, scale = scale, shape = shape), scale = scale, shape = shape)

}

if (FALSE)
{
  
  # Set the seed
  
  set.seed(666)
  
  # Sample from an unconditional distribution
  
  TestVector = ConditionalWeibull(n = 1000000, scale = 2, shape = 1, min = 0, max = Inf)

  # Observe the test statistics
  
  min(TestVector)
  max(TestVector)

  TestVector = data.table(Sample = TestVector[order(TestVector)])[, .(CDF = .N / length(TestVector)), by = 'Sample']
  TestVector$CDF = cumsum(TestVector$CDF)
  
  max(abs(TestVector$CDF - pweibull(q = TestVector$Sample, scale = 2, shape = 1)))
  
  # Sample from a conditional distribution bounded below by 1
  
  TestVector = ConditionalWeibull(n = 1000000, scale = 2, shape = 1, min = 1, max = Inf)
  
  # Observe the test statistics
  
  min(TestVector)
  max(TestVector)
  
  TestVector = data.table(Sample = TestVector[order(TestVector)])[, .(CDF = .N / length(TestVector)), by = 'Sample']
  TestVector$CDF = cumsum(TestVector$CDF)

  max(abs(TestVector$CDF - (pweibull(q = TestVector$Sample, scale = 2, shape = 1) - pweibull(q = 1, scale = 2, shape = 1)) / (pweibull(q = Inf, scale = 2, shape = 1) - pweibull(q = 1, scale = 2, shape = 1))))

  # Sample from a conditional distribution bounded above by 2
  
  TestVector = ConditionalWeibull(n = 1000000, scale = 2, shape = 1, min = 0, max = 2)

  # Observe the test statistics
  
  min(TestVector)
  max(TestVector)
  
  TestVector = data.table(Sample = TestVector[order(TestVector)])[, .(CDF = .N / length(TestVector)), by = 'Sample']
  TestVector$CDF = cumsum(TestVector$CDF)
  
  max(abs(TestVector$CDF - (pweibull(q = TestVector$Sample, scale = 2, shape = 1) - pweibull(q = 0, scale = 2, shape = 1)) / (pweibull(q = 2, scale = 2, shape = 1) - pweibull(q = 0, scale = 2, shape = 1))))

  # Sample from a conditional distribution bounded below by 1 and above by 2

  TestVector = ConditionalWeibull(n = 1000000, scale = 2, shape = 1, min = 1, max = 2)  

  # Observe the test statistics
  
  min(TestVector)
  max(TestVector)
  
  TestVector = data.table(Sample = TestVector[order(TestVector)])[, .(CDF = .N / length(TestVector)), by = 'Sample']
  TestVector$CDF = cumsum(TestVector$CDF)

  max(abs(TestVector$CDF - (pweibull(q = TestVector$Sample, scale = 2, shape = 1) - pweibull(q = 1, scale = 2, shape = 1)) / (pweibull(q = 2, scale = 2, shape = 1) - pweibull(q = 1, scale = 2, shape = 1))))

}