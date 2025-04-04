AlgebraicLogistic = function(x, PositiveParameter)
{
  
  .5 * (x / sqrt(x^2 + PositiveParameter) + 1)
  
}

if (FALSE)
{
  
  # Set the seed
  
  set.seed(666)
  
  # Generate two vectors
  
  x = runif(1000, min = -1, max = 1)
  
  # Initialize the difference vector
  
  Differences = c()
  
  # Calculate the differences between AlgebraicSoftMax and the indicator function
  
  for (i in seq(100000))
  {
    
    Differences = c(Differences, max(abs(AlgebraicLogistic(x, 1 / i) -  as.integer(0 < x))))
    
  }
  
  # Show the differences are decreasing
  
  min(Differences[seq(1, length(Differences) - 1)] - Differences[seq(2, length(Differences))])
  
  # Observe the difference for the smallest positive parameter
  
  Differences[length(Differences)]
  AlgebraicLogistic(max(x[x <= 0]), 1 / 100000)
  1 - AlgebraicLogistic(min(x[0 < x]), 1 / 100000)
  
  # Observe that the bound holds
  
  max(abs(Differences - .5 * (1 - min(abs(x)) / sqrt(min(abs(x))^2 + 1 / seq(100000)))))
  
}