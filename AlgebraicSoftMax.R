AlgebraicSoftMax = function(x, y, PositiveParameter)
{
  
  .5 * (sqrt((x - y)^2 + PositiveParameter) + x + y)
  
}

if (FALSE)
{
  
  # Set the seed
  
  set.seed(666)
  
  # Generate two vectors
  
  x = runif(1000, min = -100, max = 100)
  y = runif(1000, min = -100, max = 100)
  
  # Initialize the difference vector
  
  Differences = c()
  
  # Calculate the differences between AlgebraicSoftMax and the max(x, y) function
  
  for (i in seq(100000))
  {
    
    Differences = c(Differences, max(abs(AlgebraicSoftMax(x, y, 1 / i) - pmax(x, y))))
    
  }
  
  # Show the differences are decreasing
  
  min(Differences[seq(1, length(Differences) - 1)] - Differences[seq(2, length(Differences))])
  
  # Observe the difference for the largest joint
  
  Differences[length(Differences)]
  
}