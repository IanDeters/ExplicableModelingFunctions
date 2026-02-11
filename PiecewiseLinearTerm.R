PiecewiseLinearTerm = function(Matrix, Knots, Degree = 1)
{
  
  if (Degree == 0)
  {
    
    apply(matrix(data = Knots, nrow = nrow(Matrix), ncol = ncol(Matrix), byrow = TRUE) < Matrix, 1, prod)
    
  }
  
  else
  {
    
    apply(pmax(Matrix - matrix(data = Knots, nrow = nrow(Matrix), ncol = ncol(Matrix), byrow = TRUE), 0)^Degree, 1, prod)

  }

}

if (FALSE)
{
  
  library(data.table)
  
  # Set the parameters for the example

  NumberOfObservations = 1000
  NumberOfColumns = 3
  
  # Set the seed
  
  set.seed(666)
  
  # Create the example matrix
  
  Matrix = matrix(data = runif(n = NumberOfObservations * NumberOfColumns), nrow = NumberOfObservations, ncol = NumberOfColumns)
  
  # Create the knots
  
  Knots = runif(n = NumberOfColumns)
  
  # Apply the function and examine the results

  PiecewiseLinearTerm(Matrix, Knots, Degree = 1)
  PiecewiseLinearTerm(Matrix, Knots, Degree = 0)

}