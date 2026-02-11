PiecewiseLinearTerm = function(Matrix, Knots, Degree = 1)
{
  
  if (Degree == 0)
  {
    
    apply(matrix(data = Knots[is.na(Knots) == FALSE], nrow = nrow(Matrix), ncol = sum(is.na(Knots) == FALSE), byrow = TRUE) < Matrix[, is.na(Knots) == FALSE], 1, prod)
    
  }
  
  else
  {
    
    apply(pmax(Matrix[, is.na(Knots) == FALSE] - matrix(data = Knots[is.na(Knots) == FALSE], nrow = nrow(Matrix), ncol = sum(is.na(Knots) == FALSE), byrow = TRUE), 0)^Degree, 1, prod) * apply(as.matrix(Matrix[, is.na(Knots) == TRUE]^Degree), 1, prod)

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

  cbind(Matrix, pmax(Matrix[, is.na(Knots) == FALSE] - matrix(data = Knots[is.na(Knots) == FALSE], nrow = nrow(Matrix), ncol = sum(is.na(Knots) == FALSE), byrow = TRUE), 0), Matrix[, is.na(Knots) == TRUE], PiecewiseLinearTerm(Matrix, Knots, Degree = 1))
  cbind(Matrix, matrix(data = Knots[is.na(Knots) == FALSE], nrow = nrow(Matrix), ncol = sum(is.na(Knots) == FALSE), byrow = TRUE), PiecewiseLinearTerm(Matrix, Knots, Degree = 0))
  
  # Make a knot missing
  
  Knots[2] = NA

  # Apply the function and examine the results
  
  cbind(Matrix, pmax(Matrix[, is.na(Knots) == FALSE] - matrix(data = Knots[is.na(Knots) == FALSE], nrow = nrow(Matrix), ncol = sum(is.na(Knots) == FALSE), byrow = TRUE), 0), Matrix[, is.na(Knots) == TRUE], PiecewiseLinearTerm(Matrix, Knots, Degree = 1))
  cbind(Matrix, matrix(data = Knots[is.na(Knots) == FALSE], nrow = nrow(Matrix), ncol = sum(is.na(Knots) == FALSE), byrow = TRUE), PiecewiseLinearTerm(Matrix, Knots, Degree = 0))
    
  # Make all knots missing
  
  Knots = rep(NA, length(Knots))
  
  # Apply the function and examine the results
  
  cbind(Matrix, pmax(Matrix[, is.na(Knots) == FALSE] - matrix(data = Knots[is.na(Knots) == FALSE], nrow = nrow(Matrix), ncol = sum(is.na(Knots) == FALSE), byrow = TRUE), 0), Matrix[, is.na(Knots) == TRUE], PiecewiseLinearTerm(Matrix, Knots, Degree = 1))
  cbind(Matrix, matrix(data = Knots[is.na(Knots) == FALSE], nrow = nrow(Matrix), ncol = sum(is.na(Knots) == FALSE), byrow = TRUE), PiecewiseLinearTerm(Matrix, Knots, Degree = 0))
  
}