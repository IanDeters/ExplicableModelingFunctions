AlgebraicSoftMaxGradient = function(x, y, PositiveParameter)
{
  
  Shift = 1 / sqrt((x - y)^2 + PositiveParameter)
  Shift02 = Shift * (x - y)
  
  .5 * matrix(data = c(Shift02 + 1, Shift02 * (-1) + 1, .5 * Shift), ncol = 3)
  
}

if (FALSE)
{
  
  # Set the seed

  set.seed(666)

  # Generate three vectors

  x = runif(1000, min = -100, max = 100)
  y = runif(1000, min = -100, max = 100)
  PositiveParameter = 1 / (runif(1) + rpois(1000, 5))

  # Check against numerical differentiation

  max(abs(AlgebraicSoftMaxGradient(x, y, PositiveParameter) - t(sapply(seq(1000), function(n){numDeriv::grad(function(x){AlgebraicSoftMax(x[1], x[2], x[3])}, c(x[n], y[n], PositiveParameter[n]))}))))
  
}