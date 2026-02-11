weibullMethodOfMoments = function(Mean, Variance)
{
  
  # Calculate the constant for the root function
  
  Constant = Variance / Mean^2 + 1
  
  # Create a function to estimate the shape parameter
  
  RootFunction = function(k){Constant * gamma(1 + 1 / k)^2 - gamma(1 + 2 / k)}
  
  # Initialize the upper bound of the interval containing the shape parameter

  k = 1
  
  # The asymptotic value of RootFunction is Variance / Mean > 0.  Hence, the following loop will terminate

  while (RootFunction(k) < 0){k = k + 1}
  
  # The lower bound is obtained by considering squares of factorials

  k = try(uniroot(f = RootFunction, interval = c(1 / ceiling(log2(Constant)), k))[['root']])
  
  # If an error was thrown, try using the multiroot function
  
  if (class(k) == 'try-error')
  {
    
    # Define the derivative of the root function
    
    RootFunctionDerivative = function(k){(-2) * Constant * gamma(1 + 1 / k) * digamma(1 + 1 / k) * gamma(1 + 1 / k) / k^2 + 2 * digamma(1 + 2 / k) * gamma(1 + 2 / k) / k^2}
    
    # Numerically estimate the shape parameter
    
    k = multiroot(f = RootFunction, start = 1, jacfunc = RootFunctionDerivative)[['root']]
    
  }

  # Return the results

  c(Mean / gamma(1 + 1 / k), k)

}

if (FALSE)
{

  # Apply the method of moments
  
  scale = weibullMethodOfMoments(100, 50)
  shape = scale[2]
  scale = scale[1]
  
  # Demonstrate the efficacy of the method
  
  scale * gamma(1 + 1 / shape)
  
  scale^2 * (gamma(1 + 2 / shape) - gamma(1 + 1 / shape)^2)
  
  # Sample from the distribution
  
  set.seed(666)
  
  Sample = rweibull(n = 1000000, shape = shape, scale = scale)
  
  # Observe the statistics
  
  mean(Sample)
  var(Sample)

}