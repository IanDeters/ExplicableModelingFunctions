tMethodOfMoments = function(Mean, Variance)
{
  
  # Create the function whose root yields the df parameter.
  
  # RootFunction = function(df){df / (df - 2) * (1 + 2 * Mean^2 * gamma(.5 * df)^2 / (df * gamma(.5 * (df - 1))^2)) - Mean^2 - Variance}
  RootFunction = function(df){df / (df - 2) * (1 + 2 * Mean^2 * exp(2 * lgamma(.5 * df) - 2 * lgamma(.5 * (df - 1))) / df) - Mean^2 - Variance}

  # A few remarks are in order to observe that this function has a root. The most technical reason is that the following expressions,
  # derived from Stirling's approximation of the gamma function are asymptotically equal.  A numerical illustration of this follows
  # the expressions.

  # sqrt(.5 * x) * gamma(.5 * (x - 1)) / gamma(.5 * x)
  # sqrt(1 + 1 / (.5 * x - 1)) * (1 - .5 / (.5 * x - 1))^(.5 * x - 1) * exp(.5)
  # 
  # sapply(X = seq(4, 100) ,FUN = function(x){(sqrt(.5 * x) * gamma(.5 * (x - 1)) / gamma(.5 * x)) / (sqrt(1 + 1 / (.5 * x - 1)) * (1 - .5 / (.5 * x - 1))^(.5 * x - 1) * exp(.5))})

  # Recognizing that (1 - .5 / (.5 * x - 1))^(.5 * x - 1) converges to exp(.5), it can be seen than the first term in the function
  # is asymptotically equal to 1 + Mean^2.  Since the variance of a t distribution is greater than 1, the function becomes negative
  # for large values of df.  Hence, it is only necessary to find a value slightly larger than 2, where the function is positive
  # for a lower bound on the root.  Finally, the function is slightly rewritten to use the lgamma function instead of the gamma
  # function, since the gamma function gets large quickly.  The next bits of code create bounds within which lie the root.

  LowerBound = -1
  while(RootFunction(2 + 2^LowerBound) < 0){LowerBound = LowerBound - 1}

  UpperBound = 3
  while(0 < RootFunction(UpperBound)){UpperBound = UpperBound + 1}
  
  # Find the degrees of freedom

  df = try(uniroot(f = RootFunction, interval = c(2 + 2^LowerBound, UpperBound), tol = .Machine$double.eps)[['root']])

  # Return the results
  
  # c(df, Mean * sqrt(2 / df) * gamma(.5 * df) / gamma(.5 * (df - 1)))
  c(df, Mean * sqrt(2 / df) * exp(lgamma(.5 * df) - lgamma(.5 * (df - 1))))

}

if (FALSE)
{

  # Apply the method of moments
  
  df = tMethodOfMoments(100, 200)
  ncp = df[2]
  df = df[1]
  
  # Demonstrate the efficacy of the method
  
  # ncp * sqrt(df / 2) * gamma(.5 * (df - 1)) / gamma(.5 * df)
  ncp * sqrt(df / 2) * exp(lgamma(.5 * (df - 1)) - lgamma(.5 * df))
  # df * (1 + ncp^2) / (df - 2) - ncp^2 * df / 2 * (gamma(.5 * (df - 1)) / gamma(.5 * df))^2
  df * (1 + ncp^2) / (df - 2) - ncp^2 * df / 2 * (exp(lgamma(.5 * (df - 1)) - lgamma(.5 * df)))^2
  
  # Sample from the distribution
  
  set.seed(666)
  
  Sample = rt(n = 1000000, df = df, ncp = ncp)
  
  # Observe the statistics
  
  mean(Sample)
  var(Sample)

}