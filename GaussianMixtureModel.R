GaussianMixtureModel = function(q = 0, Weight = 1, mean = 0, sd = 1)
{
  
  sapply(seq(length(q)), function(x){sum(sapply(seq(length(Weight)), function(y){Weight[y] * pnorm(q = q[x], mean = mean[y], sd = sd[y])}))})

}

if (FALSE)
{
  
  library(data.table)

  # Set the seed
    
  set.seed(666)
  
  # Generate a sample
  
  x = rpois(n = 1000, lambda = 5)
  
  # Create the empirical cumulative distribution function
  
  Data = data.table(x = x)[order(x), .(ECDF = .N), by = 'x']
  Data$ECDF = cumsum(Data$ECDF) / 1000
  
  # Compare the results
  
  GaussianMixtureModel(q = Data$x, Weight = c(.5, .5), mean = c(4, 6), sd = c(1, 1))
  .5 * pnorm(q = Data$x, mean = 4, sd = 1) + .5 * pnorm(q = Data$x, mean = 6, sd = 1)

}