BreakPointApplication = function(BreakPoints, x)
{
  
  as.numeric((matrix(data = BreakPoints, nrow = length(x), ncol = length(BreakPoints), byrow = TRUE) < matrix(data = x, nrow = length(x), ncol = length(BreakPoints), byrow = FALSE)) %*% rep(1, length(BreakPoints)) + 1)
  
}

if (FALSE)
{
  
  library(data.table)
  
  # Set the seed
  
  set.seed(666)
  
  # Generate a vector
  
  x = runif(1000)
  
  # Apply the function and examine the results
  
  data.table(x = x[order(x)], Group = BreakPointApplication(seq(.1, .9, by = .1), x[order(x)]))[, .(Minimum = min(x), Maximum = max(x)), by = 'Group']
  
}