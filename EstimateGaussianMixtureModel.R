EstimateGaussianMixtureModel = function(q = 0, ecdf = .5, k = 1, InitialValue = NULL, method = c('BFGS', 'CG', 'L-BFGS-B'))
{
  
  # Order the quantiles and attendant ecdf values
  
  q = q[order(q)]
  ecdf = ecdf[order(ecdf)]
  
  # Define the objective function
  
  GaussianMixtureModelObjectiveFunction = function(x)
  {
    
    # Ensure weights are positive and sum to 1
    
    x[seq(1, k)] = x[seq(1, k)]^2 / sum(x[seq(1, k)]^2)
    
    # Calculate the sum of squared differences
    
    sum((GaussianMixtureModel(q = q, Weight = x[seq(1, k)], mean = x[seq(k + 1, 2 * k)], sd = x[seq(2 * k + 1, 3 * k)]) - ecdf)^2)
    
  }
  
  GaussianMixtureModelObjectiveFunctionGradient = function(x)
  {
    
    # Record quantities related to the gradient of the weights
    
    Gradient = sapply(seq(k), function(i){(-2) * x[seq(k)][i] * ((x[seq(k)][i]^2 - sum(x[seq(k)]^2)) * (seq(k) == i) + x[seq(k)][i] * x[seq(k)] * (seq(k) != i)) / sum(x[seq(k)]^2)^2})
    
    # Ensure weights are non - negative and sum to 1
    
    x[seq(1, k)] = x[seq(1, k)]^2 / sum(x[seq(1, k)]^2)

    # Create matrix of gradients pertaining to the weight parameters
    
    Gradient = sapply(seq(k), function(i){pnorm(q, mean = x[k + i], sd = x[2 * k + i])}) %*% Gradient
    
    # Create matrix of gradients pertaining to the mean parameters
    
    Gradient = cbind(Gradient, sapply(seq(k), function(i){(-1) * x[i] * dnorm(x = x[k + i], mean = q, sd = x[2 * k + i])}))
    
    # Create matrix of gradients pertaining to the sd parameters
    
    Gradient = cbind(Gradient, sapply(seq(k), function(i){x[k] * dnorm(x = (q - x[k + i]) / x[2 * k + i], mean = 0, sd = 1) * (x[k + i] - q) / x[2 * k + i]^2}))

    # Return the gradient

    (2 * (GaussianMixtureModel(q = q, Weight = x[seq(1, k)], mean = x[seq(k + 1, 2 * k)], sd = x[seq(2 * k + 1, 3 * k)]) - ecdf)) %*% Gradient

  }
  
  if (is.null(InitialValue) == TRUE)
  {
    
    # Use the quantiles corresponding to the multiples of 1 / (k + 1) as initial values for the means
    
    Initialmean = matrix(data = seq(1 / (k + 1), k / (k + 1), by = 1 / (k + 1)), nrow = length(q), ncol = k, byrow = TRUE)
    Initialmean = Initialmean <= matrix(data = ecdf, nrow = length(q), ncol = k)
    Initialmean = Initialmean %*% rep(1, k)
    Initialmean = sapply(seq(k), function(i){q[i <= Initialmean][1]})
    
    # Use equal weights and standard deviations of 1 for the initial values
    
    InitialValue = c(rep(1 / k, k), Initialmean, rep(1, k))

  }

  # Try the different methods
    
  Parameters = lapply(method, function(x){optim(par = InitialValue, fn = GaussianMixtureModelObjectiveFunction, gr = GaussianMixtureModelObjectiveFunctionGradient, method = x, lower = c(rep(0, k), rep(min(q), k), rep(0, k)), upper = c(rep(1, k), rep(max(q), k), rep(max(q) - min(q), k)), hessian = FALSE)})

  # Select the parameters corresponding to the smallest error
    
  Parameters = Parameters[[seq(length(method))[sapply(seq(length(method)), function(i){Parameters[[i]][['value']]}) == min(sapply(seq(length(method)), function(i){Parameters[[i]][['value']]}))][1]]]

  # Return the parameters  

  list(
    
    Weight = Parameters[['par']][seq(1, k)]^2 / sum(Parameters[['par']][seq(1, k)]^2),
    mean = Parameters[['par']][seq(k + 1, 2 * k)],
    sd = Parameters[['par']][seq(2 * k + 1, 3 * k)]
    
  )  
  
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
  
  # Estimate a model using one Gaussian
  
  Results = EstimateGaussianMixtureModel(q = Data$x, ecdf = Data$ECDF, k = 1, InitialValue = NULL)
  
  # Examine the results
  
  sum((GaussianMixtureModel(q = Data$x, Weight = Results[['Weight']], mean = Results[['mean']], sd = Results[['sd']]) - Data$ECDF)^2)
  max(abs(GaussianMixtureModel(q = Data$x, Weight = Results[['Weight']], mean = Results[['mean']], sd = Results[['sd']]) - Data$ECDF))
  
  # Estimate a model using two Gaussians

  Results = EstimateGaussianMixtureModel(q = Data$x, ecdf = Data$ECDF, k = 2, InitialValue = NULL)
  
  # Examine the results
  
  sum((GaussianMixtureModel(q = Data$x, Weight = Results[['Weight']], mean = Results[['mean']], sd = Results[['sd']]) - Data$ECDF)^2)
  max(abs(GaussianMixtureModel(q = Data$x, Weight = Results[['Weight']], mean = Results[['mean']], sd = Results[['sd']]) - Data$ECDF))
  
  # Estimate a model using three Gaussians
  
  Results = EstimateGaussianMixtureModel(q = Data$x, ecdf = Data$ECDF, k = 3, InitialValue = NULL)
  
  # Examine the results
  
  sum((GaussianMixtureModel(q = Data$x, Weight = Results[['Weight']], mean = Results[['mean']], sd = Results[['sd']]) - Data$ECDF)^2)
  max(abs(GaussianMixtureModel(q = Data$x, Weight = Results[['Weight']], mean = Results[['mean']], sd = Results[['sd']]) - Data$ECDF))
  
  # Estimate a model using four Gaussians
  
  Results = EstimateGaussianMixtureModel(q = Data$x, ecdf = Data$ECDF, k = 4, InitialValue = NULL)
  
  # Examine the results
  
  sum((GaussianMixtureModel(q = Data$x, Weight = Results[['Weight']], mean = Results[['mean']], sd = Results[['sd']]) - Data$ECDF)^2)
  max(abs(GaussianMixtureModel(q = Data$x, Weight = Results[['Weight']], mean = Results[['mean']], sd = Results[['sd']]) - Data$ECDF))

}