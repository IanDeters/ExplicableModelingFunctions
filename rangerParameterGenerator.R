rangerParameterGenerator = function(ParameterBoundList)
{

  # Generate a list of parameters
  
  list(
    
    num.trees = seq(min(ParameterBoundList[['num.trees']]), max(ParameterBoundList[['num.trees']]))[sample(x = max(ParameterBoundList[['num.trees']]) - min(ParameterBoundList[['num.trees']]) + 1, 1)], # [1, Inf)
    mtry = seq(min(ParameterBoundList[['mtry']]), max(ParameterBoundList[['mtry']]))[sample(x = max(ParameterBoundList[['mtry']]) - min(ParameterBoundList[['mtry']]) + 1, 1)], # [1, ncol(x)], sqrt(ncol(x))
    min.node.size = runif(1, min = min(ParameterBoundList[['min.node.size']]), max = max(ParameterBoundList[['min.node.size']])), # (0, Inf), 5
    min.bucket = runif(1, min = min(ParameterBoundList[['min.bucket']]), max = max(ParameterBoundList[['min.bucket']])), # (0, Inf), 1
    max.depth = seq(min(ParameterBoundList[['max.depth']]), max(ParameterBoundList[['max.depth']]))[sample(x = max(ParameterBoundList[['max.depth']]) - min(ParameterBoundList[['max.depth']]) + 1, 1)], # [1, floor(nrow(x) / min.bucket)), NULL
    replace = sample(ParameterBoundList[['replace']], 1), # c(FALSE, TRUE), TRUE
    sample.fraction = runif(1, min = min(ParameterBoundList[['sample.fraction']]), max = max(ParameterBoundList[['sample.fraction']])), # (0, 1), c(.632, 1)
    oob.error = sample(ParameterBoundList[['oob.error']], 1), # TRUE
    num.threads = seq(min(ParameterBoundList[['num.threads']]), max(ParameterBoundList[['num.threads']]))[sample(x = max(ParameterBoundList[['num.threads']]) - min(ParameterBoundList[['num.threads']]) + 1, 1)], # 2
    seed = seq(min(ParameterBoundList[['seed']]), max(ParameterBoundList[['seed']]))[sample(x = max(ParameterBoundList[['seed']]) - min(ParameterBoundList[['seed']]) + 1, 1)] # NULL

  )

}

if (FALSE)
{
  
  # Create a list of bounds
  
  ParameterBoundList = list(
    
    num.trees = c(1, 500), # [1, Inf)
    mtry = c(1, 386), # [1, ncol(x)], sqrt(ncol(x))
    min.node.size = c(3, 10), # (0, Inf), 5
    min.bucket = c(1, 100), # (0, Inf), 1
    max.depth = c(1, 10), # [1, floor(nrow(x) / min.bucket)), NULL
    replace = c(FALSE, TRUE), # c(FALSE, TRUE), TRUE
    sample.fraction = c(0, 1), # (0, 1), c(.632, 1)
    oob.error = c(TRUE), # TRUE
    num.threads = c(32, 32), # 2
    seed = c(667, 667) # NULL
    
  )
  
  # Set the seed
  
  set.seed(666)
  
  # Generate the parameters within those bounds
  
  rangerParameterGenerator(ParameterBoundList)
  
}