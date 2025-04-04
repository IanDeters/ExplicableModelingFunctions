rangerCrossValidation = function(data, label, params, NFold = 10, TrainingPercentage = .5)
{
  
  # Initialize the summary
  
  Summary = data.table()
  
  for (i in seq(NFold))
  {
    
    # Set the seed
    
    set.seed(ifelse(is.null(params[['seed']]) == TRUE, i, params[['seed']] - 1 + i))
    
    # Create a vector to select the training records
    
    TrainTestVector = sample(x = c(TRUE, FALSE), size = nrow(data), replace = TRUE, prob = c(TrainingPercentage, 1 - TrainingPercentage))

    # Create a random forest
    
    Model = ranger(
      
      y = label, x = data,
      case.weights = TrainTestVector,
      holdout = TRUE,
      num.trees = params[['num.trees']],
      mtry = params[['mtry']],
      importance = 'impurity',
      min.node.size = params[['min.node.size']],
      min.bucket = params[['min.bucket']],
      max.depth = params[['max.depth']],
      replace = params[['replace']],
      sample.fraction = params[['sample.fraction']],
      splitrule = 'variance',
      oob.error = params[['oob.error']],
      num.threads = params[['num.threads']],
      seed = ifelse(is.null(params[['seed']]) == TRUE, i, params[['seed']] - 1 + i)
      
    )

    # Update the summary
    
    Summary = rbind(Summary, data.table(seed = ifelse(is.null(params[['seed']]) == TRUE, i, params[['seed']] - 1 + i), prediction.error = Model[['prediction.error']]))
    
  }
  
  # Return the summary
  
  Summary
  
}

if (FALSE)
{
  
  library(data.table)
  library(ranger)
  
  # Set the number of observations
  
  NumberOfObservations = 1000
  
  # Set the number of predictors to be used
  
  NumberOfPredictors = 47
  
  # Set the seed
  
  set.seed(666)
  
  # Create a data set of potential predictors
  
  DataSet = as.data.table(matrix(data = runif(NumberOfObservations * 100), ncol = 100, nrow = NumberOfObservations))
  
  # Create a set of predictor variables 
  
  Predictors = sample(names(DataSet), NumberOfPredictors, replace = FALSE)
  
  # Create a weight variable
  
  DataSet$Weight = runif(NumberOfObservations)
  
  # Create a Poisson random variable
  
  DataSet$Poisson = round(100 * rpois(NumberOfObservations, DataSet$Weight * exp(as.matrix(DataSet[, Predictors, with = FALSE]) %*% (5 * seq(NumberOfPredictors) / sum(seq(NumberOfPredictors))))) / DataSet$Weight, 0)

  # Create parameters for the ranger function
  
  rangerParameters = list(
    
    num.trees = 500, # [1, Inf)
    mtry = NULL, # [1, ncol(x)] sqrt(ncol(x))
    min.node.size = NULL, # (0, Inf), 5
    min.bucket = NULL, # (0, Inf), 1
    max.depth = NULL, # [1, floor(nrow(x) / min.bucket))
    replace = TRUE, # c(FALSE, TRUE)
    sample.fraction = 1, # (0, 1), c(.632, 1)
    oob.error = TRUE,
    num.threads = NULL,
    seed = 667
    
  )
  
  # Observe the results
    
  rangerCrossValidation(data = DataSet[, names(DataSet)[!(names(DataSet) %in% c('Poisson'))], with = FALSE], label = DataSet$Poisson, params = rangerParameters, NFold = 10, TrainingPercentage = .7)
  
}