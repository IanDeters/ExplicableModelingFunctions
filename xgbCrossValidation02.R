xgbCrossValidation = function(data, params, nrounds = 100, NFold = 10, TrainingPercentage = .5)
{
  
  # Initialize the summary
  
  Summary = data.table()
  
  for (i in seq(NFold))
  {
    
    # Create a vector to select the training records
    
    TrainTestVector = sample(x = c(TRUE, FALSE), size = nrow(data), replace = TRUE, prob = c(TrainingPercentage, 1 - TrainingPercentage))

    # Create the model
    
    Model = xgb.train(data = data[seq(nrow(data))[TrainTestVector], ], params = params, nrounds = nrounds, watchlist = list(train = data[seq(nrow(data))[TrainTestVector], ], test = data[seq(nrow(data))[!(TrainTestVector)], ]), verbose = 0)

    # Determine the round at which the error on the testing data is the lowest
    
    nrounds02 = Model[['evaluation_log']][['iter']][Model[['evaluation_log']][[3]] == min(Model[['evaluation_log']][[3]])][1]
    
    # Update the summary
    
    Summary = rbind(Summary, Model[['evaluation_log']][nrounds02, ])
    
  }
  
  # Return the summary
  
  Summary
  
}

if (FALSE)
{

  library(data.table)  
  library(xgboost)
  
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

  # Create the parameter set for xgb.train
  
  xgb.trainParameters = list(
    
    # General Parameters
    
    booster = 'gbtree', # c('gbtree', 'gblinear', 'dart')
    verbosity = 1, # c(0, 1, 2, 3)
    validate_parameters = TRUE,
    nthread = 2, # detectCores()
    disable_default_eval_metric = FALSE,
    
    # Parameters for Tree Booster  
    
    eta =  1, # [0, 1]
    gamma = 0, # [0, Inf)
    max_depth = 2, # [0, Inf)
    min_child_weight = 1, # (0, Inf)
    max_delta_step = 0, # (0, Inf)
    subsample = 1, # (0, 1]
    sampling_method = 'uniform', # c('uniform', 'gradient_based')
    colsample_bynode = 1, # (0, 1]
    colsample_bylevel = 1, # (0, 1]
    colsample_bytree = 1, # (0, 1]
    lambda = 1, # [0, Inf)
    alpha = 0, # [0, Inf)
    tree_method = 'auto', # c('auto', 'exact', 'approx', 'hist')
    scale_pos_weight = 1,
    refresh_leaf = 1, # c(0, 1)
    process_type = 'default', # c('default', 'update')
    grow_policy = 'depthwise', # c('depthwise', 'lossguide')
    max_leaves = 0, # [0, Inf)
    max_bin = 256, # [1, Inf)
    num_parallel_tree = 1, # [1, Inf)
    monotone_constraints = c(0, 0, 0), # c(-1, 0, 1)^NumberOfInputOrdinalVariables
    
    # Learning task parameters
    
    objective = 'reg:squarederror', 
    eval_metric = 'rmse',
    seed_per_iteration = FALSE)
  
  # Set the seed
  
  set.seed(666)
  
  # Run the cross validation
  
  xgbCrossValidation(data = DataSet[, names(DataSet)[!(names(DataSet) %in% c('Poisson'))], with = FALSE], label = DataSet$Poisson, params = xgb.trainParameters, nrounds = 90, NFold = 10, TrainingPercentage = .7)
  
}