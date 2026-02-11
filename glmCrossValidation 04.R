glmCrossValidation = function(Model, Variables, NFold = 10, TrainingPercentage = .5, Cluster = NULL, PartitioningVariables = NULL, DTthreads = 1, Summarize = TRUE)
{
  
  if (is.null(Cluster) == FALSE)
  {
    
    # Register the cluster
    
    registerDoParallel(Cluster)
    
  }
  
  # Record the full data if summarization is to take place
  
  if (Summarize == TRUE)
  {
    
    FullData = Model[['data']]
    
  }

  # Extract the weights from the model object
  
  weights = Model$prior.weights
  
  # Record the current coefficients
  
  CurrentCoefficients = Model$coefficients
  
  # Extract the terms from the model formula
  
  CurrentTerms = names(CurrentCoefficients)
  CurrentTerms = CurrentTerms[CurrentTerms != '(Intercept)']
  
  # Initialize the summary
  
  Summary = list()
  
  for (i in seq(NFold))
  {
    
    # Determine which records will be used for training
    
    if (is.null(PartitioningVariables) == TRUE)
    {
      
      TrainingVector = sample(x = c(TRUE, FALSE), size = nrow(Model[['data']]), replace = TRUE, prob = c(TrainingPercentage, 1 - TrainingPercentage))

    }
    
    else
    {
      
      # Find all combinations of the partitioning variables
      
      TrainingVector = unique(Model[['data']][, PartitioningVariables, with = FALSE])
      
      # Select among the combinations of the partitioning variables according to the training percentage
      
      TrainingVector = TrainingVector[sample(x = c(TRUE, FALSE), size = nrow(TrainingVector), replace = TRUE, prob = c(TrainingPercentage, 1 - TrainingPercentage)), ]

      # Find the observation numbers corresponding to the selected combinations
      
      TrainingVector = merge(TrainingVector, cbind(seq(nrow(Model[['data']])), Model[['data']][, PartitioningVariables, with = FALSE]), by = PartitioningVariables)

      # Create a boolean vector according to the selected observation numbers
            
      TrainingVector = seq(nrow(Model[['data']])) %in% TrainingVector$V1
      
    }
    
    if (Summarize == TRUE)
    {
      
      # Get the current number of data table threads
      
      CurrentDTthreads = getDTthreads()
      
      # Set the number of data table threads
      
      setDTthreads(threads = DTthreads)
      
      # Initialize the summarized data

      SummarizedData = cbind(data.table(y = Model[['y']], prior.weights = Model$prior.weights, offset = Model$offset), FullData[, Variables, with = FALSE])
      
      # If there is no offset variable, create one
      
      if (is.null(SummarizedData[['offset']]) == TRUE)
      {
        
        SummarizedData[['offset']] = 0
        
      }
      
      SummarizedData = SummarizedData[TrainingVector, .(prior.weights = sum(prior.weights), y = sum(prior.weights * y) / sum(prior.weights), offset = get(Model$family$link)(sum(prior.weights * Model$family$linkinv(offset)) / sum(prior.weights))), by = c(Variables)]
      
      # Reset the number of data table threads

      setDTthreads(threads = CurrentDTthreads)

    }

    # Calibrate the current model using the data specified by the training vector
    
    if (Summarize == TRUE)
    {
      
      NewModel = glm.fit(x = cbind(rep(1, nrow(SummarizedData)), SummarizedData[, CurrentTerms, with = FALSE]), y = SummarizedData$y, weights = SummarizedData$prior.weights, offset = SummarizedData$offset, family = Model$family, control = Model$control, start = CurrentCoefficients)

    }
    
    else
    {
      
      NewModel = glm.fit(x = cbind(rep(1, sum(TrainingVector)), Model[['data']][TrainingVector, CurrentTerms, with = FALSE]), y = Model[['y']][TrainingVector], weights = weights[TrainingVector], offset = Model$offset[TrainingVector], family = Model$family, control = Model$control, start = CurrentCoefficients)

    }

    # Set missing estimates to 0
    
    NewModel$coefficients[is.na(NewModel$coefficients) == TRUE] = 0
    
    # Create the model predictions

    fitted.values = Model$family$linkinv(as.matrix(cbind(rep(1, length(weights)), Model[['data']][, CurrentTerms, with = FALSE])) %*% NewModel$coefficients)
    
    # Determine the adjustment so that the average prediction matches the average response on the testing data
    
    AdjustmentFactor = sum(Model[['prior.weights']][!(TrainingVector)] * Model$y[!(TrainingVector)]) / sum(Model[['prior.weights']][!(TrainingVector)] * fitted.values[!(TrainingVector)])
    
    # Adjust the model predictions
    
    fitted.values = fitted.values * AdjustmentFactor
    
    # Update the summary
    
    Summary = c(Summary, list(list(Variable = NA, Pass = i, Error = sqrt(sum(Model[['prior.weights']][!(TrainingVector)] * (Model$y[!(TrainingVector)] - fitted.values[!(TrainingVector)])^2) / sum(!(TrainingVector))), ParameterEstimate = summary.glm(NewModel)[['coefficients']])))
    
    if (is.null(Cluster) == FALSE)
    {

      Summary = c(Summary, foreach(j = seq(length(Variables)), .export = c('data.table')) %dopar% {
        
        Variable = Variables[j]
        
        # Create the terms for the model
        
        Terms = CurrentTerms[CurrentTerms != Variable]
        start = CurrentCoefficients[names(CurrentCoefficients) %in% c('(Intercept)', Terms)]
        
        # If it is a forward pass, include the variable
        
        if ((Variable %in% CurrentTerms) == FALSE)
        {
          
          Terms = c(CurrentTerms, Variable)
          start = c(start, 0)
          
        }
        
        # Create the new model
        
        if (Summarize == TRUE)
        {
          
          NewModel = try(glm.fit(x = cbind(rep(1, nrow(SummarizedData)), SummarizedData[, Terms, with = FALSE]), y = SummarizedData$y, weights = SummarizedData$prior.weights, offset = SummarizedData$offset, family = Model$family, control = Model$control, start = start))
          
          # If an error was thrown, try using the default initial values
          
          if (class(NewModel) == 'try-error')
          {
            
            NewModel = try(glm.fit(x = cbind(rep(1, nrow(SummarizedData)), SummarizedData[, Terms, with = FALSE]), y = SummarizedData$y, weights = SummarizedData$prior.weights, offset = SummarizedData$offset, family = Model$family, control = Model$control))
            
          }

        }
        
        else
        {
        
          NewModel = try(glm.fit(x = cbind(rep(1, sum(TrainingVector)), Model[['data']][TrainingVector, Terms, with = FALSE]), y = Model[['y']][TrainingVector], weights = weights[TrainingVector], offset = Model$offset[TrainingVector], family = Model$family, control = Model$control, start = start))
          
          # If an error was thrown, try using the default initial values
          
          if (class(NewModel) == 'try-error')
          {
            
            NewModel = glm.fit(x = cbind(rep(1, sum(TrainingVector)), Model[['data']][TrainingVector, Terms, with = FALSE]), y = Model[['y']][TrainingVector], weights = weights[TrainingVector], offset = Model$offset[TrainingVector], family = Model$family, control = Model$control)
            
          }
        
        }
        
        # Set missing estimates to 0
        
        NewModel$coefficients[is.na(NewModel$coefficients) == TRUE] = 0

        # Create the model predictions
        
        fitted.values = Model$family$linkinv(as.matrix(cbind(rep(1, length(weights)), Model[['data']][, Terms, with = FALSE])) %*% NewModel$coefficients)

        # Determine the adjustment so that the average prediction matches the average response on the testing data
        
        AdjustmentFactor = sum(Model[['prior.weights']][!(TrainingVector)] * Model$y[!(TrainingVector)]) / sum(Model[['prior.weights']][!(TrainingVector)] * fitted.values[!(TrainingVector)])
        
        # Adjust the model predictions
        
        fitted.values = fitted.values * AdjustmentFactor
        
        # Update the summary     
        
        list(Variable = Variable, Pass = i, Error = sqrt(sum(Model[['prior.weights']][!(TrainingVector)] * (Model$y[!(TrainingVector)] - fitted.values[!(TrainingVector)])^2) / sum(!(TrainingVector))), ParameterEstimate = summary.glm(NewModel)[['coefficients']])
        
      })

    }

    else
    {
      
      CheckTerm = function(Variable)
      {
        
        # Create the terms for the model
        
        Terms = CurrentTerms[CurrentTerms != Variable]
        start = CurrentCoefficients[names(CurrentCoefficients) %in% c('(Intercept)', Terms)]
        
        # If it is a forward pass, include the variable
        
        if ((Variable %in% CurrentTerms) == FALSE)
        {
          
          Terms = c(CurrentTerms, Variable)
          start = c(start, 0)
          
        }
        
        # Create the new model
        
        NewModel = try(glm.fit(x = cbind(rep(1, sum(TrainingVector)), Model[['data']][TrainingVector, Terms, with = FALSE]), y = Model[['y']][TrainingVector], weights = weights[TrainingVector], offset = Model$offset[TrainingVector], family = Model$family, control = Model$control, start = start))
        
        # If an error was thrown, try using the default initial values
        
        if (class(NewModel) == 'try-error')
        {
          
          NewModel = glm.fit(x = cbind(rep(1, sum(TrainingVector)), Model[['data']][TrainingVector, Terms, with = FALSE]), y = Model[['y']][TrainingVector], weights = weights[TrainingVector], offset = Model$offset[TrainingVector], family = Model$family, control = Model$control)
          
        }
        
        # Set missing estimates to 0
        
        NewModel$coefficients[is.na(NewModel$coefficients) == TRUE] = 0
        
        # Create the model predictions
        
        fitted.values = Model$family$linkinv(as.matrix(cbind(rep(1, length(weights)), Model[['data']][, Terms, with = FALSE])) %*% NewModel$coefficients)
        
        # Determine the adjustment so that the average prediction matches the average response on the testing data
        
        AdjustmentFactor = sum(Model[['prior.weights']][!(TrainingVector)] * Model$y[!(TrainingVector)]) / sum(Model[['prior.weights']][!(TrainingVector)] * fitted.values[!(TrainingVector)])
        
        # Adjust the model predictions
        
        fitted.values = fitted.values * AdjustmentFactor
        
        # Update the summary     
        
        list(Variable = Variable, Pass = i, Error = sqrt(sum(Model[['prior.weights']][!(TrainingVector)] * (Model$y[!(TrainingVector)] - fitted.values[!(TrainingVector)])^2) / sum(!(TrainingVector))), ParameterEstimate = summary.glm(NewModel)[['coefficients']])
        
      }
      
      Summary = c(Summary, lapply(Variables, CheckTerm))

    }

  }
  
  # Label the intercept terms
  
  for (i in seq(length(Summary))){row.names(Summary[[i]][['ParameterEstimate']])[1] = '(Intercept)'}
 
  # Return the results
   
  Summary

}

if (FALSE)
{
  
  library(data.table)
  library(doParallel)
  
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

  # Create variables to index records
    
  DataSet$PartitionVariable01 = sample(x = seq(10), size = nrow(DataSet), replace = TRUE)
  DataSet$PartitionVariable02 = sample(x = seq(10), size = nrow(DataSet), replace = TRUE)
  
  # Create an initial model object
  
  InterceptModel = glm(formula = as.formula('Poisson ~ 1'), family = poisson(link = 'log'), DataSet, weights = DataSet$Weight)

  # Create a cluster for parallel execution
  
  detectCores()
  Cluster = makeCluster(4)

  # Run four scenarios with and without summarization and parallel execution but no reference to variables used to
  # partition the data into training and testing data sets

  set.seed(666)
  
  Results = glmCrossValidation(Model = InterceptModel, Variables = paste('V', seq(100), sep = ''), NFold = 10, TrainingPercentage = .7, Cluster = NULL, PartitioningVariables = NULL, DTthreads = 1, Summarize = FALSE)
  
  set.seed(666)
  
  Results02 = glmCrossValidation(Model = InterceptModel, Variables = paste('V', seq(100), sep = ''), NFold = 10, TrainingPercentage = .7, Cluster = NULL, PartitioningVariables = NULL, DTthreads = 2, Summarize = TRUE)

  set.seed(666)
  
  Results03 = glmCrossValidation(Model = InterceptModel, Variables = paste('V', seq(100), sep = ''), NFold = 10, TrainingPercentage = .7, Cluster = Cluster, PartitioningVariables = NULL, DTthreads = 1, Summarize = FALSE)
  
  set.seed(666)
  
  Results04 = glmCrossValidation(Model = InterceptModel, Variables = paste('V', seq(100), sep = ''), NFold = 10, TrainingPercentage = .7, Cluster = Cluster, PartitioningVariables = NULL, DTthreads = 2, Summarize = TRUE)

  # Observe the results are the same
    
  max(abs(sapply(X = seq(length(Results)), function(i){Results[[i]][['Error']]}) - sapply(X = seq(length(Results02)), function(i){Results02[[i]][['Error']]})))
  max(abs(sapply(X = seq(length(Results02)), function(i){Results02[[i]][['Error']]}) - sapply(X = seq(length(Results03)), function(i){Results03[[i]][['Error']]})))
  max(abs(sapply(X = seq(length(Results03)), function(i){Results03[[i]][['Error']]}) - sapply(X = seq(length(Results04)), function(i){Results04[[i]][['Error']]})))

  # Run four scenarios with and without summarization and parallel execution and reference to variables used to
  # partition the data into training and testing data sets
    
  set.seed(666)
  
  Results = glmCrossValidation(Model = InterceptModel, Variables = paste('V', seq(100), sep = ''), NFold = 10, TrainingPercentage = .7, Cluster = NULL, PartitioningVariables = c('PartitionVariable01', 'PartitionVariable02'), DTthreads = 1, Summarize = FALSE)
  
  set.seed(666)
  
  Results02 = glmCrossValidation(Model = InterceptModel, Variables = paste('V', seq(100), sep = ''), NFold = 10, TrainingPercentage = .7, Cluster = NULL, PartitioningVariables = c('PartitionVariable01', 'PartitionVariable02'), DTthreads = 2, Summarize = TRUE)
  
  set.seed(666)
  
  Results03 = glmCrossValidation(Model = InterceptModel, Variables = paste('V', seq(100), sep = ''), NFold = 10, TrainingPercentage = .7, Cluster = Cluster, PartitioningVariables = c('PartitionVariable01', 'PartitionVariable02'), DTthreads = 1, Summarize = FALSE)
  
  set.seed(666)
  
  Results04 = glmCrossValidation(Model = InterceptModel, Variables = paste('V', seq(100), sep = ''), NFold = 10, TrainingPercentage = .7, Cluster = Cluster, PartitioningVariables = c('PartitionVariable01', 'PartitionVariable02'), DTthreads = 2, Summarize = TRUE)

  # Observe the results are the same
  
  max(abs(sapply(X = seq(length(Results)), function(i){Results[[i]][['Error']]}) - sapply(X = seq(length(Results02)), function(i){Results02[[i]][['Error']]})))
  max(abs(sapply(X = seq(length(Results02)), function(i){Results02[[i]][['Error']]}) - sapply(X = seq(length(Results03)), function(i){Results03[[i]][['Error']]})))
  max(abs(sapply(X = seq(length(Results03)), function(i){Results03[[i]][['Error']]}) - sapply(X = seq(length(Results04)), function(i){Results04[[i]][['Error']]})))

  # Stop the cluster
  
  stopCluster(Cluster)
  
  # Check the connections
  
  showConnections()
 
}