glmCrossValidation = function(Model, Variables, NFold = 10, TrainingPercentage = .5, Cluster = NULL)
{
  
  if (is.null(Cluster) == FALSE)
  {
    
    # Register the cluster
    
    registerDoParallel(Cluster)
    
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
    
    TrainingVector = sample(x = c(TRUE, FALSE), size = nrow(Model[['data']]), replace = TRUE, prob = c(TrainingPercentage, 1 - TrainingPercentage))
    
    # Calibrate the current model using the data specified by the training vector
    
    NewModel = glm.fit(x = cbind(rep(1, sum(TrainingVector)), Model[['data']][TrainingVector, CurrentTerms, with = FALSE]), y = Model[['y']][TrainingVector], weights = weights[TrainingVector], offset = Model$offset[TrainingVector], family = Model$family, control = Model$control, start = CurrentCoefficients)
    
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
          
          glm.fit(x = cbind(rep(1, sum(TrainingVector)), Model[['data']][TrainingVector, Terms, with = FALSE]), y = Model[['y']][TrainingVector], weights = weights[TrainingVector], offset = Model$offset[TrainingVector], family = Model$family, control = Model$control)
          
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
  
  # Create an initial model object
  
  InterceptModel = glm(formula = as.formula('Poisson ~ 1'), family = poisson(link = 'log'), DataSet, weights = DataSet$Weight)
  
  # Execute one round of cross validation
  
  Results = glmCrossValidation(Model = InterceptModel, Variables = paste('V', seq(100), sep = ''), NFold = 10, TrainingPercentage = .7, Cluster = NULL)
  
  # Initialize the error and parameter estimate tables
  
  ErrorTable = data.table()
  ParameterEstimateTable = data.table()
  
  for (i in seq(length(Results)))
  {
    
    # Update the error tables
    
    ErrorTable = rbind(ErrorTable, data.table(Variable = Results[[i]][['Variable']], Pass = Results[[i]][['Pass']], Error = Results[[i]][['Error']]))
    
    # Obtain the parameter estimates
    
    ParameterEstimate = as.data.frame(Results[[i]][['ParameterEstimate']])
    
    # Use the row names to record the parameter name
    
    ParameterEstimate$Parameter = row.names(ParameterEstimate)
    
    # Record the variable question
    
    ParameterEstimate$Variable = Results[[i]][['Variable']]
    
    # Record the pass
    
    ParameterEstimate$Pass = Results[[i]][['Pass']]
    
    # Update the parameter estimate table
    
    ParameterEstimateTable = rbind(ParameterEstimateTable, ParameterEstimate)
    
  }
  
  # Merge the incumbent model errors with those of the potential models
  
  ErrorTable02 = merge(ErrorTable[is.na(ErrorTable$Variable) == TRUE, ], ErrorTable[is.na(ErrorTable$Variable) == FALSE, ], by = 'Pass')
  
  # Calculate the average improvement for adding or removing the term and the number of times there was improvement
  
  ErrorTable03 = ErrorTable02[, .(ImprovementCount = sum(Error.y < Error.x), ImprovementMean = mean(Error.x^2 - Error.y^2)), by = 'Variable.y']
  
  # Calculate the number of passes an estimate is less than or equal to zero
  
  SignCheck = ParameterEstimateTable[(is.na(ParameterEstimateTable$Variable) == FALSE) & (ParameterEstimateTable$Parameter == ParameterEstimateTable$Variable), .(SignCount = sum(Estimate <= 0)), by = 'Variable']
  
  # Update the sign count so that 10 implies either positive or negative sign consistency
  
  SignCheck$SignCount = abs(SignCheck$SignCount - 5) + 5
  
  # Order the table by error improvement
  
  ErrorTable03 = ErrorTable03[order(ErrorTable03$ImprovementMean, decreasing = TRUE), ]
  
  # Find the term that most consistently improves the Bible
  
  ChosenTerm = ErrorTable03$Variable.y[(0 < ErrorTable03$ImprovementMean) & (ErrorTable03$Variable.y %in% SignCheck$Variable[SignCheck$SignCount == 10]) & (ErrorTable03$ImprovementCount == 10)][1]

  # The best term is among the predictors
  
  ChosenTerm %in% Predictors
  
  # Initialize the summary table
  
  Summary = data.table()
  
  # Use single threaded glmCrossValidation
  
  StartTime = Sys.time()
  
  for (i in seq(10))
  {
    
    Results = glmCrossValidation(Model = InterceptModel, Variables = paste('V', seq(100), sep = ''), NFold = 10, TrainingPercentage = .7, Cluster = NULL)
    
  }
  
  Summary = rbind(Summary, data.table(nthread = NA, AverageDuration = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))
  
  # Use a multi threaded glmStepwiseSelection
  
  for (j in seq(2, detectCores() - 1))
  {
    
    StartTime = Sys.time()
    
    Cluster = makeCluster(j)
    
    for (i in seq(10))
    {
      
      Results = glmCrossValidation(Model = InterceptModel, Variables = paste('V', seq(100), sep = ''), NFold = 10, TrainingPercentage = .7, Cluster = Cluster)
      
    }
    
    stopCluster(Cluster)
    
    Summary = rbind(Summary, data.table(nthread = j, AverageDuration = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))
    
  }
  
  showConnections()
  
  # Observe the results
  
  Summary
  
}