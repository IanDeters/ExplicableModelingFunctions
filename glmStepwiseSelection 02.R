glmStepwiseSelection = function(Model, Variables, Significance = .95, MaximumIterations = 50, FullRatioTest = FALSE, Cluster = NULL, SaveFile = NULL)
{
  
  # Extract call from the model object
  
  call = summary(Model)[['call']]
  
  # Extract the component which contains the variance power
  
  call = call[grepl('var.power', call)]
  
  if (is.null(call) == TRUE)
  {
    
    # If there is no variance power, set it to the default value
    
    TweediePower = 0
    
  }
  
  else
  {
    
    # Create a string
    
    call = toString(call)
    
    # Find where the var.power string ends
    
    call = substr(call, regexpr('var.power', call)[1] + 9, nchar(call))
    
    # Find where the equal sign following the var.power ends
    
    call = substr(call, regexpr('=', call)[1] + 1, nchar(call))
    
    # Extract the variance power
    
    TweediePower = as.numeric(substr(call, 1, min(regexpr(',', call)[1], regexpr(')', call)[1]) - 1))
    
  }

  if (is.null(Cluster) == FALSE)
  {
    
    # Register the cluster
    
    registerDoParallel(Cluster)
    
  }
  
  # Record the current coefficients
  
  CurrentCoefficients = Model$coefficients
  
  # Extract the terms from the model formula
  
  CurrentTerms = names(CurrentCoefficients)
  CurrentTerms = CurrentTerms[CurrentTerms != '(Intercept)']
  
  # Extract the deviance from the current model
  
  CurrentDeviance = Model$deviance
  
  # Extract the current dispersion parameter
  
  CurrentDispersionParameter = summary.glm(Model)[['dispersion']]
  
  # Extract the weights from the model object
  
  weights = Model$prior.weights

  # Initialize the cross validation history
  
  StepwiseSelectionHistory = list()
  
  # Initialize the round
  
  Round = 1
  
  # Initialize the looping condition

  LoopingCondition = TRUE

  while (LoopingCondition)
  {

    if (is.null(Cluster) == FALSE)
    {
      
      Summary = foreach(j = seq(length(Variables)), .export = c('data.table')) %dopar% {

        # Obtain the variable in question
        
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
        
        NewModel = try(glm.fit(x = cbind(rep(1, length(weights)), Model[['data']][, Terms, with = FALSE]), y = Model[['y']], weights = weights, offset = Model$offset, family = Model$family, control = Model$control, start = start))
        
        # If an error was thrown, try using the default initial values
        
        if (class(NewModel) == 'try-error')
        {
          
          NewModel = glm.fit(x = cbind(rep(1, length(weights)), Model[['data']][, Terms, with = FALSE]), y = Model[['y']], weights = weights, offset = Model$offset, family = Model$family, control = Model$control)
          
        }
        
        # Record -2 * the log likelihood difference of the new model and update the summary
        
        if (FullRatioTest == TRUE)
        {
          
          if (toString(NewModel[['family']][1]) == 'gaussian')
          {
            
            Summary = data.table(Variable = Variable, NegativeTwoLogLikelihoodDifference = CurrentDeviance / CurrentDispersionParameter - NewModel$deviance / summary.glm(NewModel)[['dispersion']] + log(CurrentDispersionParameter / summary.glm(NewModel)[['dispersion']]) * nrow(Model[['data']]), ParameterNumber = length(NewModel$coefficients))
            
          }
          
          if (toString(NewModel[['family']][1]) == 'Gamma')
          {
            
            Summary = data.table(Variable = Variable, NegativeTwoLogLikelihoodDifference = CurrentDeviance / CurrentDispersionParameter - NewModel$deviance / summary.glm(NewModel)[['dispersion']] + 2 * sum(weights) * ((1 + log(CurrentDispersionParameter)) / CurrentDispersionParameter - (1 + log(summary.glm(NewModel)[['dispersion']])) / summary.glm(NewModel)[['dispersion']]) + 2 * sum(lgamma(weights / CurrentDispersionParameter) - lgamma(weights / summary.glm(NewModel)[['dispersion']])) - 2 * sum(weights * log(weights)) * (1 / CurrentDispersionParameter - 1 / summary.glm(NewModel)[['dispersion']]), ParameterNumber = length(NewModel$coefficients))
            
          }
          
          else
          {
            
            Summary = data.table(Variable = Variable, NegativeTwoLogLikelihoodDifference = (CurrentDeviance - NewModel$deviance) / summary.glm(NewModel)[['dispersion']], ParameterNumber = length(NewModel$coefficients))
            
          }
          
        }
        
        else
        {
          
          Summary = data.table(Variable = Variable, Deviance = NewModel$deviance, DispersionParameter = summary.glm(NewModel)[['dispersion']], NegativeTwoLogLikelihoodDifference = (CurrentDeviance - NewModel$deviance) / summary.glm(NewModel)[['dispersion']], ParameterNumber = length(NewModel$coefficients))
          
        }
        
        # Extract the coefficient vector
        
        CoefficientVector = NewModel[['coefficients']]
        CoefficientVector = c(NA, CoefficientVector)
        names(CoefficientVector)[seq(2)] = c(Variable, '(Intercept)')
        
        # Return the results
        
        list(Summary, CoefficientVector)

      }

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

        NewModel = try(glm.fit(x = cbind(rep(1, length(weights)), Model[['data']][, Terms, with = FALSE]), y = Model[['y']], weights = weights, offset = Model$offset, family = Model$family, control = Model$control, start = start))

        # If an error was thrown, try using the default initial values
                
        if (class(NewModel) == 'try-error')
        {
          
          NewModel = glm.fit(x = cbind(rep(1, length(weights)), Model[['data']][, Terms, with = FALSE]), y = Model[['y']], weights = weights, offset = Model$offset, family = Model$family, control = Model$control)

        }

        # Record -2 * the log likelihood difference of the new model and update the summary

        if (FullRatioTest == TRUE)
        {
          
          if (toString(NewModel[['family']][1]) == 'gaussian')
          {
            
            Summary = data.table(Variable = Variable, NegativeTwoLogLikelihoodDifference = CurrentDeviance / CurrentDispersionParameter - NewModel$deviance / summary.glm(NewModel)[['dispersion']] + log(CurrentDispersionParameter / summary.glm(NewModel)[['dispersion']]) * nrow(Model[['data']]), ParameterNumber = length(NewModel$coefficients))

          }
          
          if (toString(NewModel[['family']][1]) == 'Gamma')
          {
            
            Summary = data.table(Variable = Variable, NegativeTwoLogLikelihoodDifference = CurrentDeviance / CurrentDispersionParameter - NewModel$deviance / summary.glm(NewModel)[['dispersion']] + 2 * sum(weights) * ((1 + log(CurrentDispersionParameter)) / CurrentDispersionParameter - (1 + log(summary.glm(NewModel)[['dispersion']])) / summary.glm(NewModel)[['dispersion']]) + 2 * sum(lgamma(weights / CurrentDispersionParameter) - lgamma(weights / summary.glm(NewModel)[['dispersion']])) - 2 * sum(weights * log(weights)) * (1 / CurrentDispersionParameter - 1 / summary.glm(NewModel)[['dispersion']]), ParameterNumber = length(NewModel$coefficients))

          }
          
          else
          {
            
            Summary = data.table(Variable = Variable, NegativeTwoLogLikelihoodDifference = (CurrentDeviance - NewModel$deviance) / summary.glm(NewModel)[['dispersion']], ParameterNumber = length(NewModel$coefficients))

          }
          
        }
        
        else
        {
          
          Summary = data.table(Variable = Variable, Deviance = NewModel$deviance, DispersionParameter = summary.glm(NewModel)[['dispersion']], NegativeTwoLogLikelihoodDifference = (CurrentDeviance - NewModel$deviance) / summary.glm(NewModel)[['dispersion']], ParameterNumber = length(NewModel$coefficients))

        }

        # Extract the coefficient vector
        
        CoefficientVector = NewModel[['coefficients']]
        CoefficientVector = c(NA, CoefficientVector)
        names(CoefficientVector)[seq(2)] = c(Variable, '(Intercept)')

        # Return the results
        
        list(Summary, CoefficientVector)

      }
      
      Summary = lapply(Variables, CheckTerm)

    }

    # Extract the vectors of coefficients
    
    CoefficientVectorList = lapply(seq(length(Summary)), function(i){Summary[[i]][[2]]})
    
    # Extract the differences of negative two times the log likelihood
    
    Summary = do.call(rbind, lapply(seq(length(Summary)), function(i){Summary[[i]][[1]]}))
    
    # Initialize the chosen term to add or remove
    
    ChosenTerm = NA
    
    # Sort the summary by the negative log likelihood differences
    
    Summary = Summary[order(Summary$NegativeTwoLogLikelihoodDifference, decreasing = TRUE), ]
    
    for (ThisVariable in Summary$Variable)
    {
      
      # Check if there was improvement

      if ((0 < Summary$NegativeTwoLogLikelihoodDifference[Summary$Variable == ThisVariable]) == TRUE)
      {
        
        # Check if the improvement had more parameters than the incumbent model

        if ((length(Model$coefficients) < Summary$ParameterNumber[Summary$Variable == ThisVariable]) == TRUE)
        {
          
          # Check if the improvement was statistically significant
          
          if ((qchisq(p = Significance, df = Summary$ParameterNumber[Summary$Variable == ThisVariable] - length(Model$coefficients)) < Summary$NegativeTwoLogLikelihoodDifference[Summary$Variable == ThisVariable]) == TRUE)
          {
            
            # Obtain the term which yields the greatest improvement
            
            ChosenTerm = ThisVariable
            
          }
          
        }
        
        # In this case there is model improvement through the removal of a variable
        
        else
        {
          
          # Obtain the term which yields the greatest improvement
          
          ChosenTerm = ThisVariable
          
        }
        
      }

      else
      {
       
        # Check if the deterioration had fewer parameters than the incumbent model
        
        if ((Summary$ParameterNumber[Summary$Variable == ThisVariable] < length(Model$coefficients)) == TRUE)
        {
          
          # Check if the deterioration was statistically insignificant
          
          if (((-1) * Summary$NegativeTwoLogLikelihoodDifference[Summary$Variable == ThisVariable] < qchisq(p = Significance, df = length(Model$coefficients) - Summary$ParameterNumber[Summary$Variable == ThisVariable])) == TRUE)
          {
            
            # Obtain the term which yields the greatest improvement
            
            ChosenTerm = ThisVariable
            
          }
          
        }

      }
      
      (is.na(ChosenTerm) == FALSE)
      {
        
        break
        
      }
      
    }

    if (is.na(ChosenTerm) == FALSE)
    {
      
      # Update the current terms
      
      CurrentTerms = c(CurrentTerms[CurrentTerms != ChosenTerm], ifelse(ChosenTerm %in% CurrentTerms, c(), ChosenTerm))

      # Update the current deviance and dispersion parameter
      
      CurrentDeviance = Summary$Deviance[Summary$Variable == ChosenTerm]
      CurrentDispersionParameter = Summary$DispersionParameter[Summary$Variable == ChosenTerm]
      
      # Update the current coefficients
      
      CurrentCoefficients = CoefficientVectorList[[seq(length(CoefficientVectorList))[sapply(seq(length(CoefficientVectorList)), function(i){names(CoefficientVectorList[[i]])[1] == ChosenTerm})][1]]]
      CurrentCoefficients = CurrentCoefficients[c(FALSE, rep(TRUE, length(CurrentCoefficients) - 1))]

      # Initialize the check to see if models are being repeated
      
      LoopingCheck = c()
      
      if (0 < length(StepwiseSelectionHistory))
      {
        
        for (i in seq(length(StepwiseSelectionHistory)))
        {
          
          # Check if the terms of the current model are a subset of a previous model
          
          LoopingCheck = c(LoopingCheck, as.logical(prod(c('(Intercept)', CurrentTerms) %in% names(StepwiseSelectionHistory[[i]][['SelectedParameters']]))))
          
          # Check if the terms of a previous model are a subset of the current model
          
          LoopingCheck[length(LoopingCheck)] = LoopingCheck[length(LoopingCheck)] & as.logical(prod(StepwiseSelectionHistory[[i]][['SelectedParameters']] %in% c('(Intercept)', CurrentTerms)))
          
        }
        
      }
      
      else
      {
        
        LoopingCheck = FALSE
        
      }
      
      # If both of the above are true for a previous model, then the selection is looping
      
      if (as.logical(pmin(sum(LoopingCheck), 1)) == TRUE)
      {
        
        LoopingCondition = FALSE
        
      }
      
      # Update the stepwise history
      
      StepwiseSelectionHistory = c(StepwiseSelectionHistory, list(list(ErrorTable = Summary, SelectedParameters = CurrentCoefficients)))
      
      # If a file location was given, record the results thus far
      
      if (is.null(SaveFile) == FALSE)
      {
        
        fwrite(do.call(rbind, lapply(seq(length(StepwiseSelectionHistory)), function(i){data.table(Round = i, Parameter = names(StepwiseSelectionHistory[[i]][['SelectedParameters']]), Estimate = StepwiseSelectionHistory[[i]][['SelectedParameters']])})), file = SaveFile)
        
      }

    }
    
    else
    {
      
      LoopingCondition = FALSE
      
    }

    # Update the round
    
    Round = Round + 1
    
    if (MaximumIterations < Round)
    {
      
      LoopingCondition = FALSE
      
    }

  }

  # Return the results
   
  StepwiseSelectionHistory

}

if (FALSE)
{
  
  library(data.table)
  library(MASS)
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

  # Create a scope for stepAIC
  
  glmFormula = as.formula(paste('Poisson ~', paste(paste('V', seq(100), sep = ''), collapse = ' + ')))
  
  # Initialize the summary table

  Summary = data.table()
      
  # Use step AIC
  
  StartTime = Sys.time()
  
  for (i in seq(10))
  {
    
    Results = stepAIC(InterceptModel, scope = glmFormula, direction = 'both', k = 4, steps = 61)

  }
  
  Summary = rbind(Summary, data.table(nthread = NA, AverageDuration = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))

  # Use single threaded glmStepwiseSelection
  
  StartTime = Sys.time()
  
  for (i in seq(10))
  {
    
    Results = glmStepwiseSelection(Model = InterceptModel, Variables = paste('V', seq(100), sep = ''), Significance = .95, MaximumIterations = 100, FullRatioTest = FALSE, Cluster = NULL, SaveFile = NULL)
    
  }
  
  Summary = rbind(Summary, data.table(nthread = 1, AverageDuration = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))

  # Use a multi threaded glmStepwiseSelection
    
  for (j in seq(2, detectCores() - 1))
  {
    
    Cluster = makeCluster(j)
    
    StartTime = Sys.time()
    
    for (i in seq(10))
    {
      
      Results = glmStepwiseSelection(Model = InterceptModel, Variables = paste('V', seq(100), sep = ''), Significance = .95, MaximumIterations = 100, FullRatioTest = FALSE, Cluster = Cluster, SaveFile = NULL)
      
    }
    
    Summary = rbind(Summary, data.table(nthread = j, AverageDuration = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))
    
    stopCluster(Cluster)

  }
  
  showConnections()

  # Observe the results
  
  Summary

}