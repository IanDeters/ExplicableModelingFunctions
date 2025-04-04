IncreasingGroups = function(Weight = rep(1, length(Target)), Target, OrderingVariable = Target, Direction = 1, Tolerance = .0001, MethodList = c('L-BFGS-B', 'BFGS', 'CG'))
{

  # Create a data set from the input vectors
  
  DataSet = data.table(Record = seq(length(Target)), Weight = Weight, Target = Target * Direction, OrderingVariable = OrderingVariable)
  
  # Shift the target
  
  DataSet$Target = DataSet$Target - min(DataSet$Target)
  
  # Summarize to the level of the ordering variable
  
  DataSet02 = DataSet[order(DataSet$OrderingVariable), .(Weight = sum(Weight), Target = sum(Weight * Target) / sum(Weight)), by = 'OrderingVariable']
  
  # Check if there is only one value of the ordering variable
  
  if (nrow(DataSet02) == 1)
  {
  
    return(rep(1, length(Target)))  

  }
  
  # Initialize the groups
  
  DataSet02$Group = seq(1, nrow(DataSet02))
  
  # Check if the order is satisfied

  LoopingCondition = (min(round(DataSet02$Target[seq(2, nrow(DataSet02))] - DataSet02$Target[seq(1, nrow(DataSet02) - 1)], ceiling((-1) * log10(Tolerance)))) < 0)

  while (LoopingCondition)
  {
    
    # Summarize to the level of the current groups
    
    DataSet03 = DataSet02[order(DataSet02$Group), .(Weight = sum(Weight), Target = sum(Weight * Target) / sum(Weight)), by = 'Group']
    
    # Record the current number of groups
    
    CurrentGroupNumber = nrow(DataSet03)
    
    # Create auxiliary triangular matrices
    
    LowerTriangularMatrix = lower.tri(diag(x = DataSet03$Target), diag = TRUE)
    UpperTriangularMatrix = t(LowerTriangularMatrix)
    
    # Create the objective function
    
    ObjectiveFunction = function(x){sum(DataSet03$Weight * (LowerTriangularMatrix %*% (x^2) - DataSet03$Target)^2)}
    
    # Create the gradient of the objective function
    
    ObjectiveFunctionGradient = function(x)
    {
      
      4 * x * UpperTriangularMatrix %*% (DataSet03$Weight * (LowerTriangularMatrix %*% (x^2) - DataSet03$Target))
      
    }
    
    # Record the cumulative maximum
    
    DataSet03$CumulativeMaximum = cummax(DataSet03$Target)
    
    # Calculate the weighted average of the target for each value of the cumulative maximum
    
    DataSet03 = merge(DataSet03, DataSet03[, .(InitialValue = sum(Weight * Target) / sum(Weight)), by = 'CumulativeMaximum'], by = 'CumulativeMaximum')
    
    # Order by the group
    
    DataSet03 = DataSet03[order(DataSet03$Group), ]
    
    # Create the initial value
    
    InitialValue = c(DataSet03$InitialValue[1], DataSet03$InitialValue[seq(2, nrow(DataSet03))] - DataSet03$InitialValue[seq(1, nrow(DataSet03) - 1)])
    InitialValue = pmax(InitialValue, 0)
    InitialValue = sqrt(InitialValue)

    # Initialize the summary of the method results
    
    SummaryOfTheMethodResults = data.table()
    
    # Attempt to find a minimum
    
    for (Method in MethodList)
    {
      
      # Check if the method is the L-BFGS-B method
      
      if (Method == 'L-BFGS-B')
      {
        
        Results = try(optim(par = InitialValue, fn = ObjectiveFunction, gr = ObjectiveFunctionGradient, method = Method, lower = rep(0, length(InitialValue)), upper = rep(sqrt(max(DataSet03$Target)), length(InitialValue)), hessian = FALSE))
        
      }
      
      else
      {
        
        Results = try(optim(par = InitialValue, fn = ObjectiveFunction, gr = ObjectiveFunctionGradient, method = Method, hessian = FALSE))

      }

      # If the optimization failed, simply use the initial value and record the value of the objective function
      # at it
            
      if (class(Results) == 'try-error')
      {
        
        SummaryOfTheMethodResults = rbind(SummaryOfTheMethodResults, data.table(Method = Method, par = InitialValue, value = rep(ObjectiveFunction(InitialValue), nrow(DataSet03))))

      }
      
      # Update the summary
      
      else
      {
        
        SummaryOfTheMethodResults = rbind(SummaryOfTheMethodResults, data.table(Method = Method, par = Results$par, value = rep(Results$value, nrow(DataSet03))))

      }
      
    }

    # Record the parameter
    
    DataSet03$par = SummaryOfTheMethodResults$par[SummaryOfTheMethodResults$Method == SummaryOfTheMethodResults$Method[SummaryOfTheMethodResults$value == min(SummaryOfTheMethodResults$value)][1]]
    
    # Record the grouped target
    
    DataSet03$GroupedTarget = round(LowerTriangularMatrix %*% (DataSet03$par^2), ceiling((-1) * log10(Tolerance)))
    
    # Check if there has been a reduction in the number of groups
    
    if (CurrentGroupNumber == length(unique(DataSet03$GroupedTarget)))
    {
      
      # Calculate the averages of adjacent pairs
      
      AverageOfAdjacentPairs = (DataSet03$Target[seq(1, nrow(DataSet03) - 1)] * DataSet03$Weight[seq(1, nrow(DataSet03) - 1)] + DataSet03$Target[seq(2, nrow(DataSet03))] * DataSet03$Weight[seq(2, nrow(DataSet03))]) / (DataSet03$Weight[seq(1, nrow(DataSet03) - 1)] + DataSet03$Weight[seq(2, nrow(DataSet03))])
      
      # Calculate the errors associated with the averages
      
      Errors = DataSet03$Weight[seq(1, nrow(DataSet03) - 1)] * (AverageOfAdjacentPairs - DataSet03$Target[seq(1, nrow(DataSet03) - 1)])^2
      Errors = Errors + DataSet03$Weight[seq(2, nrow(DataSet03))] * (AverageOfAdjacentPairs - DataSet03$Target[seq(2, nrow(DataSet03))])^2
      
      # Determine where monotoncity fails
      
      MonotoncityFailures = seq(nrow(DataSet03) - 1) * (DataSet03$Target[seq(1, nrow(DataSet03) - 1)] > DataSet03$Target[seq(2, nrow(DataSet03))])
      MonotoncityFailures = MonotoncityFailures[0 < MonotoncityFailures]
      
      # Determine which index will be combined with its neighbor
      
      CombinationIndex = MonotoncityFailures[Errors[MonotoncityFailures] == min(Errors[MonotoncityFailures])][1]
      
      # Initialize the new grouped target
      
      DataSet03$GroupedTarget = DataSet03$Target
      
      # Combine the indicated indices
      
      DataSet03$GroupedTarget[c(CombinationIndex, CombinationIndex + 1)] = (DataSet03$GroupedTarget[CombinationIndex] * DataSet03$Weight[CombinationIndex] + DataSet03$GroupedTarget[CombinationIndex + 1] * DataSet03$Weight[CombinationIndex + 1]) / (DataSet03$Weight[CombinationIndex] + DataSet03$Weight[CombinationIndex + 1])

      # Apply the desired rounding
            
      DataSet03$GroupedTarget = round(DataSet03$GroupedTarget, ceiling((-1) * log10(Tolerance)))

    }
    
    if (length(unique(DataSet03$GroupedTarget)) == 1)
    {
      
      # Only one group remains
      
      DataSet02$Group = 1
      
      # Update the looping condition
      
      LoopingCondition = FALSE
      
    }
    
    else
    {
      
      # Summarize to level of the grouped target
      
      DataSet03 = merge(DataSet03, DataSet03[, .(Target02 = sum(Weight * Target) / sum(Weight)), by = 'GroupedTarget'], by = 'GroupedTarget')

      # Update the group variable
      
      DataSet02 = merge(DataSet02[, names(DataSet02)[names(DataSet02) != 'Target02'], with = FALSE], DataSet03[, c('Group', 'Target02')], by = 'Group')
      
      # Order by the group variable
      
      DataSet02 = DataSet02[order(DataSet02$Group), ]
      
      # Update the group variable
      
      DataSet02$Group = cumsum(c(TRUE, DataSet02$Target02[seq(2, nrow(DataSet02))] != DataSet02$Target02[seq(1, nrow(DataSet02) - 1)]))
            
      # Order by the group
      
      DataSet03 = DataSet03[order(DataSet03$Group), ]
      
      # Update the looping condition
      
      LoopingCondition = (min(round(DataSet03$Target02[seq(2, nrow(DataSet03))] - DataSet03$Target02[seq(1, nrow(DataSet03) - 1)], ceiling((-1) * log10(Tolerance)))) < 0)
      
    }
        
  }
  
  # Merge the group into the raw data

  DataSet = merge(DataSet, DataSet02[, c('OrderingVariable', 'Group')], by = 'OrderingVariable')
  
  # Return the groups
  
  DataSet$Group[order(DataSet$Record)]
    
}

if (FALSE)
{
  
  library(data.table)
  
  # Generate the data
  
  set.seed(666)
  
  NumberOfObservations = 2000
  
  OrderingVariable = seq(NumberOfObservations)
  Weight = runif(NumberOfObservations)
  Target = rnorm(NumberOfObservations, seq(NumberOfObservations), sd = 1)
  
  # Apply the function
  
  StartTime = Sys.time()
  
  CheckThis = IncreasingGroups(OrderingVariable, Weight = Weight, Target = Target, Tolerance = .0001, Direction = 1)
  
  EndTime = Sys.time()
  
  # Observe the duration
  
  EndTime - StartTime

  # Summarize to the level of the grouping
  
  CheckThis02 = data.table(Weight = Weight, Target = Target, OrderingVariable = OrderingVariable, Group = CheckThis)[, .(Weight = sum(Weight), OrderingVariable = sum(Weight * OrderingVariable) / sum(Weight), Target = sum(Weight * Target) / sum(Weight)), by = 'Group']
  
  # Observe the results
  
  as.data.frame(CheckThis02)[, c('Group', 'Target')]
  
  min(CheckThis02$Target[seq(2, nrow(CheckThis02))] - CheckThis02$Target[seq(nrow(CheckThis02) - 1)])

  # Reserve the relationship of the ordering variable and the target
    
  Target = (-1) * Target
  
  # Apply the function
  
  StartTime = Sys.time()
  
  CheckThis = IncreasingGroups(OrderingVariable, Weight = Weight, Target = Target, Tolerance = .0001, Direction = -1)
  
  EndTime = Sys.time()
  
  # Observe the duration
  
  EndTime - StartTime
  
  # Summarize to the level of the grouping
  
  CheckThis02 = data.table(Weight = Weight, Target = Target, OrderingVariable = OrderingVariable, Group = CheckThis)[, .(Weight = sum(Weight), OrderingVariable = sum(Weight * OrderingVariable) / sum(Weight), Target = sum(Weight * Target) / sum(Weight)), by = 'Group']
  
  # Observe the results
  
  as.data.frame(CheckThis02)[order(CheckThis02$Group), c('Group', 'Target')]
  
  max(CheckThis02$Target[seq(2, nrow(CheckThis02))] - CheckThis02$Target[seq(nrow(CheckThis02) - 1)])
  
}