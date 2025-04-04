IncreasingGroupSearch = function(Weight = rep(1, length(Target)), Target, OrderingVariable = Target, Direction = 1, Tolerance = .0001, MethodList = c('L-BFGS-B', 'BFGS', 'CG'), DiscretizationVector = c(100), RawDiscrete = TRUE, Cluster = NULL)
{

  # Create a data set from the input vectors

  DataSet = data.table(Record = seq(length(Target)), Weight = Weight, Target = Target, OrderingVariable = OrderingVariable)
  
  # Initialize the summary
  
  Summary = list()
  
  if (is.null(Cluster) == FALSE)
  {
    
    # Register the cluster
    
    registerDoParallel(Cluster)
    
    Summary = foreach(j = seq(length(DiscretizationVector)), .combine = c, .export = c('data.table', 'Discretize', 'IncreasingGroups')) %dopar% {

      # Discretize the ordering variable
      
      DataSet$Group = Discretize(Weight = DataSet$Weight, x = DataSet$OrderingVariable, GroupNumber = DiscretizationVector[j])
      
      # Use the discretized ordering variable as the ordering variable in IncreasingGroups
      
      DataSet$IncreasingGroup = IncreasingGroups(OrderingVariable = DataSet$Group, Weight = DataSet$Weight, Target = DataSet$Target, Tolerance = Tolerance, Direction = Direction)
      
      # Merge in the average target value for each group
      
      DataSet = merge(DataSet[, names(DataSet)[names(DataSet) != 'GroupedTarget'], with = FALSE], DataSet[, .(GroupedTarget = sum(Weight * Target) / sum(Weight)), by = 'IncreasingGroup'], by = 'IncreasingGroup')
      
      # Record the group number, grouping, and associated error
      
      list(list(Discretization = DiscretizationVector[j], Grouping = DataSet$IncreasingGroup[order(DataSet$Record)], Error = sum(DataSet$Weight * (DataSet$Target - DataSet$GroupedTarget)^2)))

    }
  }
  
  else
  {
  
    for (GroupNumber in DiscretizationVector)
    {
      
      # Discretize the ordering variable
      
      DataSet$Group = Discretize(Weight = DataSet$Weight, x = DataSet$OrderingVariable, GroupNumber = GroupNumber)
      
      # Use the discretized ordering variable as the ordering variable in IncreasingGroups
      
      DataSet$IncreasingGroup = IncreasingGroups(OrderingVariable = DataSet$Group, Weight = DataSet$Weight, Target = DataSet$Target, Tolerance = Tolerance, Direction = Direction)
      
      # Merge in the average target value for each group
      
      DataSet = merge(DataSet[, names(DataSet)[names(DataSet) != 'GroupedTarget'], with = FALSE], DataSet[, .(GroupedTarget = sum(Weight * Target) / sum(Weight)), by = 'IncreasingGroup'], by = 'IncreasingGroup')
      
      # Record the group number, grouping, and associated error
      
      Summary = c(Summary, list(list(Discretization = GroupNumber, Grouping = DataSet$IncreasingGroup[order(DataSet$Record)], Error = sum(DataSet$Weight * (DataSet$Target - DataSet$GroupedTarget)^2))))
  
    }
    
  }
  
  if (RawDiscrete == TRUE)
  {
    
    # Use the raw ordering variable in IncreasingGroups
    
    DataSet$IncreasingGroup = IncreasingGroups(OrderingVariable = DataSet$OrderingVariable, Weight = DataSet$Weight, Target = DataSet$Target, Tolerance = Tolerance, Direction = Direction)
    
    # Merge in the average target value for each group
    
    DataSet = merge(DataSet[, names(DataSet)[names(DataSet) != 'GroupedTarget'], with = FALSE], DataSet[, .(GroupedTarget = sum(Weight * Target) / sum(Weight)), by = 'IncreasingGroup'], by = 'IncreasingGroup')
    
    # Declare the group number to be 0 and record the grouping and associated error
    
    Summary = c(Summary, list(list(Discretization = 0, Grouping = DataSet$IncreasingGroup[order(DataSet$Record)], Error = sum(DataSet$Weight * (DataSet$Target - DataSet$GroupedTarget)^2))))

  }
  
  # Create the error summary
  
  ErrorSummary = as.data.table(t(sapply(seq(length(Summary)), function(i){c(i, Summary[[i]][['Error']])})))
  
  # Return the results
  
  Summary[[ErrorSummary$V1[ErrorSummary$V2 == min(ErrorSummary$V2)][1]]]

}

if (FALSE)
{
  
  library(data.table)
  library(doParallel)
  
  # Generate the data
  
  set.seed(666)
  
  NumberOfObservations = 2000
  
  OrderingVariable = seq(NumberOfObservations)
  Weight = runif(NumberOfObservations)
  Target = rnorm(NumberOfObservations, seq(NumberOfObservations) * Weight, sd = 10 * sqrt(Weight)) / Weight
  
  # Check the construction
  
  summary(glm(formula = 'Target ~ OrderingVariable', data = data.table(Target = Target, Weight = Weight), family = gaussian(link = 'identity'), weights = Weight))
  
  # Apply the function
  
  StartTime = Sys.time()
  
  CheckThis = IncreasingGroupSearch(Weight = Weight, Target = Target, OrderingVariable = OrderingVariable, Direction = 1, Tolerance = .0001, MethodList = c('L-BFGS-B', 'BFGS', 'CG'), DiscretizationVector = seq(10, 1000, by = 10), RawDiscrete = TRUE, Cluster = NULL)
  
  EndTime = Sys.time()
  
  # Observe the duration
  
  EndTime - StartTime
  
  # Create a table to check the results
  
  CheckThis02 = data.table(OrderingVariable = OrderingVariable, Weight = Weight, Target = Target, Group = Discretize(Weight = Weight, x = OrderingVariable, GroupNumber = CheckThis[['Discretization']]), IncreasingGroup = CheckThis[['Grouping']])

  # Observe that the increasing groups are in agreement
  
  max(abs(CheckThis02$IncreasingGroup - IncreasingGroups(OrderingVariable = CheckThis02$Group, Weight = Weight, Target = Target, Tolerance = .0001, Direction = 1)))
  
  # Merge in the average value of the target variable by group
  
  CheckThis02 = merge(CheckThis02, CheckThis02[, .(GroupedTarget = sum(Weight * Target) / sum(Weight)), by = 'IncreasingGroup'], by = 'IncreasingGroup')

  # Observe the error is recovered
  
  abs(sum(CheckThis02$Weight * (CheckThis02$Target - CheckThis02$GroupedTarget)^2) - CheckThis[['Error']])
  
  # Reserve the relationship of the ordering variable and the target
  
  Target = (-1) * Target
  
  # Apply the function
  
  StartTime = Sys.time()
  
  CheckThis = IncreasingGroupSearch(Weight = Weight, Target = Target, OrderingVariable = OrderingVariable, Direction = -1, Tolerance = .0001, MethodList = c('L-BFGS-B', 'BFGS', 'CG'), DiscretizationVector = seq(10, 1000, by = 10), Cluster = NULL)
  
  EndTime = Sys.time()
  
  # Observe the duration
  
  EndTime - StartTime
  
  # Create a table to check the results
  
  CheckThis02 = data.table(OrderingVariable = OrderingVariable, Weight = Weight, Target = Target, Group = Discretize(Weight = Weight, x = OrderingVariable, GroupNumber = CheckThis[['Discretization']]), IncreasingGroup = CheckThis[['Grouping']])
  
  # Observe that the increasing groups are in agreement
  
  max(abs(CheckThis02$IncreasingGroup - IncreasingGroups(OrderingVariable = CheckThis02$Group, Weight = Weight, Target = Target, Tolerance = .0001, Direction = -1)))
  
  # Merge in the average value of the target variable by group
  
  CheckThis02 = merge(CheckThis02, CheckThis02[, .(GroupedTarget = sum(Weight * Target) / sum(Weight)), by = 'IncreasingGroup'], by = 'IncreasingGroup')
  
  # Observe the error is recovered
  
  abs(sum(CheckThis02$Weight * (CheckThis02$Target - CheckThis02$GroupedTarget)^2) - CheckThis[['Error']])
  
  # Reduce the size of the data for faster examples
  
  NumberOfObservations = 1000
  
  OrderingVariable = seq(NumberOfObservations)
  Weight = runif(NumberOfObservations)
  Target = rnorm(NumberOfObservations, seq(NumberOfObservations) * Weight, sd = 10 * sqrt(Weight)) / Weight
  
  # Check the construction
  
  summary(glm(formula = 'Target ~ OrderingVariable', data = data.table(Target = Target, Weight = Weight), family = gaussian(link = 'identity'), weights = Weight))
  
  # Use single threaded IncreasingGroupSearch
  
  Summary = data.table()
  
  StartTime = Sys.time()
  
  for (i in seq(10))
  {
    
    Results = IncreasingGroupSearch(Weight = Weight, Target = Target, OrderingVariable = OrderingVariable, Direction = 1, Tolerance = .0001, MethodList = c('L-BFGS-B', 'BFGS', 'CG'), DiscretizationVector = seq(5, 500, by = 5), RawDiscrete = FALSE, Cluster = NULL)
    
  }
  
  Summary = rbind(Summary, data.table(nthread = 1, AverageDuration = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))
  
  # Use a multi threaded IncreasingGroupSearch
  
  for (j in seq(2, detectCores() - 1))
  {
    
    Cluster = makeCluster(j)

    StartTime = Sys.time()
    
    for (i in seq(10))
    {
      
      Results = IncreasingGroupSearch(Weight = Weight, Target = Target, OrderingVariable = OrderingVariable, Direction = 1, Tolerance = .0001, MethodList = c('L-BFGS-B', 'BFGS', 'CG'), DiscretizationVector = seq(10, 1000, by = 10), RawDiscrete = FALSE, Cluster = Cluster)
      
    }
    
    Summary = rbind(Summary, data.table(nthread = j, AverageDuration = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))
    
    stopCluster(Cluster)
    rm(Cluster)
    
  }
  
  # Observe the results
  
  Summary
  
  showConnections()
  
}