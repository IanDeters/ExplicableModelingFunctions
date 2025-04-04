Discretize = function(Weight = rep(1, length(x)), x, GroupNumber = 100)
{
  
  if (is.null(Weight) == TRUE)
  {
    
    Weight = rep(1, length(x))
    
  }

  # Create an initial data set
    
  DataSet = data.table(Record = seq(length(x)), Weight = Weight, x = x)
  
  # Summarize to the level of the variable in question
  
  DataSet02 = DataSet[order(DataSet$x), .(Weight = sum(Weight)), by = 'x']
  
  # Assign the variable values to their group
  
  DataSet02$Group = pmax(pmin(ceiling(GroupNumber * cumsum(DataSet02$Weight) / sum(DataSet02$Weight)), GroupNumber), 1)
  
  # Merge the group number back in
  
  DataSet = merge(DataSet, DataSet02[, c('x', 'Group')], by = 'x')
  
  # Return the results
  
  DataSet$Group[order(DataSet$Record)]
  
}

if (FALSE)
{
  
  library(data.table)
  
  # Generate some data
  
  set.seed(666)
  
  Weight = runif(1000)
  x = runif(1000)
  
  # Discretize into centiles

  CheckThis = data.table(Weight = Weight, x = x, Group = Discretize(Weight = Weight, x = x, GroupNumber = 100))
 
  # Summarize to the level of the groups
     
  CheckThis02 = CheckThis[order(CheckThis$Group), .(Minimum = min(x), Maximum = max(x), Weight = sum(Weight)), by = 'Group']
  
  # Observe the distribution of weight among the groups
  
  sum(CheckThis02$Weight) / 100
  mean(CheckThis02$Weight)
  sqrt(var(CheckThis02$Weight))
  
  # Observe that there is increasing separation among the groups

  min(CheckThis02$Maximum[seq(2, nrow(CheckThis02))] - CheckThis02$Minimum[seq(1, nrow(CheckThis02) - 1)])
  
}