MinimumWeightImposition = function(Weight, Target, MinimumWeight)
{
  
  # Record the initial vectors
  
  DataSet = data.table(Record = seq(length(Weight)), Weight = Weight, Target = Target)
  
  # Initialize the grouping
  
  DataSet$Group = seq(nrow(DataSet))
  
  # Initialize the interim data set
  
  DataSet02 = DataSet
  
  while ((0 < sum(DataSet02$Weight < MinimumWeight)) == TRUE)
  {
    
    # Determine the indices violating the minimum weight condition
    
    ViolationIndices = seq(length(DataSet02$Weight))[DataSet02$Weight < MinimumWeight]
    
    # Initialize the summary
    
    Summary = data.table()
    
    for (Index in ViolationIndices)
    {
      
      # Find the neighbors of the index in question
      
      Neighbors = pmax(pmin(c(Index - 1, Index + 1), nrow(DataSet02)), 1)
      
      # Exclude the index in question
      
      Neighbors = Neighbors[Neighbors != Index]
      
      for (Neighbor in Neighbors)
      {
        
        # Calculate the average value of the target associated to the indices
                
        GroupedTarget = (DataSet02$Target[Neighbor] * DataSet02$Weight[Neighbor] + DataSet02$Target[Index] * DataSet02$Weight[Index]) / (DataSet02$Weight[Neighbor] + DataSet02$Weight[Index])
        
        # Record the results
        
        Summary = rbind(Summary, data.table(Index = Index, Neighbor = Neighbor, Error = sum((DataSet02$Weight * (GroupedTarget - DataSet02$Target)^2)[c(Neighbor, Index)])))
        
      }

    }
    
    # Initialize the new group
    
    DataSet02$Group02 = DataSet02$Group
    
    # Combine the indices yielding the smallest error
    
    DataSet02$Group02 = DataSet02$Group02 - (min(Summary$Index[Summary$Error == min(Summary$Error)][1], Summary$Neighbor[Summary$Error == min(Summary$Error)][1]) < DataSet02$Group02)

    # Merge the new group into the unsummarized data    

    DataSet = merge(DataSet[, names(DataSet)[names(DataSet) != 'Group02'], with = FALSE], DataSet02[, c('Group', 'Group02')], by = 'Group')    

    # Update the group
    
    DataSet$Group = DataSet$Group02
    
    # Summarize to the level of the new group
    
    DataSet02 = DataSet[order(DataSet$Group), .(Weight = sum(Weight), Target = sum(Weight * Target) / sum(Weight)), by = 'Group']
    
  }
  
  # Return the grouping
  
  DataSet$Group[order(DataSet$Record)]

}

if (FALSE)
{
  
  library(data.table)
  
  # Generate some data
  
  NumberOfObservations = 20
  
  set.seed(666)
  
  Weight = runif(NumberOfObservations)
  Target = rpois(n = NumberOfObservations, lambda = Weight * seq(NumberOfObservations)) / Weight
  
  # Observe that the minimal weight is satisfied
  
  data.table(Weight = Weight, Group = MinimumWeightImposition(Weight, Target, 2))[, .(Weight = sum(Weight)), by = 'Group']

}