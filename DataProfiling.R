DataProfiling = function(DataSet, UnivariateSummarySize = 100, NumericThreshold = .99)
{
 
  # Record the original record count
   
  RecordCount = nrow(DataSet)
  
  # Remove any duplicate records
  
  DataSet = unique(DataSet)
  
  # Record if there were duplicate records
  
  Output = list(DuplicateRecordFlag = as.integer(nrow(DataSet) < RecordCount))
  
  # Initialize the set of columns to be considered in composite key
  
  RemainingColumns = names(DataSet)

  # Initialize the looping condition
    
  LoopingCondition = TRUE
  
  # Initialize the composite key
  
  Key = c()
  
  while(LoopingCondition)
  {
    
    # Initialize the key summary
    
    KeySummary = data.table()
    
    for (Column in RemainingColumns)
    {
      
      # Count the number of unique coumbinations for the key
      
      KeySummary = rbind(KeySummary, data.table(Column = Column, Count = nrow(unique(DataSet[, c(Key, Column), with = FALSE]))))
      
    }
    
    # Update the key with the column yielding the largest number of unique combinations
    
    Key = c(Key, KeySummary$Column[KeySummary$Count == max(KeySummary$Count)][1])

    # Update the remaining columns to consider
    
    RemainingColumns = RemainingColumns[RemainingColumns != KeySummary$Column[KeySummary$Count == max(KeySummary$Count)][1]]

    # Check if the key is a composite key
        
    LoopingCondition = max(KeySummary$Count) < nrow(DataSet)

  }
  
  # Record the composite key
  
  Output = c(Output, list(Key = Key))
  
  # Initialize the meta data
  
  MetaData = data.table(Variable = names(DataSet), ConceptualType = 'Continuous')
  
  # Suppress the warnings from attempted numeric conversions

  suppressWarnings({for (Column in RemainingColumns)
  {
    
    # Obtain a count of each of the distinct values from the data
    
    VariableSummary = DataSet[, .(Count = .N), by = c(Column)]
    
    # Keep only the most populated values for analysis
    
    VariableSummary = VariableSummary[order(VariableSummary$Count, decreasing = TRUE), ]
    
    VariableSummary = VariableSummary[seq(pmin(nrow(VariableSummary), UnivariateSummarySize))]

    # Update the output
    
    Output = c(Output, list(Column = VariableSummary))
    
    names(Output)[length(Output)] = Column
    
    # Update the conceptual type in the meta data
    
    MetaData$ConceptualType[MetaData$Variable == Column] = ifelse(sum(is.na(as.numeric(DataSet[[Column]][is.na(DataSet[[Column]]) == FALSE])) == FALSE) / sum(is.na(DataSet[[Column]]) == FALSE) < NumericThreshold, 'Categorical', 'Continuous')

  }})
  
  # Return the output
  
  c(Output, list(MetaData = MetaData))

}

if (FALSE)
{
  
  library(data.table)
  
  # Set the number of observations
  
  NumberOfObservations = 1000
  
  # Set the seed
  
  set.seed(666)
  
  # Create some data
  
  DataSet = data.table(Key = seq(NumberOfObservations), Variable01 = runif(NumberOfObservations), Variable02 = sample(c('a', 'b', 'c'), NumberOfObservations, replace = TRUE), Variable03 = sample(seq(4), NumberOfObservations, replace = TRUE))
  
  # Observe the results of the data profile
  
  DataProfiling(DataSet, UnivariateSummarySize = 10, NumericThreshold = .99)
  
  # Create duplicate record and observe the results of the data profile
  
  DataProfiling(rbind(DataSet, DataSet[1, ]), UnivariateSummarySize = 10, NumericThreshold = .99)
  
}