CDFAssignment = function(x, Weight = NULL, SpecialValueVector = NULL, Name = 'CDF')
{
  
  # If no weight is specified, use a vector of 1s as the weight
  
  if (is.null(Weight) == TRUE)
  {
    
    Weight = rep(1, length(x))
    
  }
  
  # Compute the minimum value
  
  Minimumx = min(x, na.rm = TRUE)
  
  # Initialize the empirical CDF by making all entries non negative.  This will allow one to distinguish between
  # regular, missing, and special values
  
  Output = data.table(Record = seq(length(x)), CDF = x - Minimumx, Weight = Weight)
  
  # Assign the missing values to -1.  Now all entries are populated
  
  Output$CDF = ifelse(is.na(Output$CDF) == TRUE, -1, Output$CDF)
  
  if (is.null(SpecialValueVector) == FALSE)
  {
    
    for (i in seq(length(SpecialValueVector)))
    {
      
      # Assign the special value to a negative integer less than -1
      
      Output$CDF = ifelse(Output$CDF == SpecialValueVector[i] - Minimumx, -1 - i, Output$CDF)
      
      # Create the indicator for the special value
      
      Output[, paste(Name, 'SpecialValue', i, sep = '')] = ifelse(Output$CDF == -1 - i, 1, 0)
      
    }
    
  }
  
  # Create the missing indicator
  
  Output[, paste(Name, 'Missing', sep = '')] = ifelse(Output$CDF == -1, 1, 0)
  
  # Order by the adjusted input vector
  
  Output = Output[order(Output$CDF), ]
  
  # Summarize to the level of the unique values of the variable in question
  
  Output02 = Output[0 <= Output$CDF, .(Weight = sum(Weight)), by = 'CDF']
  
  # Compute the empirical CDF taking care to discard the weight associated to missing and special values
  
  Output02$CDF02 = cumsum(Output02$Weight) / sum(Output02$Weight)
  
  # Merge in the CDF
  
  Output = merge(Output, Output02[, c('CDF', 'CDF02')], all.x = TRUE)
  
  # Combine the CDF with the values corresponding to the special values of the variable
  
  Output$CDF02 = ifelse(is.na(Output$CDF02) == TRUE, Output$CDF, Output$CDF02)
  
  # Restore the original sort order and restrict attention to the columns of interest
  
  Output = Output[order(Output$Record), c(2, ncol(Output), seq(4, ncol(Output) - 1)), with = FALSE]
  
  # Update the name of the CDF transformation to the specified name
  
  names(Output)[names(Output) == 'CDF02'] = Name
  
  # Return the output
  
  Output[, seq(2, ncol(Output)), with = FALSE]
  
}

if (FALSE)
{
  
  library(data.table)
  
  # Set the seed
  
  set.seed(666)
  
  # Set the number of observations
  
  NumberOfObservations = 1000
  
  # Generate the vector
  
  x = runif(NumberOfObservations)
  
  # Generate the weight vector
  
  Weight = runif(NumberOfObservations)
  
  # Assign some components special valuess
  
  x[sample(seq(NumberOfObservations), 100, replace = FALSE)] = 1
  x[sample(seq(NumberOfObservations), 200, replace = FALSE)] = 2
  
  # Make some components missing
  
  x[sample(seq(NumberOfObservations), 300, replace = FALSE)] = NA
  
  # Declare the vector of special values
  
  SpecialValueVector = c(1, 2)
  
  # Apply the function
  
  Results = cbind(data.table(x = x, Weight = Weight), CDFAssignment(x = x, Weight = Weight, SpecialValueVector = c(1, 2), Name = 'CDF'))
  
  # Check the indicator variables

  unique(Results$CDFMissing[is.na(Results$x) == TRUE])
  unique(Results$CDFMissing[Results$CDF == -1])
  unique(Results$CDFSpecialValue1[(is.na(Results$x) == FALSE) & (Results$x == 1)])
  unique(Results$CDFSpecialValue1[Results$CDF == -2])
  unique(Results$CDFSpecialValue2[(is.na(Results$x) == FALSE) & (Results$x == 2)])
  unique(Results$CDFSpecialValue2[Results$CDF == -3])
  unique(Results[(is.na(Results$x) == FALSE) & (Results$x != 1) & (Results$x != 2), c('CDFMissing', 'CDFSpecialValue1', 'CDFSpecialValue2')])
  unique(Results[0 < Results$CDF, c('CDFMissing', 'CDFSpecialValue1', 'CDFSpecialValue2')])
  
  # Restrict attention to populated, non special values
  
  Results = Results[(is.na(x) == FALSE) & (x != 1) & (x != 2), .(Weight = sum(Weight)), by = c('x', 'CDF')]
  
  # Sort by the variable
  
  Results = Results[order(Results$x), ]
  
  # Check the empirical cumulative distribution function
  
  max(abs(Results$CDF - cumsum(Results$Weight) / sum(Results$Weight)))
  
}