DataStandardization = function(DataSet, WeightVariable = NULL, MetaData, NonMissingSpecialValues = NULL)
{
  
  # Check that the variables in MetaData and NonMissingSpecialValues are in DataSet
  
  for (Check in c('MetaData', 'NonMissingSpecialValues'))
  {
    
    # Get the vector of variable names which are not in DataSet
    
    VariableViolations = get(Check)$Variable[!(get(Check)$Variable %in% names(DataSet))]

    if (1 <= length(VariableViolations))
    {
      
      if (2 <= length(VariableViolations))
      {
        
        # Initialize the variable string
        
        VariableString = paste(VariableViolations[seq(length(VariableViolations) - 1)], collapse = ', ')
        
        # If there are only two variables, no commas are needed
        
        if (length(VariableViolations) == 2)
        {
          
          VariableString = paste(VariableString, ' and ', VariableViolations[length(VariableViolations)], sep = '')
          
        }
        
        # If there are more than two variables, commas, including an oxford comma, are added
        
        else
        {
          
          VariableString = paste(VariableString, ', and ', VariableViolations[length(VariableViolations)], sep = '')
          
        }
        
        stop(paste('The variables ', VariableString, ' in ' , Check, ' are not in DataSet.', sep = ''))
        
      }
      
      else
      {
        
        stop(paste('The variable ', VariableViolations, ' in ' , Check, ' is not in DataSet.', sep = ''))
        
      }
      
    }
    
  }

  # Check if there is a weight variable and initialize the predictor data set
  
  if (is.null(WeightVariable) == TRUE)
  {
    
    DataSet02 = data.table(Weight = rep(1, nrow(DataSet)))

  }
  
  else
  {
    
    if (!(WeightVariable %in% names(DataSet)))
    {
      
      stop('The variable WeightVariable is not in DataSet.')
      
    }
    
    else
    {
      
      DataSet02 = data.table(Weight = DataSet[[WeightVariable]])

    }

  }
  
  # Initialize the variable translation table
  
  VariableTranslationTable = data.table()
  
  if (0 < sum(MetaData$ConceptualType == 'Continuous'))
  {
    
    # Obtain the set of continuous variables
    
    ContinuousVariables = MetaData$Variable[MetaData$ConceptualType == 'Continuous']
    
    for (i in seq(length(ContinuousVariables)))
    {
      
      # Obtain the variable name
      
      ContinuousVariable = ContinuousVariables[i]
      
      # Create the suffix on the basis of the variable number
      
      Suffix = AddLeadingZeros(i, nrow(MetaData))
      
      # Create the new variable name
      
      NewVariable = paste('Variable', Suffix, sep = '')
      
      # Create the CDF version of the variables
      
      if (sum(NonMissingSpecialValues$Variable == ContinuousVariable) == 0)
      {
        
        EmpiricalCDF = CDFAssignment(DataSet[[ContinuousVariable]], Weight = DataSet02$Weight, SpecialValueVector = NULL, Name = NewVariable)
        
      }
      
      else
      {
        
        EmpiricalCDF = CDFAssignment(DataSet[[ContinuousVariable]], Weight = DataSet02$Weight, SpecialValueVector = NonMissingSpecialValues$Value[NonMissingSpecialValues$Variable == ContinuousVariable], Name = NewVariable)
        
      }
      
      # Add the empirical cumulative distribution function and missing and special value indicators to the data
      
      DataSet02 = cbind(DataSet02, EmpiricalCDF)
      
      # Update the translation table
      
      VariableTranslationTable = rbind(VariableTranslationTable, data.table(OldVariable = rep(ContinuousVariable, ncol(EmpiricalCDF)), NewVariable = names(EmpiricalCDF)))
      
    }
    
  }
  
  if (0 < sum(MetaData$ConceptualType != 'Continuous'))
  {
    
    # Obtain the set of categorical variables
    
    CategoricalVariables = MetaData$Variable[MetaData$ConceptualType != 'Continuous']
    
    # Initialize the maximum length of the values of the categorical variables
    
    MaximumCategoricalVariableLength = 1
    
    # Determine the longest variable value length of the categorical variables
    
    for (Variable in CategoricalVariables)
    {
      
      MaximumCategoricalVariableLength = max(MaximumCategoricalVariableLength, max(nchar(as.character(unique(DataSet[[Variable]][is.na(DataSet[[Variable]]) == FALSE])))))
      
    }
    
    # Check if there were continuous variables
    
    if (exists('i') == FALSE)
    {
      
      Start = 1
      
    }
    
    else
    {
      
      Start = i + 1
      
    }
    
    for (i in seq(Start, Start + length(CategoricalVariables) - 1))
    {
      
      # Obtain the variable name
      
      CategoricalVariable = CategoricalVariables[i - Start + 1]
      
      # Create a version of the variable where missing values are populated with a concatenation of z's
      
      DataSet02$x = as.numeric(as.factor(DataSet[[CategoricalVariable]]))

      # Attend to the missing values
      
      DataSet02$x[is.na(DataSet02$x) == TRUE] = 0
      
      # Create the suffix on the basis of the variable number
      
      Suffix = AddLeadingZeros(i, nrow(MetaData))
      
      # Create the new variable name
      
      NewVariable = paste('Variable', Suffix, sep = '')
      
      # Update the variable name
      
      names(DataSet02)[ncol(DataSet02)] = NewVariable
      
      # Update the translation table
      
      VariableTranslationTable = rbind(VariableTranslationTable, data.table(OldVariable = CategoricalVariable, NewVariable = NewVariable))
      
    }
    
  }
  
  # Initialize the output
  
  Output = list()
  
  # Populate the output
  
  Output[[1]] = DataSet02
  Output[[2]] = VariableTranslationTable
  
  # Return the output
  
  Output
  
}

if (FALSE)
{
  
  library(data.table)
  
  # Set the number of observations
  
  NumberOfObservations = 1000
  
  # Set the seed
  
  set.seed(666)
  
  # Create some data
  
  DataSet = data.table(Weight = runif(NumberOfObservations), Column01 = sample(c(NA, seq(100)), NumberOfObservations, replace = TRUE), Column02 = sample(c('a', 'b', 'c'), NumberOfObservations, replace = TRUE), Column03 = sample(c(NA, seq(4)), NumberOfObservations, replace = TRUE))

  # State the meta data for the variables
  
  MetaData = data.table(
    
    Variable = c('Column01', 'Column02', 'Column03'), 
    
    ConceptualType = c('Continuous', 'Categorical', 'Categorical')
    
  )
  
  # Mark some values as special

  NonMissingSpecialValues = data.table(
    
    Variable = c('Column01', 'Column01'),
    
    Value = c(96, 98)
    
  )
  
  # Standardize the variables
   
  DataStandardization(DataSet, WeightVariable = 'Weight', MetaData = MetaData, NonMissingSpecialValues = NonMissingSpecialValues)

}