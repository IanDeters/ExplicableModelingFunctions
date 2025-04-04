CreateTranslationTable = function(DataSet, DesignMatrix, VariableNames)
{
  
  # Get the vector of variable names which are not in DataSet
    
  VariableViolations = VariableNames[!(VariableNames %in% names(DataSet))]
  
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
      
      # If there are more than two variables, commas, including and oxford comma, are added
      
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
  
  # Create a translation table to aid in the use of the design matrix
  
  TranslationTable = data.table(DesignMatrixName = names(DesignMatrix), ColumnNumber = seq(length(names(DesignMatrix))))
  
  TranslationTable$DesignMatrixName = as.character(TranslationTable$DesignMatrixName)
  
  # Initialize the variable and variable value columns
  
  TranslationTable$Variable = NA
  TranslationTable$VariableValue = NA

  # Attend to the variables whose conceptual type is continuous
  
  TranslationTable$Variable[TranslationTable$DesignMatrixName %in% VariableNames] = TranslationTable$DesignMatrixName[TranslationTable$DesignMatrixName %in% VariableNames]
  TranslationTable$VariableValue[TranslationTable$DesignMatrixName %in% VariableNames] = NA
  
  # Remove the continuous variables from the vector
  
  VariableNames = VariableNames[!(VariableNames %in% TranslationTable$Variable[is.na(TranslationTable$Variable) == FALSE])]
  
  # Order the variables by name length with the largest names first
  
  VariableNames = VariableNames[order(nchar(VariableNames), decreasing = TRUE)]
  
  for (Variable in VariableNames)
  {
    
    # In the case that no as.factor transformation was applied to the variable,
    # determine the variable and variable value associated to the column
    
    TranslationTable$Variable[substr(TranslationTable$DesignMatrixName, 1, nchar(Variable)) == Variable] = Variable
    TranslationTable$VariableValue[substr(TranslationTable$DesignMatrixName, 1, nchar(Variable)) == Variable] = substr(TranslationTable$DesignMatrixName, nchar(Variable) + 1, nchar(TranslationTable$DesignMatrixName))[substr(TranslationTable$DesignMatrixName, 1, nchar(Variable)) == Variable]
    
    # In the case that an as.factor transformation was applied to the variable,
    # determine the variable and variable value associated to the column
    
    TranslationTable$Variable[substr(TranslationTable$DesignMatrixName, 1, nchar(Variable) + 11) == paste('as.factor(', Variable, ')', sep = '')] = Variable
    TranslationTable$VariableValue[substr(TranslationTable$DesignMatrixName, 1, nchar(Variable) + 11) == paste('as.factor(', Variable, ')', sep = '')] = substr(TranslationTable$DesignMatrixName, nchar(Variable) + 12, nchar(TranslationTable$DesignMatrixName))[substr(TranslationTable$DesignMatrixName, 1, nchar(Variable) + 11) == paste('as.factor(', Variable, ')', sep = '')]

  }
  
  # Initialize the missing columns from the design matrix
  
  NewDesignMatrix = data.table()
  
  for (Variable in VariableNames)
  {
    
    # Determine which value from the data set was excluded in the design matrix
    
    MissingValue = unique(DataSet[[Variable]])
    MissingValue = MissingValue[!(MissingValue %in% TranslationTable$VariableValue[TranslationTable$Variable == Variable])]

    # Determine which columns were included in the design matrix
        
    VariableColumns = TranslationTable$ColumnNumber[TranslationTable$Variable == Variable]
    VariableColumns = VariableColumns[is.na(VariableColumns) == FALSE]
    
    # Update the matrix of new columns
    
    NewDesignMatrix = cbind(NewDesignMatrix, 1 - as.matrix(DesignMatrix[, VariableColumns, with = FALSE]) %*% rep(1, length(VariableColumns)))
    
    # Rename the new column
    
    names(NewDesignMatrix)[ncol(NewDesignMatrix)] = ifelse(length(unique(substr(TranslationTable$DesignMatrixName, 1, nchar(Variable)) == Variable)) == 2, paste(Variable, MissingValue, sep = ''), paste('as.factor(', Variable, ')', MissingValue, sep = ''))
    
    # Update the translation table
    
    TranslationTable = rbind(TranslationTable, data.table(DesignMatrixName = names(NewDesignMatrix)[ncol(NewDesignMatrix)], ColumnNumber = max(TranslationTable$ColumnNumber) + 1, Variable = Variable, VariableValue = MissingValue))
    
  }

  # Initialize the output
  
  Output = list()
  
  # Populate the output
  
  Output[[1]] = NewDesignMatrix
  Output[[2]] = TranslationTable
  
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
  
  DataSet = data.table(Weight = runif(NumberOfObservations), Variable01 = runif(NumberOfObservations), Variable02 = sample(c('a', 'b', 'c'), NumberOfObservations, replace = TRUE), Variable03 = as.factor(sample(seq(4), NumberOfObservations, replace = TRUE)))

  # Create the design matrix
  
  nrow(DataSet)
  1 + 1 + 1 + 2 + 3

  DesignMatrix = as.data.table(model.matrix(as.formula('~ Weight + Variable01 + Variable02 + Variable03'), DataSet))

  nrow(DesignMatrix)
  ncol(DesignMatrix)
  
  # Create the additional columns in the design matrix and the translation table
  
  DesignMatrix02 = CreateTranslationTable(DataSet, DesignMatrix, c('Weight', 'Variable01', 'Variable02', 'Variable03'))
  
  # Add the additional columns to the design matrix
  
  nrow(DesignMatrix)
  ncol(DesignMatrix) + 2
  
  DesignMatrix = cbind(DesignMatrix, DesignMatrix02[[1]])
  
  nrow(DesignMatrix)
  ncol(DesignMatrix)
  
  # Observe the translation table
  
  TranslationTable = DesignMatrix02[[2]]
  TranslationTable
  
  # Discard the function output
  
  rm(DesignMatrix02)
  
  # Observe the behavior of the columns
  
  unique(cbind(DataSet$Variable02[order(DataSet$Variable02)], DesignMatrix[order(DataSet$Variable02), TranslationTable$DesignMatrixName[substr(TranslationTable$DesignMatrixName, 1, 10) == 'Variable02'], with = FALSE]))
  unique(cbind(DataSet$Variable03[order(DataSet$Variable03)], DesignMatrix[order(DataSet$Variable03), TranslationTable$DesignMatrixName[substr(TranslationTable$DesignMatrixName, 1, 10) == 'Variable03'], with = FALSE]))

  # Generate a target variable  
  
  DesignMatrix$Target = rpois(NumberOfObservations, DesignMatrix$Weight * exp(as.matrix(DesignMatrix[, !(names(DesignMatrix) %in% c('Weight', 'Variable02a', 'Variable031')), with = FALSE]) %*% seq(.1, .7, by = .1))) / DesignMatrix$Weight

  # Observe the equivalence of the output
  
  summary.glm(glm.fit(x = DesignMatrix[, !(names(DesignMatrix) %in% c('Weight', 'Variable02a', 'Variable031', 'Target')), with = FALSE], y = DesignMatrix$Target, weights = DesignMatrix$Weight, family = poisson('link' = log)))
  summary(glm(as.formula('Target ~ Variable01 + Variable02 + Variable03'), data = cbind(DesignMatrix[ ,'Target'], DataSet), weights = Weight, family = poisson(link = 'log')))
  
}