FormatearthOutput = function(Model)
{
  
  # Record the term number, name, variables in the term, how it is used, and the associated knot value
  # Warnings are suppressed since some values are undefined for the intercept term
  
  ModelInformation = suppressWarnings(do.call(rbind, lapply(seq(nrow(Model[['dirs']])), function(i){data.table(Term = i, Name = rownames(Model[['dirs']])[i], Variable = colnames(Model[['dirs']])[Model[['dirs']][i, ] != 0], Use = as.numeric(Model[['dirs']][i, Model[['dirs']][i, ] != 0]), Knot = as.numeric(Model[['cuts']][i, Model[['dirs']][i, ] != 0]))})))

  # Restrict attention to the selected terms
  
  ModelInformation = ModelInformation[ModelInformation$Term %in% Model[['selected.terms']], ]
  
  if ('(Intercept)' %in% ModelInformation$Name)
  {
    
    # Renumber the terms to prepare discarding the intercept term
    
    ModelInformation$Term = ModelInformation$Term - (ModelInformation$Term[ModelInformation$Name == '(Intercept)'] <= ModelInformation$Term)

    # Discard information corresponding to the intercept term
        
    ModelInformation = ModelInformation[ModelInformation$Name != '(Intercept)', ]

  }

  # Declare as missing the knots associated to terms which entered linearly
  
  ModelInformation$Knot[ModelInformation$Use == 2] = NA
  
  # Record the model output
  
  Results = list(ModelInformation)
  
  # Expand the model information to allow for selection from more simplistic terms
  
  for (i in seq(max(ModelInformation$Term)))
  {
    
    # Restrict attention to the term in question
    
    Term = ModelInformation[ModelInformation$Term == i, ]
    
    # Obtain the number of factors in the term
    
    NumberOfFactors = sum(ModelInformation$Term == i)
    
    if ((1 < NumberOfFactors) == TRUE)
    {
      
      for (j in seq(NumberOfFactors - 1))
      {
        
        # Determine the number of combinations
        
        Combinations = combn(NumberOfFactors, j)
        
        for (k in seq(ncol(Combinations)))
        {
          
          ModelInformation = rbind(ModelInformation, data.table(Term = max(ModelInformation$Term) + 1, Name = NA, Variable = Term$Variable[Combinations[, k]], Use = Term$Use[Combinations[, k]], Knot = Term$Knot[Combinations[, k]]))
          
        }
        
      }
      
    }
    
  }

  # Discard the Name field
  
  ModelInformation$Name = NULL
  
  # Merge in the degree of each term
  
  ModelInformation = merge(ModelInformation, ModelInformation[, .(TermDegree = .N), by = c('Term')], by = c('Term'))
  
  # Determine the number of left hinges in a term
  
  ModelInformation = merge(ModelInformation, ModelInformation[, .(LeftHingeCount = sum(Use == -1)), by = c('Term')], by = c('Term'))
  
  # Partition the model information into terms which need expanded and which do not
  
  TermsToConvert = ModelInformation[0 < ModelInformation$LeftHingeCount, names(ModelInformation)[names(ModelInformation) != 'LeftHingeCount'], with = FALSE]
  ModelInformation = ModelInformation[0 == ModelInformation$LeftHingeCount, names(ModelInformation)[names(ModelInformation) != 'LeftHingeCount'], with = FALSE]

  # Initialize the looping condition
  
  LoopingCondition = as.logical(sum(TermsToConvert$Use == -1))
  
  while (LoopingCondition)
  {
    
    # Select a term to split
    
    TermToConvert = TermsToConvert[TermsToConvert$Term == TermsToConvert$Term[TermsToConvert$Use == -1][1], ]
    
    # Split it into two terms
    
    ConvertedTerms = rbind(TermToConvert, TermToConvert)
    
    # Update the term numbers
    
    ConvertedTerms$Term = as.numeric(sapply(seq(2), function(i){rep(max(TermsToConvert$Term) + i, TermToConvert$TermDegree[1])}))
    
    # Select the variable to change
    
    Variable = TermToConvert$Variable[TermToConvert$Use == -1][1]
    
    # Update the use values
    
    ConvertedTerms$Use[ConvertedTerms$Variable == Variable] = c(2, 1)
    
    # Update the knot value
    
    ConvertedTerms$Knot[ConvertedTerms$Use == 2] = NA
    
    # Update the terms needing conversion
    
    TermsToConvert = rbind(TermsToConvert[!(TermsToConvert$Term == TermsToConvert$Term[TermsToConvert$Use == -1][1]), ], ConvertedTerms)
    
    # Update the looping condition
    
    LoopingCondition = as.logical(sum(TermsToConvert$Use == -1))
    
  }
  
  # Order by the model degree and the term number
  
  ModelInformation = ModelInformation[order(ModelInformation$Term), ]
  
  # Create a new sequential term number
  
  ModelInformation$Term02 = cumsum(c(TRUE, !(ModelInformation$Term[seq(2, nrow(ModelInformation))] == ModelInformation$Term[seq(1, nrow(ModelInformation) - 1)])))
  
  # Order by the model degree and the term number
  
  TermsToConvert = TermsToConvert[order(TermsToConvert$Term), ]
  
  # Create a new sequential term number
  
  TermsToConvert$Term02 = cumsum(c(TRUE, !(TermsToConvert$Term[seq(2, nrow(TermsToConvert))] == TermsToConvert$Term[seq(1, nrow(TermsToConvert) - 1)])))
  
  # Update the sequential term number
  
  TermsToConvert$Term02 = TermsToConvert$Term02 + max(ModelInformation$Term02)
  
  # Recombine the data sets
  
  ModelInformation = rbind(ModelInformation[, c('Variable', 'Use', 'Knot', 'TermDegree', 'Term02')], TermsToConvert[, c('Variable', 'Use', 'Knot', 'TermDegree', 'Term02')])
  ModelInformation$Term = ModelInformation$Term02
  
  # Sort by the model degree, term, and variable
  
  ModelInformation = ModelInformation[order(ModelInformation$Term, ModelInformation$Variable), ]
  
  # Create a string representing each term
  
  TermComparison = as.data.table(t(sapply(seq(nrow(unique(ModelInformation[, c('Term')]))), function(i){data.table(Term = unique(ModelInformation[, c('Term')])[['Term']][i], Name = paste(paste(ModelInformation$Variable[(ModelInformation$Term == unique(ModelInformation[, c('Term')])[['Term']][i])], collapse = '_'), paste(ModelInformation$Knot[ModelInformation$Term == unique(ModelInformation[, c('Term')])[['Term']][i]], collapse = '_'), sep = '_'))})))
  
  # Determine if another term represents the term in question
  
  TermComparison$Duplicate = c(sapply(seq(nrow(TermComparison) - 1), function(i){TermComparison$Name[i] %in% TermComparison$Name[seq(i + 1, nrow(TermComparison))]}), FALSE)
  
  # Format the columns
  
  TermComparison$Term = as.numeric(TermComparison$Term)
  
  # Restrict attention to the unique terms
  
  TermComparison = TermComparison[!(TermComparison$Duplicate), c('Term')]
  
  # Select the terms corresponding to the unique terms
  
  ModelInformation = merge(ModelInformation, TermComparison, by = c('Term'))
  
  # Order by the model degree and the term number
  
  ModelInformation = ModelInformation[order(ModelInformation$Term), ]
  
  # Create a new sequential term number
  
  ModelInformation$Term02 = cumsum(c(TRUE, !(ModelInformation$Term[seq(2, nrow(ModelInformation))] == ModelInformation$Term[seq(1, nrow(ModelInformation) - 1)])))
  ModelInformation$Term = ModelInformation$Term02
  
  # Update the results
  
  Results = c(Results, list(ModelInformation[, c('Term', 'Variable', 'Use', 'Knot')]))
  
  # Name the elements of the list
  
  names(Results) = c('ModelInformation', 'ExpandedModelInformation')
  
  # Return the results
  
  Results
  
}

if (FALSE)
{
  
  # install.packages('data.table')
  library(data.table)
  
  # install.packages('earth')
  library(earth)
  
  # Set the seed
  
  set.seed(666)
  
  # Declare the number of observations
  
  NumberOfObservations = 10000
  
  # Create the predictor, weight, and offset variables
  
  CheckThis = data.table(x = runif(NumberOfObservations), y = runif(NumberOfObservations), Weight = runif(NumberOfObservations), Offset = runif(NumberOfObservations))
  
  # Create the weighted target variable
  
  CheckThis$Target = rpois(NumberOfObservations, CheckThis$Weight * exp(1 + 2 * pmax(CheckThis$x - .2, 0) + 3 * pmax(CheckThis$y - .15, 0) + 4 * pmax(CheckThis$x - .1, 0) * pmax(CheckThis$y - .05, 0) + CheckThis$Offset)) / CheckThis$Weight
  
  # Check the model construction
  
  summary(glm(formula = Target ~ pmax(CheckThis$x - .2, 0) + pmax(CheckThis$y - .15, 0) + pmax(CheckThis$x - .1, 0) : pmax(CheckThis$y - .05, 0), weights = Weight, offset = Offset, family = poisson(link = 'log'), data = CheckThis))

  # Run the earth subroutine
  
  CheckThis02 = earth(formula = Target ~ x + y + offset(Offset), weights = Weight, data = CheckThis)
  
  # Observe model parameter estimates
  
  CheckThis02[['coefficients']]
  
  # Explicitly specify the distribution and link function
  
  CheckThis02 = earth(formula = Target ~ x + y + offset(Offset), weights = Weight, data = CheckThis, glm = list(family = gaussian(link = 'identity')))
  
  # Observe that the parameter estimates are the same
  
  CheckThis02[['coefficients']]
  CheckThis02[['glm.coefficients']]
  
  # Explicitly specify the distribution and link function from the original model
  
  CheckThis02 = earth(formula = Target ~ x + y + offset(Offset), weights = Weight, data = CheckThis, glm = list(family = poisson(link = 'log')))
  
  # Observe that the parameter estimates are not the same
  
  CheckThis02[['coefficients']]
  CheckThis02[['glm.coefficients']]
  
  # Observe that the standard linear model estimates are used to construct the fitted values
  
  max(abs(CheckThis02[['bx']] %*% CheckThis02[['coefficients']] + CheckThis$Offset - CheckThis02[['fitted.values']]))
  
  # Observe that the fitted values are used to calculate the residual sum of squared errors (i.e. rss)
  
  CheckThis02[['rss']]
  sum(CheckThis$Weight * (CheckThis02[['fitted.values']] - CheckThis$Target)^2)
  
  # Observe that the sum of squared errors are used to the R squared (i.e. rsq) statistic which is used in the
  # stopping criteria.  Conclude that knot selection is done in terms of the standard linear including weights
  # and offsets
  
  CheckThis02[['rsq']]
  1 - CheckThis02[['rss']] / sum(CheckThis$Weight * (CheckThis$Target - CheckThis$Offset - sum(CheckThis$Weight * (CheckThis$Target - CheckThis$Offset)) / sum(CheckThis$Weight))^2)
  
  # Call the function with explicit default parameters and compare the results
  
  earth(formula = Target ~ x + y + offset(Offset), data = CheckThis, weights = Weight, thresh = .001,
        pmethod = 'backward', glm = list(family = gaussian(link = 'identity')), degree = 1, nk = 21,
        nprune = NULL, varmod.method = 'none')[['coefficients']]
  
  CheckThis02[['coefficients']]
  
  # Observe that when there is no pruning more terms are initially selected

  earth(formula = Target ~ x + y + offset(Offset), data = CheckThis, weights = Weight, thresh = .001,
        pmethod = 'none', glm = list(family = gaussian(link = 'identity')), degree = 1, nk = 21,
        nprune = NULL, varmod.method = 'none')[['coefficients']]
  
  CheckThis02[['coefficients']]
  
  # Observe that when the treshold is made more stringent, fewer terms are created
  
  earth(formula = Target ~ x + y + offset(Offset), data = CheckThis, weights = Weight, thresh = .001,
        pmethod = 'none', glm = list(family = gaussian(link = 'identity')), degree = 1, nk = 21,
        nprune = NULL, varmod.method = 'none')[['coefficients']]
  
  earth(formula = Target ~ x + y + offset(Offset), data = CheckThis, weights = Weight, thresh = .01,
        pmethod = 'none', glm = list(family = gaussian(link = 'identity')), degree = 1, nk = 21,
        nprune = NULL, varmod.method = 'none')[['coefficients']]
  
  # Observe that using the nprune parameter forces selection of the best transformation

  earth(formula = Target ~ x + y + offset(Offset), data = CheckThis, weights = Weight, thresh = .001,
        pmethod = 'none', glm = list(family = gaussian(link = 'identity')), degree = 1, nk = 21,
        nprune = 2, varmod.method = 'none')[['coefficients']]
  
  CheckThis02[['coefficients']]
  
  # Observe the other pertinent aspects of the model
  
  CheckThis02[['dirs']]
  CheckThis02[['cuts']]
  CheckThis02[['selected.terms']]
  
  CheckThis02[['dirs']][CheckThis02[['selected.terms']]]
  CheckThis02[['cuts']][CheckThis02[['selected.terms']]]

  # Since the algorithm minimizes the sum of squared errors, create such a target
  
  CheckThis$Target = rnorm(n = NumberOfObservations, mean = CheckThis$Weight * (1 + 2 * pmax(CheckThis$x - .2, 0) + 3 * pmax(CheckThis$y - .15, 0) + 4 * pmax(CheckThis$x - .1, 0) * pmax(CheckThis$y - .05, 0) + CheckThis$Offset), sd = 2 * sqrt(CheckThis$Weight)) / CheckThis$Weight

  # Check the model construction.  
  
  summary(glm(formula = Target ~ pmax(CheckThis$x - .2, 0) + pmax(CheckThis$y - .15, 0) + pmax(CheckThis$x - .1, 0) : pmax(CheckThis$y - .05, 0), weights = Weight, offset = Offset, family = gaussian(link = 'identity'), data = CheckThis))

  # Run the earth subroutine 
    
  CheckThis02 = earth(formula = Target ~ x + y + offset(Offset), data = CheckThis, weights = Weight, thresh = .001,
                      pmethod = 'backward', glm = list(family = gaussian(link = 'identity')), degree = 2, nk = 21,
                      nprune = NULL, varmod.method = 'none')
  
  # Examine the formatted output
  
  FormatearthOutput(CheckThis02)

}