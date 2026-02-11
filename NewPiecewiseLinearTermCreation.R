NewPiecewiseLinearTermCreation = function(MetaData01, MetaData02 = MetaData01)
{
  
  # Create the cartesian product of all terms

  Interactions = as.data.table(expand.grid(MetaData01Term = unique(MetaData01$Term), MetaData02Term = unique(MetaData02$Term)))
  Interactions = merge(Interactions, MetaData01, by.x = 'MetaData01Term', by.y = 'Term', allow.cartesian = TRUE)
  Interactions = merge(Interactions, MetaData02, by.x = 'MetaData02Term', by.y = 'Term', allow.cartesian = TRUE)
  
  # Discard terms with repeated variables
  
  TermsToDiscard = Interactions[, .(DuplicateVariable = as.logical(pmin(sum(Variable.x == Variable.y), 1))), by = c('MetaData01Term', 'MetaData02Term')]
  
  Interactions = merge(Interactions, TermsToDiscard[TermsToDiscard$DuplicateVariable == FALSE, c('MetaData01Term', 'MetaData02Term')], by = c('MetaData01Term', 'MetaData02Term'))

  # Create an index set for the new meta data
  
  NewMetaDataIndices = unique(Interactions[, c('MetaData01Term', 'MetaData02Term')])
  
  # Create the new meta data

  MetaData = unique(do.call(rbind, lapply(X = seq(nrow(NewMetaDataIndices)), FUN = function(i){data.table(Term = rep(i, 2 * sum((Interactions$MetaData01Term == NewMetaDataIndices$MetaData01Term[i]) & (Interactions$MetaData02Term == NewMetaDataIndices$MetaData02Term[i]))), Variable = c(Interactions$Variable.x[(Interactions$MetaData01Term == NewMetaDataIndices$MetaData01Term[i]) & (Interactions$MetaData02Term == NewMetaDataIndices$MetaData02Term[i])], Interactions$Variable.y[(Interactions$MetaData01Term == NewMetaDataIndices$MetaData01Term[i]) & (Interactions$MetaData02Term == NewMetaDataIndices$MetaData02Term[i])]), Knot = c(Interactions$Knot.x[(Interactions$MetaData01Term == NewMetaDataIndices$MetaData01Term[i]) & (Interactions$MetaData02Term == NewMetaDataIndices$MetaData02Term[i])], Interactions$Knot.y[(Interactions$MetaData01Term == NewMetaDataIndices$MetaData01Term[i]) & (Interactions$MetaData02Term == NewMetaDataIndices$MetaData02Term[i])]))})))

  if (ifelse(1 <= nrow(MetaData), 2 <= max(MetaData$Term), FALSE))
  {
    
    # Sort the meta data
    
    MetaData = MetaData[order(MetaData$Term, MetaData$Variable, MetaData$Knot), ]
    
    # Check if there are duplicate terms as defined by containing all of the same variable / knot pairs
    
    DuplicateTerms = data.table(Term = seq(max(MetaData$Term)), Duplicate = c(FALSE, sapply(X = seq(2, max(MetaData$Term)), FUN = function(i){as.logical(pmin(sum(sapply(X = seq(1, i - 1), FUN = function(j){as.logical(prod((MetaData$Variable[MetaData$Term == i] == MetaData$Variable[MetaData$Term == j]) & (ifelse((is.na(MetaData$Knot[MetaData$Term == i]) == FALSE) & (is.na(MetaData$Knot[MetaData$Term == j]) == FALSE), MetaData$Knot[MetaData$Term == i] == MetaData$Knot[MetaData$Term == j], FALSE) | (is.na(MetaData$Knot[MetaData$Term == i]) == TRUE) & (is.na(MetaData$Knot[MetaData$Term == j]) == TRUE))))})), 1))})))

    # Discard the duplicate terms
    
    MetaData = MetaData[MetaData$Term %in% DuplicateTerms$Term[DuplicateTerms$Duplicate == FALSE], ]

    # Update the term numbers
    
    MetaData = merge(data.table(OldTerm = unique(MetaData$Term), Term = seq(length(unique(MetaData$Term)))), MetaData, by.x = 'OldTerm', by.y = 'Term')[, names(MetaData), with = FALSE]

  }
  
  # Return the results

  MetaData  

}

if (FALSE)
{
  
  library(data.table)
  
  # Create the initial meta data
  
  MetaData01 = data.table(Term = c(1, 2, 3, 4), Variable = c('Variable01', 'Variable02', 'Variable02', 'Variable03'), Knot = c(1, NA, 2, 3))
  MetaData02 = MetaData01
  
  # Create the degree two terms
  
  MetaData03 = NewPiecewiseLinearTermCreation(MetaData01 = MetaData01, MetaData02 = MetaData02)
  
  # Examine the results
  
  MetaData01
  MetaData02
  MetaData03

  # Create the degree three terms
  
  MetaData04 = NewPiecewiseLinearTermCreation(MetaData01 = MetaData01, MetaData02 = MetaData03)
  
  # Examine the results
  
  MetaData01
  MetaData03
  MetaData04
  
  # Create the degree four terms
  
  MetaData05 = NewPiecewiseLinearTermCreation(MetaData01 = MetaData01, MetaData02 = MetaData04)
  
  # Examine the results
  
  MetaData01
  MetaData04
  MetaData05

}