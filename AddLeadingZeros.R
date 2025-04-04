AddLeadingZeros = function(Integer, Magnitude)
{
  
  if (Magnitude < 0)
  {
    
    stop('The variable Magnitude is negative.')
    
  }
  
  # Determine the character length of the base 10 number representing Magnitude
  
  LengthOfMagnitude = ceiling(log10(Magnitude)) + 1
  
  # Add LengthOfMagnitude leading zeros
  
  CharacterColumnNumber = paste(strrep('0', LengthOfMagnitude), toString(Integer), sep = '')
  
  # Return the rightmost LengthOfMagnitude characters
  
  substr(CharacterColumnNumber, nchar(CharacterColumnNumber) - LengthOfMagnitude + 1, nchar(CharacterColumnNumber))
  
}

if (FALSE)
{

  AddLeadingZeros(42, 10)
  AddLeadingZeros(42, 100)
  AddLeadingZeros(42, 101)
  AddLeadingZeros(42, 1000)
  
}