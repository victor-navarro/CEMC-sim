getStrength = function(eMatrix, sMatrix){
  #this function outputs the net associative strength of all the exemplars in a category taking into account the
  #current strength at each element (stored in sMatrix) and if that element is activated by the stimulus
  
  nExemplars = dim(eMatrix)[1]
  nElements = dim(sMatrix)[2]
  
  #we create the matrix to store the values
  eStrength = matrix(0, dim(sMatrix)[1], nExemplars)
  
  #we go through all possible responses
  for (resp in 1:dim(sMatrix)[1]){
    #we go through all possible exemplars
    for (exem in 1:nExemplars){
      #we initialize the strength holder
      strength = 0
      #we go through all possible elements
      for (elem in 1:nElements){
        #if the element is activated by the exemplar
        if (eMatrix[exem, elem]){
          #then we add the strength in that element to the strength holder
          strength = strength + sMatrix[resp, elem]
        }
      }
      #after we are done sumating the strength, we set the strength of the exemplar to that value
      eStrength[resp, exem] = strength
    }
  }
  return(eStrength)
}