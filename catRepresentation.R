catRepresentation = function(stim, sim){
  #this function takes a number of elements per category (n), and two parameters for the beta function (alpha & beta).

  #we create a matrix to hold the elements (columns) of each category (rows)
  catElem = matrix(NA, nrow = stim$nCategories, ncol = sim$nElements)
 
  #we go through every category
  
  #EACH one of those elements has a sampling probability from the beta distribution
  for (cat in 1:stim$nCategories){
      for (elem in 1:sim$nElements){
        catElem[cat, elem] = rbeta(1, sim$alphaDist, sim$betaDist)
      }
  }
  return(catElem)
}
