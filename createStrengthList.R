createStrengthList = function(stim, responses, sim, exp){
  
  strList = list(elem = list(), exem = list())
  
  if (exp$type == 'progressive simultaneous discrimination'){
      strList$elem = rep(0, sim$nElements)
      
      #create the exemplars list
      for (cat in 1:stim$nCategories){
          strList$exem[[cat]] = rep(0, stim$nStimuli)
      }
      
      #if we need a context, create a list for the context
      if (exp$context){
        strList$ctx = rep(0, sim$contextElements)
      }
  }
  return(strList)
}