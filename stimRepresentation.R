stimRepresentation = function(elem_distrib, stim, sim){
    #this function will determine if the element in a stimulus will be activated (1) or not (0), for stim$nStimuli, using a Bernoulli distribution rng,
    #using a success chance determined by elem_distrib for each element
    
    #intitialize a list
    catList = list()
    
    #we through all the categories
    for (c in 1:stim$nCategories){
        #initialize a matrix of stimuli x elements dimensions
        stimuliMatrix = matrix(NA, nrow = stim$nStimuli, ncol = dim(elem_distrib)[2])
        #go trough every stimuli
        for (s in 1:stim$nStimuli){
            #go through every element
            for (e in 1:dim(elem_distrib)[2]){
                stimuliMatrix[s, e] = rbinom(1, 1, elem_distrib[c, e])
            }
        }
    #we save that matrix in our list
    catList[[c]] = stimuliMatrix
    }
    return(catList)
}