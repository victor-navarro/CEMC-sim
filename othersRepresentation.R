othersRepresentation = function(elem, exem, stim, sim, exp){
    if (exp$type == 'progressive simultaneous discrimination'){
        if (exp$context){
            #calculate the mean probability of the elements
            meanProb = mean(elem)
            
            #create vector to hold all the elements
            exem[[length(exem)+1]] = rep(0, sim$contextElements)
            
            #activate elements using mean probability from the category representation
            for (e in 1:sim$contextElements){
                exem[[length(exem)]][e]= rbinom(1, 1, meanProb)
            }
        }
    }
    return(exem)
}