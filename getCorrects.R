getCorrects = function(exp, stim, resp, group, phase){
    #this function creates vectors of correct responses, as a function of the number of responses,
    #the type of correct responses, and the current phase of the simulated experiment.
    
    #this one is pretty specific, in the future this function will be more general
    if (exp$type == 'progressive simultaneous discrimination' | exp$type == 'progressive simultaneous discrimination w/ context'){
        if (exp$nPhases > 1){
            routine = resp$correctResponses[[group]][[phase]]
        }else{
            routine = resp$correctResponses[[group]]
        }
        if (routine == 'consistent'){
            correct = rep(1, each = 24)
        }else if (routine == 'inconsistent'){
            correct = c(1,2,2,1,2,1,1,2,2,1,2,1,2,2,1,1,2,1,1,2,1,2,2,1)
        }else{
            stop(paste("Response specification", routine, "is not supported.\n Current supported types are 'consistent' and 'inconsistent'"))
        }
    }
    return(correct)
}