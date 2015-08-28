createTraining = function(exp, stim, resp, phase, group){
    #This function creates a matrix training as a function of the type of experiment being simulated (exp$type),
    #and if needed, as a function of the phase in which the program is in.
    
    #import functions needed
    source('getCorrects.R')
    
    #get the correct responses
    corrects = getCorrects(exp, stim, resp, group, phase)
    #create a vector with the stimuli numbers to randomize the pairs
    stimuliVector = sample(c(1:stim$nStimuli))
    
    # we start the master matrix
    trainMatrix = NULL
    
    if (exp$type == 'progressive simultaneous discrimination'){
        
        for (session in 1:exp$nSessions){
            holder = matrix(c(rep(stimuliVector[session], exp$nTrials), rep(corrects[session], exp$nTrials)), nrow = exp$nTrials, ncol = 2)
            trainMatrix = rbind(trainMatrix, cbind(rep(session, exp$nTrials), 1:exp$nTrials, holder[sample.int(nrow(holder)),]))
        }
        
    }
    
    return(trainMatrix)
}