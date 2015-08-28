updateStr = function(exp, sim, resp, c_resp, stim, c_stim, str, exem){
    
    #the formula is delta-V = alpha*beta*(lambda - Vt)
    #but we adapt it to delta-V = alpha*beta*(lambda - Vt*presence)
    #were presence refers to the activation of an element in a given trial, and comes from elemMatrix[stim]
    
    if (exp_info$type == 'progressive simultaneous discrimination'){
        
        #get the total associative strength for the response made
        vT = sum(str$elem[exem[[c_resp[2]]][c_stim, ] > 0])
        #sum the associative value of the context if necessary
        if (exp$context){
            vT = vT + sum(str$ctx)
        }
        
        #update the exemplar activated elements using the R-W rule
        str$elem[exem[[c_resp[2]]][c_stim, ] > 0] = str$elem[exem[[c_resp[2]]][c_stim, ] > 0] + (sim$alphaRW*sim$betaRW[c_resp[3]+1]*(sim$lambda[c_resp[3]+1] - vT))
        
        #update the context activated elements if necessary
        if (exp$context){
            str$ctx[exem[[stim$nCategories + 1]] > 0 ] = str$ctx[exem[[stim$nCategories + 1]] > 0 ] + (sim$ctxAlphaRW*sim$betaRW[c_resp[3]+1]*(sim$lambda[c_resp[3]+1] - vT))
        }
        
        #update the strength of the exemplars for each category
        #go through all categories
        for (cat in 1:stim$nCategories){
            for (s in 1:dim(exem[[1]])[1]){
                str$exem[[cat]][s] = sum(str$elem[exem[[cat]][s, ] > 0])
            }
        }
        
        #print(str$ctx)
        
    }
    return(str)
}