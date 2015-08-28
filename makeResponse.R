makeResponse = function(correct, resp, stim, str, exp, theta){
    
    #this function simulates a response given then associative strength of a given stimulus (stim) with all the possible responses (taken from Matrix)
    #first we take a look at all the possible responses
    
    if (exp$type == 'progressive simultaneous discrimination'){
        
        #initialize the total incentive value
        total = 0
        #we make a vector to hold the different associative strengths for each response
        rStrengths = c()
        
        if(exp$context){
            #save the total
            for (response in 1:resp$nResponses){
                total = total + exp(str$exem[[response]][stim]*theta)
            }
            
#             print('stimuli total:')
#             print(total)
            
            #add the context incentive
            total = total + exp(sum(str$ctx)*theta)
            
#             print('context:')
#             print(exp(sum(str$ctx)*theta))
            
            #we calculate the net associative strength for each response associative strength,
            #and then the probability using the softmax choice rule
            for (response in 1:resp$nResponses){
                rStrengths[response] = (exp(str$exem[[response]][stim]*theta) + (exp(sum(str$ctx)*theta) / resp$nResponses)) / total
            }
            
        }else{
            #save the total
            for (response in 1:resp$nResponses){
                total = total + exp(str$exem[[response]][stim]*theta)
            }
            #we calculate the net associative strength for each response associative strength,
            #and then the probability using the softmax choice rule
            for (response in 1:resp$nResponses){
                rStrengths[response] = exp(str$exem[[response]][stim]*theta)/total
            }
        }
    }
    
    #pick the response to be made, using rng from the multinomial distribution
    response_made = which(rmultinom(1, 1, rStrengths) == 1)
    
    
    #save the probability of making the correct response, the response made, and the outcome
    responseData = c(rStrengths[correct], response_made, ifelse(response_made == correct, 1, 0))
    return(responseData)
}