saveOptions = function(type, save){
    
    condition_string = NULL
    if (type == 'progressive simultaneous discrimination' | type == 'progressive simultaneous discrimination w/ context' ){
        
        if (save == 'immediate transfer'){
            condition_string = 'current_day == 1 & current_trial < 17 & current_attempt == 1'
        }
        if (save == 'immediate transfer and session end'){
            condition_string = 'current_day == 1 & (current_trial < 17 | current_trial > 112) & current_attempt == 1'
        }

    }
    
    if (is.null(condition_string)){
        message('WARNING: Saving option (option$saving_option) not supported. Saving every trial.')
        condition_string = 'T'
    }
    
    return(condition_string)
}