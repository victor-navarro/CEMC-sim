runExperiment = function(exp_info, session_info, stimuli_info, response_info, simulation_info, options){
    
    #Create list with currently supported experiments
    supported = c('progressive simultaneous discrimination')
    
    ### LOAD FUNCTIONS ###
    source('catRepresentation.R')
    source('createStrengthList.R')
    source('createTraining.R')
    source('makeResponse.R')
    source('stimRepresentation.R')
    source('updateStr.R')
    source('getCorrects.R')
    source('pasteFiles.R')
    source('saveOptions.R')
    source('plotSpecial.R')
    source('othersRepresentation.R')
    library(scales)
    
    ###### CHECKING ARGUMENTS #######
    if (missing(exp_info)){
        stop("ERROR: Need to specify experimental information (exp_info).")
    }else{
        if (!is.null(exp_info$type)){
            if (!(exp_info$type %in% supported)){
                stop("ERROR: Experiment type (exp_info$type) not currently supported.")
            }
        }else{
            stop("ERROR: Experiment type (exp_info$type) not especified.")
        }
        if (is.null(exp_info$nPhases) | is.character(exp_info$nPhases) | exp_info$nPhases == 0){
            message("WARNING: Number of phases not correctly specified (exp_info$nPhases). Defaulting to 1.")
            exp_info$nPhases = 1
        }
        if (is.null(exp_info$nSubjects) | is.character(exp_info$nSubjects) | exp_info$nSubjects == 0){
            message("WARNING: Number of subjects not correctly specified (exp_info$nSubjects). Defaulting to 1.")
            exp_info$nSubjects = 1
        }
        if (is.null(exp_info$nSessions) | is.character(exp_info$nSessions) | exp_info$nSessions == 0){
            message("WARNING: Number of sessions not correctly specified (exp_info$nSessions). Defaulting to 1.")
            exp_info$nSessions = 1
        }
        if (is.null(exp_info$nTrialsPerSession) | is.character(exp_info$nTrialsPerSession) | exp_info$nTrialsPerSession == 0){
            message("WARNING: Number of sessions not correctly specified (exp_info$nTrialsPerSession). Defaulting to 10.")
            exp_info$nTrialsPerSession = 10
        }
        if (is.null(exp_info$criterion) | is.character(exp_info$criterion) | exp_info$criterion == 0){
            message("WARNING: Proportion correct criterion not correctly specified (exp_info$criterion). Defaulting to .85.")
            exp_info$criterion = .85
        }
        if (is.null(exp_info$corrections) | !is.logical(exp_info$corrections)){
            message("WARNING: Correction procedure not correctly specified (exp_info$corrections). Defaulting to TRUE.")
            exp_info$corrections = T
        }
    }

    if (missing(stimuli_info)){
        stop("Need to specify stimuli information (stimuli_info).")
    }else{
        if (is.null(stimuli_info$nCategories) | is.character(stimuli_info$nCategories) | stimuli_info$nCategories == 0){
            message("WARNING: Number of categories not correctly specified (stimuli_info$nCategories). Defaulting to 1.")
            stimuli_info$nCategories = 1
        }
        if (is.null(stimuli_info$nStimuli) | is.character(stimuli_info$nStimuli) | stimuli_info$nStimuli == 0){
            message("WARNING: Number of stimuli not correctly specified (stimuli_info$nStimuli). Defaulting to 2.")
            stimuli_info$nStimuli = 2
        }
        if (is.null(exp_info$nStimPerSession) | is.character(exp_info$nStimPerSession) | exp_info$nStimPerSession == 0){
            message("WARNING: Number of sessions not correctly specified (exp_info$nStimPerSession). Defaulting to the number of stimuli.")
            exp_info$nStimPerSession = stimuli_info$nStimuli
        }
    }
    
    if (missing(response_info)){
        stop("Need to specify response information (response_info).")
    }else{
        if (is.null(response_info$nResponses) | is.character(response_info$nResponses) | response_info$nResponses == 0){
            message("WARNING: Number of responses not correctly specified (response_info$nResponses). Defaulting to the number of categories.")
            response_info$nResponses = stimuli_info$nCategories
        }
        if (is.null(exp_info$nGroups) | is.character(exp_info$nGroups) | exp_info$nGroups == 0){
            message("WARNING: Number of groups not correctly specified (exp_info$nGroups). Defaulting to 1.")
            exp_info$nGroups = 1
        }
        if (exp_info$nGroups != length(response_info$correctResponses)){
            stop("ERROR: Number of groups (exp_info$nGroups) is not consistent with the number of response specifications (response_info$correctResponses).")   
        }else{
            for (group in 1:exp_info$nGroups){
                if (exp_info$nPhases != length(response_info$correctResponses[[group]])){
                    print(exp_info$nPhases != length(response_info$correctResponses[[group]]))
                    stop(paste("ERROR: Number of phases (exp_info$nPhases) for group", group, "is not consistent with its number of response specifications (response_info$correctResponses)."))
                }
            }
        }
    }
    
    if (missing(simulation_info)){
        stop("Need to specify simulation information (simulation_info).")
    }else{
        if (is.null(simulation_info$nElements) | is.character(simulation_info$nElements) | simulation_info$nElements == 0){
            message("WARNING: Number of elements to simulate not correctly specified (simulation_info$nElements). Defaulting to 100")
            simulation_info$nElements = 100
        }
        if (is.null(simulation_info$alphaDist) | is.character(simulation_info$alphaDist)){
            message("WARNING: The alpha parameter for the binomial distribution is missing (simulation_info$alphaDist). Defaulting to 1")
            simulation_info$alphaDist = 1
        }
        if (is.null(simulation_info$betaDist) | is.character(simulation_info$betaDist)){
            message("WARNING: The beta parameter for the binomial distribution is missing (simulation_info$betaDist). Defaulting to 4.5")
            simulation_info$betaDist = 4.5
        }
        if (is.null(simulation_info$alphaRW) | is.character(simulation_info$alphaRW) | simulation_info$alphaRW == 0){
            message("WARNING: The alpha parameter for R-W is missing (simulation_info$alphaRW). Defaulting to 0.02")
            simulation_info$alphaRW = 0.02
        }
        if (is.null(simulation_info$betaRW) | is.character(simulation_info$betaRW) | min(simulation_info$betaRW) < 0 | length(simulation_info$betaRW) != 2){
            message("WARNING: The beta parameters for R-W is not correctly specified (simulation_info$betaRW). Defaulting to 0.01 and 0.02")
            simulation_info$betaRW = c(0.01, 0.02)
        }
        if (is.null(simulation_info$theta) | is.character(simulation_info$theta)){
            message("WARNING: The theta parameter for softmax decision rule is not correctly specified (simulation_info$theta). Defaulting to 3")
            simulation_info$tetha = 3
        }
    }
    
    if (!is.null(exp_info$context)){
        if (exp_info$context){
            if (is.null(simulation_info$contextElements)){
                message("WARNING: Number of context elements to simulate not correctly specified (simulation_info$contextElements). Defaulting to simulation_info$nElements")
                simulation_info$contextElements = simulation_info$nElements
            }
        }
    }
    
    if (!is.null(options$group_names)){
        if (length(options$group_names) != exp_info$nGroups){
            message("WARNING: Number of groups (exp_info$nGroups) and group names (options$group_names) do not coincide. Defaulting to letters")
            options$group_names = toupper(letters[1:exp_info$nGroups])
        }
    }
    
    supported_verbose = c('all', 'medium', 'minimal', 'none')
    if(is.null(options$output_verbose) | !(options$output_verbose %in% supported_verbose)){
        options$output_verbose = 'none'
    }
    if(!is.null(options$save_options)){
        condition = saveOptions(type = exp_info$type, save = options$save_options)
    }
    
    supported_plots = c('elements_by_session')
    if(is.null(options$plots) | !(options$plots %in% supported_plots)){
        options$plots = 'none'
    }else if (options$plots == 'elements_by_session'){
        saveElements = T
        sessionElements = list()
    }
    
    if (options$plots == 'none'){
        saveElements = F
    }

    
    ### PREPARE SOME VARIABLES ###
    
    #set the date, and name of the simulation
    filename = paste(exp_info$type, ' ',date())
    
    #set the number of simulation required, and a simulations counter
    nSimulations = exp_info$nGroups*exp_info$nSubjects*exp_info$nPhases
    currentSimulations = 0
    
    ### START THE SIMULATION ###
    
    colors = c('black', 'red')
    plot(-1, -1, xlim = c(1, 24), ylim = c(-.5, 1), xlab = 'Pair', ylab = 'Associative strength')    
    
    #go through the groups
    for (current_group in 1:exp_info$nGroups){
        
        #--- verbose message ---#
        if (exp_info$nGroups > 1 & (options$output_verbose %in% supported_verbose[1:3])){
            print(paste('Current group:', current_group))
        }
        
        #go through the subjects in the groups
        for (current_subject in 1:exp_info$nSubjects){
            
            #--- verbose message ---#
            if (exp_info$nSubjects > 1 & (options$output_verbose %in% supported_verbose[1:3])){
                print(paste('Current subject:', current_subject))
            }
            
            #remove dataframe to save memory, if a simulation has already occurred
            if (exists("dataframe")){
                rm(dataframe)
            }
            
            #generate the data frame to save the current simulation data
            dataframe = data.frame(group = character(), subject = numeric(), 
                              phase = numeric(), session = numeric(), day = numeric(), trial = numeric(),
                              attempt = numeric(), stimulus = numeric(), correct = numeric(), 
                              response = numeric(), outcome = numeric(), p_correct = numeric(), stringsAsFactors = F)
            dataframe[1, ] = list('a', 'a', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            
            #generate the probability distribution for the elements in the category (a categories x elements matrix)
            catElements = catRepresentation(stim = stimuli_info, sim = simulation_info)
            
            #generate the exemplars for each category (a list of length 'categories', with nested stimuli x elements matrices) 
            catExemplars = stimRepresentation(elem_distrib = catElements, stim = stimuli_info, sim = simulation_info)
            
            #put additional stuff depending on the experiment
            catExemplars = othersRepresentation(elem = catElements, exem = catExemplars, stim = stimuli_info, sim = simulation_info, exp = exp_info)
            
            #generate a list holding lists and matrices for elements and exemplars' associative strengths
            strList = createStrengthList(stim = stimuli_info, responses = response_info, sim = simulation_info, exp = exp_info)
            
            #go through each phase
            for (current_phase in 1:exp_info$nPhases){
                
                #--- verbose message ---#
                if (exp_info$nPhases > 1 & (options$output_verbose %in% supported_verbose[1:2])){
                    print(paste('Current phase:', current_phase))
                }
                
                #create the training routine
                trainingRoutine = createTraining(exp = exp_info, stim = stimuli_info, resp = response_info, phase = current_phase, group = current_group) 
                
                #go through every session
                for (current_session in 1:max(trainingRoutine[, 1])){
                    
                    #--- verbose message ---#
                    if ((options$output_verbose %in% supported_verbose[1:1])){
                        print(paste('Current session:', current_session))
                    }
                    
                    #subset the training routine to the current session
                    subTrainingRoutine = trainingRoutine[trainingRoutine[, 1] == current_session, ]
                    
                    #start variables to control flow
                    daily_performance = 0
                    current_day = 0
                    
                    points(current_session, sum(strList$ctx), col = alpha(colors[current_group], .3), pch = 1)
                    points(current_session, mean(strList$exem[[1]]), col = alpha(colors[current_group], .3), pch = '+')
                    points(current_session, mean(strList$exem[[2]]), col = alpha(colors[current_group], .3), pch = '-')
                           
                    #while the criterion has not been achieved
                    while (daily_performance < exp_info$criterion){
                        
                        #increase the day of training
                        current_day = current_day + 1
                        
                        #--- verbose message ---#
                        if ((options$output_verbose %in% supported_verbose[1:1])){
                            print(paste('Current day:', current_day))
                        }
                        
                        #go through all the training trials
                        for (current_trial in 1:exp_info$nTrialsPerSession){
                            
                            #set trial information
                            current_stimulus = subTrainingRoutine[current_trial, 3]
                            current_correct = subTrainingRoutine[current_trial, 4]
                            
                            #default the correction to True
                            correction = T
                            
                            #initialize the attempt number
                            current_attempt = 1
                            
                            #while we are in a correction trial, keep responding until a correct response is made (correction = F)
                            while(correction){
                                #make a response
                                response_data = makeResponse(correct = current_correct, resp = response_info, stim = current_stimulus, str = strList, exp = exp_info, theta = simulation_info$theta)
                                
                                #if its the first attempt AND its a correct response, add it to the daily performance holder
                                if (current_attempt == 1 & response_data[3] == 1){
                                    daily_performance = daily_performance + 1
                                }
                                
                                #save the data
                                if(eval(parse(text = condition))){
                                    dataframe = rbind(dataframe, list(options$group_names[current_group],
                                                            current_subject, 
                                                            current_phase,
                                                            current_session,
                                                            current_day,
                                                            current_trial,
                                                            current_attempt,
                                                            current_stimulus,
                                                            current_correct, 
                                                            response_data[2],
                                                            response_data[3],
                                                            response_data[1]))
                                }
                                
                                #update the associative strength of both elements and exemplars
                                strList = updateStr(exp = exp_info, sim = simulation_info, resp = response_info, c_resp = response_data, stim = stimuli_info, c_stim = current_stimulus, str = strList, exem = catExemplars)
                                
                                #check if we are correcting incorrect responses
                                if (exp_info$corrections){
                                    
                                    #change the need of a correction trial according to the current outcome
                                    if (response_data[3]){
                                        correction = F
                                    }else{
                                        correction = T
                                        #increase the attempt counter
                                        current_attempt = current_attempt + 1
                                    }
                                }else{
                                    #if we do not need correction trials, just make it false
                                    correction = F
                                }
                                
                            }
                            
                        }
                        
                        #once the training session has finished, calculate the mean daily performance
                        daily_performance = daily_performance/exp_info$nTrialsPerSession
                        
                    }
                    
                    if (saveElements){
                        sessionElements[[current_session]] = strList$elem
                    }
                }
                if (options$plots == 'elements_by_session'){
                    plotSpecial(exp = exp_info$type, type = options$plots, data = sessionElements, name = filename)
                }
            }
            
            #--- verbose message ---#
            if (options$output_verbose %in% supported_verbose[1:3]){
                currentSimulations = currentSimulations + 1
                print(paste('Simulations completed: ', currentSimulations, '/', nSimulations, sep = ''))
            }
            
            setwd("/Users/edwardwasserman/Google Drive/Wasserman's Lab/R/CEMC/data")
            #save subject data
            dataframe = na.omit(dataframe)
            write.table(dataframe, paste(filename, '_G', current_group, '_S', current_subject, '_', 'TEMP.txt', sep = ''), sep = '\t', row.names = F)
            setwd("/Users/edwardwasserman/Google Drive/Wasserman's Lab/R/CEMC")
        }
    }
    summary = pasteFiles(filename)
    return(summary)
}