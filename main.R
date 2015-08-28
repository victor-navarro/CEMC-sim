#get rid of current variables
rm(list = ls())

#load libraries and functions
source('runExperiment.R')
library(ggplot2)

#information about the experiment
exp_info = list(type = 'progressive simultaneous discrimination',
                nGroups = 2,
                nSubjects = 10,
                nPhases = 1,
                nSessions = 24,
                nTrialsPerSession = 128,
                nStimPerSession = 1,
                criterion = .85,
                corrections = T,
                context = T)

#information about the stimuli
stimuli_info = list(nCategories = 2,
                    nStimuli = 24)

#information about the responses
response_info = list(nResponses = 2,
                     correctResponses = list(group1 = c('consistent'), group2 = c('inconsistent')))

#information about the simulation
simulation_info = list(nElements = 100,
                       alphaDist = 1, #for beta distribution
                       betaDist = 4.5, #for beta distribution
                       alphaRW = .05, #for R-W
                       ctxAlphaRW = .02, #for R-W (context)
                       betaRW = c(0.01, 0.02), #for R-W, first value is for non-reinforced trials
                       lambdaRW = c(-1, 1), #for R-W, first value is for non-reinforced trials
                       theta = 10, #for softmax
                       contextElements = 50) #for additional things such as a context

#optional arguments to control important and unimportant things
options = list(output_verbose = 'all', plots = 'none', save_options = 'immediate transfer and session end', group_names = c('Consistent', 'Inconsistent'))

#run the experiment
sim_data = runExperiment(exp_info = exp_info, stimuli_info = stimuli_info, 
                response_info = response_info, simulation_info = simulation_info, options = options)

setwd("/Users/edwardwasserman/Google Drive/Wasserman's Lab/R/CEMC/data")
real = read.table('processed_data.txt', sep = '\t', header = T)


ggplot(subset(sim_data, attempt == 1 & day == 1 & trial < 17), aes(x = ceiling(session/3), y = outcome, linetype = group)) + stat_summary(fun.y = 'mean', geom = 'point') + 
    stat_summary(fun.y = 'mean', geom = 'line') + stat_summary(data = subset(real, moment == 'Start'), aes(x = ceiling(pnum/3), y = correct, linetype = group), fun.y = 'mean', geom = 'line', colour = 'red')
