plotSpecial = function(exp, type, data, name){
    library(ggplot2)
    if (exp == 'progressive simultaneous discrimination'){
        if (type == 'elements_by_session'){
            elementFrame = data.frame(sessions = numeric(), element = numeric(), strength = numeric())
            for (session in 1:length(data)){
                holder = matrix(c(rep(session, length(data[[1]])), 1:length(data[[1]]), data[[session]]), ncol = 3)
                elementFrame = rbind(elementFrame, holder)
            }
            
            names(elementFrame) = c('session', 'element', 'str')
            
            
            ggplot(elementFrame, aes(x = session, y = element, fill = str)) + geom_tile(width = .8, height = .8) + scale_y_continuous(breaks = c(1, seq(5, 100, 5))) + 
                scale_fill_gradient2("Associative\nstrength", low = 'blue', high = 'red') + scale_x_continuous(breaks = c(1, 4, 8, 12, 16, 20, 24)) + theme_grey() +
                labs(y = 'Element', x = 'Pairs trained') + coord_cartesian(y = c(0, 101.2), x = c(0.2, 24.8)) +theme(axis.ticks.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
            ggsave(file = 'default.pdf', width = 4.5, height = 7.3)
            
        }
    }
}