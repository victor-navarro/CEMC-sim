pasteFiles = function(filename){
    
    setwd("/Users/edwardwasserman/Google Drive/Wasserman's Lab/R/CEMC/data")
    itemsFound = F
    itemsPasted = c()
    for (item in 1:length(dir())){
        if (ifelse(is.na(pmatch(filename, dir()[item])), 0, 1)){
            if (!itemsFound){
                summary = read.table(dir()[item], header = T, sep = '\t')
                itemsFound = T
            }else{
                summary = rbind(summary, read.table(dir()[item], header = T, sep = '\t'))
            }
            itemsPasted = cbind(itemsPasted, dir()[item])
        }
    }
    unlink(itemsPasted)
    write.table(summary, paste('summary_', filename, '.txt', sep = ''), row.names = F, sep = '\t')
    setwd("/Users/edwardwasserman/Google Drive/Wasserman's Lab/R/CEMC")
    return(summary) 
}