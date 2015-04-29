load_csv <- function(path){
    
    temp = list.files(path, pattern="*.csv")
    
    for (i in 1:length(temp)){
        assign(temp[i], read.csv(temp[i]), envir = .GlobalEnv)
    }  
    
}
