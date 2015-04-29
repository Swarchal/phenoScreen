###############################################################################
# rbind_all
#------------------------------------------------------------------------------
# opens all csv files in a folder, and assigns each an identifying column
# indicating which file it came from, and binds all data frames together
# in a row-wise fashion
#
# data frames have to have the same columns and column headers
#
# Warning: will produce new column named 'origin' containing string from which
# file data came from, may need to rename columns of existing column is already
# named 'origin'
###############################################################################

rbind_all <- function(path, skip = 0){
    
    full_path <- list.files(path, pattern = "*.csv", full.names = TRUE)
    short_path <- list.files(path, pattern = "*.csv", no.. = TRUE)
    
    for (file in full_path){
        # read the file
        new_file <- read.csv(file, header = TRUE, skip = skip)
        # assign new column named 'origin' with name of file
        new_file$origin <- paste(short_path)
        # bind dataframes together under variable 'master_file'
        master_file <- rbind(new_file)
    } 
    
    return(master_file)
}