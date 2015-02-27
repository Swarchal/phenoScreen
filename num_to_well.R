###############################################################################
# num_to_well()
#------------------------------------------------------------------------------
# Converts well number to well ID, e.g well 1 -> A01, well 96 -> H12
# Currently returns a vector of well IDs matching the input numbers
# 
# Need to:
# Ceed to consider if given a data frame, and appending a column of well IDs
#
# Also, need to add options if well numbers follow a non-conventional approach
# such as snaking
# 
# Option for 384 well plates
###############################################################################

num_to_well <- function(numbers, style = normal){

    # numbers as column from data frame of as a vector?
    # can we have input as either form and convert to most appropriate?
    
    # dataframe containing all wells in order (normal)
    well_list <- structure(list(
        well = structure(1:96,.Label = c(
            "A01", "A02", 
            "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", 
            "A12", "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", 
            "B09", "B10", "B11", "B12", "C01", "C02", "C03", "C04", "C05", 
            "C06", "C07", "C08", "C09", "C10", "C11", "C12", "D01", "D02", 
            "D03", "D04", "D05", "D06", "D07", "D08", "D09", "D10", "D11", 
            "D12", "E01", "E02", "E03", "E04", "E05", "E06", "E07", "E08", 
            "E09", "E10", "E11", "E12", "F01", "F02", "F03", "F04", "F05", 
            "F06", "F07", "F08", "F09", "F10", "F11", "F12", "G01", "G02", 
            "G03", "G04", "G05", "G06", "G07", "G08", "G09", "G10", "G11", 
            "G12", "H01", "H02", "H03", "H04", "H05", "H06", "H07", "H08", 
            "H09", "H10", "H11", "H12"),class = "factor")),
        .Names = "well", class = "data.frame", row.names = c(NA, -96L))
    
    numbers_vector <- as.vector(numbers)
    value <- function(x){return(well_list[x,])}
    well_id <- as.vector(sapply(numbers_vector, value))
    return(well_id)
    
}