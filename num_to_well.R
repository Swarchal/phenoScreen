num_to_well <- function(numbers, style = normal){

    # numbers as column from data frame of as a vector of numbers?
    # can we have input as either and convert to most appropriate?
    
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
    
    well_list <- as.vector(well_list$well)
    
    new_df <- as.data.frame(numbers)
    new_df$well <- NA
    
    for(i in numbers){
        new_df$well[i] <- well_list[i]
    }
    return(new_df)
    
    #--------------------------------------------------------------------------
    
#     platemap <- mutate(platemap,
#                        Row=as.numeric(match(toupper(substr(well, 1, 1)), LETTERS)),
#                        Column=as.numeric(substr(well, 2, 5)))
#     
}