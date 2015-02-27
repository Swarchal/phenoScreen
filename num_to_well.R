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
    
    # dataframe containing all wells in snaking order
    well_list_snake <- structure(list(
        well = structure(c(
            1L, 2L, 3L, 4L, 5L, 6L, 7L, 
            8L, 9L, 10L, 11L, 12L, 24L, 23L, 22L, 21L, 20L, 19L, 18L, 17L, 
            16L, 15L, 14L, 13L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 
            34L, 35L, 36L, 48L, 47L, 46L, 45L, 44L, 43L, 42L, 41L, 40L, 39L, 
            38L, 37L, 49L, 50L, 51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 
            60L, 72L, 71L, 70L, 69L, 68L, 67L, 66L, 65L, 64L, 63L, 62L, 61L, 
            73L, 74L, 75L, 76L, 77L, 78L, 79L, 80L, 81L, 82L, 83L, 84L, 96L, 
            95L, 94L, 93L, 92L, 91L, 90L, 89L, 88L, 87L, 86L, 85L),
            .Label = c(
                "A01", 
                "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", 
                "A11", "A12", "B01", "B02", "B03", "B04", "B05", "B06", "B07", 
                "B08", "B09", "B10", "B11", "B12", "C01", "C02", "C03", "C04", 
                "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "D01", 
                "D02", "D03", "D04", "D05", "D06", "D07", "D08", "D09", "D10", 
                "D11", "D12", "E01", "E02", "E03", "E04", "E05", "E06", "E07", 
                "E08", "E09", "E10", "E11", "E12", "F01", "F02", "F03", "F04", 
                "F05", "F06", "F07", "F08", "F09", "F10", "F11", "F12", "G01", 
                "G02", "G03", "G04", "G05", "G06", "G07", "G08", "G09", "G10", 
                "G11", "G12", "H01", "H02", "H03", "H04", "H05", "H06", "H07", 
                "H08", "H09", "H10", "H11", "H12"),
            class = "factor")), .Names = "well",
        class = "data.frame", row.names = c(NA, -96L))
    
    numbers_vector <- as.vector(numbers)
    value <- function(x){return(well_list[x,])}
    value_snake <- function(x){return(well_list_snake[x,])}
    
    if(style == normal){well_id <- as.vector(sapply(numbers_vector, value))}
    if(style == snake){well_id <- as.vector(sapply(numbers_vector, value_snake))}
    
    return(well_id)
    
}