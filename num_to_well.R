num_to_well <- function(x){
    
    ## create initial matrices containing plate maps and well numbers
    #--------------------------------------------------------------------------
    # structure of 96-well plate
    well_map96 <- as.array(structure(c(
        "A01", "B01", "C01", "D01", "E01", "F01", "G01", 
        "H01", "A02", "B02", "C02", "D02", "E02", "F02", "G02", "H02", 
        "A03", "B03", "C03", "D03", "E03", "F03", "G03", "H03", "A04", 
        "B04", "C04", "D04", "E04", "F04", "G04", "H04", "A05", "B05", 
        "C05", "D05", "E05", "F05", "G05", "H05", "A06", "B06", "C06", 
        "D06", "E06", "F06", "G06", "H06", "A07", "B07", "C07", "D07", 
        "E07", "F07", "G07", "H07", "A08", "B08", "C08", "D08", "E08", 
        "F08", "G08", "H08", "A09", "B09", "C09", "D09", "E09", "F09", 
        "G09", "H09", "A10", "B10", "C10", "D10", "E10", "F10", "G10",
        "H10", "A11", "B11", "C11", "D11", "E11", "F11", "G11", "H11", 
        "A12", "B12", "C12", "D12", "E12", "F12", "G12", "H12"), .Dim = c(8L, 
                                                                          12L),
        .Dimnames = list(NULL, c("V1", "V2", "V3", "V4", "V5", 
                                 "V6", "V7", "V8", "V9", "V10", "V11", "V12"))))
    
    # well numbers in a 96-well plate (normal counting)
    well_numbers <- as.array(structure(c(
        1L, 13L, 25L, 37L, 49L, 61L, 73L, 85L, 2L, 14L, 26L, 
        38L, 50L, 62L, 73L, 86L, 3L, 15L, 27L, 39L, 51L, 63L, 73L, 87L, 
        4L, 16L, 28L, 40L, 52L, 64L, 73L, 88L, 5L, 17L, 29L, 41L, 53L, 
        65L, 73L, 89L, 6L, 18L, 30L, 42L, 54L, 66L, 73L, 90L, 7L, 19L, 
        31L, 43L, 55L, 67L, 73L, 91L, 8L, 20L, 32L, 44L, 56L, 68L, 73L, 
        92L, 9L, 21L, 33L, 45L, 57L, 69L, 73L, 93L, 10L, 22L, 34L, 46L, 
        58L, 70L, 73L, 94L, 11L, 23L, 35L, 47L, 59L, 71L, 73L, 95L, 12L, 
        24L, 36L, 48L, 60L, 72L, 73L, 96L), 
        .Dim = c(8L, 12L), .Dimnames = list(
            NULL, c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", 
                    "V10", "V11", "V12"))))
    
    well_numbers_snake <- as.array(structure(c(
        1L, 24L, 25L, 48L, 49L, 72L, 73L, 96L, 2L, 23L, 26L, 
        47L, 50L, 71L, 74L, 95L, 3L, 22L, 27L, 46L, 51L, 70L, 75L, 94L, 
        4L, 21L, 28L, 45L, 52L, 69L, 76L, 93L, 5L, 20L, 29L, 44L, 53L, 
        68L, 77L, 92L, 6L, 19L, 30L, 43L, 54L, 67L, 78L, 91L, 7L, 18L, 
        31L, 42L, 55L, 66L, 79L, 90L, 8L, 17L, 32L, 41L, 56L, 65L, 80L, 
        89L, 9L, 16L, 33L, 40L, 57L, 64L, 81L, 88L, 10L, 15L, 34L, 39L, 
        58L, 63L, 82L, 87L, 11L, 14L, 35L, 38L, 59L, 62L, 83L, 86L, 12L, 
        13L, 36L, 37L, 60L, 61L, 84L, 85L),
        .Dim = c(8L, 12L), .Dimnames = list(
            NULL, c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", 
                    "V10", "V11", "V12"))))
    
    ## need to correspond well ID's to a vector of given numbers
    #--------------------------------------------------------------------------
    # if given a number, want function to return a row and column
    return_coords <- function(x){
        coords <- which(well_map96 == x, arr.ind = TRUE)
        return(coords)
    }
    
    # can use example_coord[[1]] to extract row
    # and example_coord[[2]] for column
    # using these in a loop we can extract values from one matrix to another
    
    
}