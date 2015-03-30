###############################################################################
# b_score()
#------------------------------------------------------------------------------
# Transforms row and column medians to zero
#
# The residual (r_ijp) of the measurement for row `i` and column `j` on the
# `p`th plate is obtained by fitting a two-way median polish.
#
# The residual is defined as the difference between the observed result (y_ijp)
# and the fitted value (yhat_ijp, defined as the estimated average of the plate 
# (mu_p) + estimated systematic measurement column offset for
# column j` on plate p.
#------------------------------------------------------------------------------
# Argument 'data' should be entered as a dataframe with the well identifiers
# under a column labelled as 'well'
#
# 'val_col' argument is the column within 'data' that contains the numerical
# values of interest. Default is the second column
#
# The 'normalise' argument, is defaulted at FALSE, if TRUE (or anything other
# than FALSE), it will return a matrix containing the raw values minus the 
# residuals
#
# Argument: `matrix`, if TRUE, returns the results in the form of a matrix
# representing the layout of 96-well plate
# if FALSE, returns a dataframe of wellID and corresponding residual
#------------------------------------------------------------------------------
# N.B: well identifiers are required to format the data into correct plate
# layout with the column name as 'well'
# 
# Currently only works with a full 96-well plate, will screw up row/columns
# when not all wells are used. need to check if NA/NaN will work in a numerical
# matrix as placeholders to preserve well spacings.
###############################################################################

b_score <- function(data, val_col = 2L, normalise = FALSE, matrix = FALSE){
	
    require(dplyr)
    
    # need to transform columns of wellID and data into
    # matrix corresponding to well positions:
    platemap <- mutate(
        data,
        row = as.numeric(match(toupper(substr(well,1,1)),LETTERS)),
        column = as.numeric(substr(well,2,5))
    )
    
    # ensure data is ordered properly before passing to matrix()
    platemap <- platemap[order(platemap$row, platemap$column), ]
    
    # transform into 12*8 matrix (96-well plate)
    # fills matrix by in a row-wise fashion i.e, A01, A02 ...
    mat_plate_map <- matrix(platemap[,val_col],
                            nrow = 8,
                            ncol = 12,
                            byrow = TRUE)
    
	# median polish of the data
	data_pol <- medpolish(mat_plate_map,
                          na.rm = TRUE)
    if (matrix == TRUE){ # returns results in matrix form
        if (normalise == TRUE){
            return(mat_plate_map - data_pol$residuals) # values minus residuals
        } else if (normalise == FALSE){
            return(data_pol$residuals) # returns the raw residuals
        } else {
            return("Error: 'normalise' needs to be either TRUE or FALSE")
        }
        
    }
    if (matrix == FALSE){ # return results in dataframe with well labels
        
        # transpose of residual matrix (as counts in column-wise fashion)
        # now well numbers correspond i.e t_out[12] = A12, t_out[13] = B01
        t_out <- t(data_pol$residuals)
        
        # num_to_well function:
        num_to_well <- function(numbers){
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
        
        # 1:96 elements of residuals corresponding to 1:96 of num_to_well
        # produce dataframe of two columns
        df <- NULL
        
        for (num in 1:length(t_out)){
            df$residual[num] <- t_out[num]
            df$well[num] <- num_to_well(num)
        }
        
        df <- as.data.frame(
            cbind("well" = df$well,
                  "residual" = df$residual))
        # change residuals from factor to numeric
        df$residual <- as.numeric(as.character(df$residual))
        return(df) 
    } else {
        return("Error: matrix has to be either TRUE or FALSE")
    }
}
