#' Calculates a b-score smooth for individual microtitre plates
#'
#' Performs a two-way median smooth to normalise for row and column effects in
#' microtitre plates. Normalises each to and column to have a median of zero.
#'
#' @param data Vector containing numerical values to normalise
#' @param well Vector of well labels, i.e "A01"
#' @param plate Integer indicating number of wells in the complete plate (96 or 384)
#' @param normalise If true, will return a matrix of residuals subtracted from raw input values
#' @param matrix If true, will return the values in the form of a matrix.
#'        If false, will return a dataframe of wellID and values
#'
#' @return Normalised values
#'
#' @export
#'
#' @note Need all the wells of the plate to be entered, empty wells should be listed as NA
#'
#' @examples
#' df <- data.frame(well = num_to_well(1:96),
#'      val = rnorm(96))
#' b_score(data = df$val, well = df$well)

b_score <- function(data, well,
                    plate = 96,
                    normalise = FALSE,
                    matrix = FALSE){
    
    require(dplyr)
    
    # need to transform columns of wellID and data into
    # matrix corresponding to well positions:
    platemap <- as.data.frame(well)
    names(platemap)[1] <- "well"
    
    platemap <- mutate(
        platemap,
        row = as.numeric(match(toupper(substr(well,1,1)),LETTERS)),
        column = as.numeric(substr(well,2,5))
    )
    
    # ensure data is ordered properly before passing to matrix()
    platemap <- platemap[order(platemap$row, platemap$column), ]
    
    
    if (length(well) > plate){
        warning("Invalid plate selection. The data given has more rows then number of wells. \nAre you sure argument 'plate' is correct for the number of wells in your data? \nnote: Default is a 96-well plate.",
                call. = FALSE)
    }
    if (plate > 2 * length(well)){
        warning("Invalid plate selection. The data given has more rows then number of wells. \nAre you sure argument 'plate' is correct for the number of wells in your data? \nnote: Default is a 96-well plate.",
                call. = FALSE)
    }
    if (plate == 96){
        # transform into 12*8 matrix (96-well plate)
        # fills matrix in a row-wise fashion i.e, A01, A02 ...
        mat_plate_map <- matrix(data,
                                nrow = 8,
                                ncol = 12,
                                byrow = TRUE)
    } else if (plate == 384){
        # transform into 24*16 matrix (384-well plate)
        # fills matrix in a row-wise fashion, i.e A01, A02 ...
        mat_plate_map <- matrix(data,
                                nrow = 16,
                                ncol = 24,
                                byrow = TRUE)
    } else{
        stop("Not a plate format. \nArgument 'plate', should be 96 or 384.",
             call. = FALSE)
    }
    
    # median polish of the data
    data_pol <- medpolish(mat_plate_map,
                          na.rm = TRUE)
    if (matrix == TRUE){ # returns results in matrix form
        if (normalise == TRUE){
            return(mat_plate_map - data_pol$residuals) # values minus residuals
        } else if (normalise == FALSE){
            return(data_pol$residuals) # returns the raw residuals
        } else stop("normalise has to be either TRUE or FALSE")
        
    }
    if (matrix == FALSE){ # return results in dataframe with well labels
        
        # transpose of residual matrix (as counts in column-wise fashion)
        # now well numbers correspond i.e t_out[12] = A12, t_out[13] = B01
        t_out <- t(data_pol$residuals)
        
        # 1:96 elements of residuals corresponding to 1:96 of num_to_well
        # produce dataframe of two columns
        df <- NULL
        
        for (num in 1:length(t_out)){
            df$residual[num] <- t_out[num]
            df$well[num] <- num_to_well(num, plate = plate)
        }
        
        df <- as.data.frame(
            cbind("well" = df$well,
                  "residual" = df$residual))
        # change residuals from factor to numeric
        df$residual <- as.numeric(as.character(df$residual))
        return(df) 
    } else stop("Matrix has to be either TRUE or FALSE")
}
