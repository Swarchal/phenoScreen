#' Display a merged image from RGB image URLs
#' 
#' Produces an RGB merge of images from image URLs
#' 
#' @param df dataframe containing columns of interest
#' @param row_number row number containing image URLs of interest
#' @param display display options, either a merged image or separate images
#'  for each channel
#' @param green_url URL for green channel
#' @param blue_url URL for blue channel
#' @param red_url URL for red channel
#'
#' @export
#'
#' @return a raster image to the display window


get_image <- function(df, row_number,
                      display = "merge",
                      green_url = "URL_Actin",
                      blue_url = "URL_DNA",
                      red_url = "URL_HCS"){
  require(raster)
  require(RColorBrewer)
  
  sanitize_files <- function(x){
    x_char <- as.character(x)
    # expand to file path
    raw <- normalizePath(x_char)
    # replace "%20" with a space
    spaced <- gsub("%20", " ", raw)
    # remove nonsense before actual file name that win recognises
    truncated <- unlist(strsplit(spaced, split='file:\\', fixed = TRUE))[2]
    return(truncated)
  }
  
  red_img   <- raster(suppressWarnings(sanitize_files(df[[red_url]][row_number])))
  green_img <- raster(suppressWarnings(sanitize_files(df[[green_url]][row_number])))
  blue_img  <- raster(suppressWarnings(sanitize_files(df[[blue_url]][row_number])))
  
  image_stack <- brick(red_img, green_img, blue_img)
  
  if (display == "merge"){
    plt <- plotRGB(image_stack,
                   r = 1,
                   g = 2,
                   b = 3,
                   stretch = "lin")
  }
  if(display == "tile"){
    cols <- cols <- colorRampPalette(brewer.pal(9,"Greys"))(max(values(red_img)))
    plt <- plot(image_stack, col = rev(cols))
  }
  
}