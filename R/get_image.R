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