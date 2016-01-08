#' Unique combination of two vectors
#'
#' Given two vectors will find all unique pairs.
#'
#' @param x Vector
#' @param y Vector
#' @param include.equals if FALSE will not return a pair that is the same
#' element paired against itself
#'
#' @return dataframe
#'
#' @export
#'
#' @examples
#' x <- c('one', 'two', 'three')
#' y <- c('a', 'b', 'c')
#' 
#' expand_grid_unique(x, y)

expand_grid_unique <- function(x, y, include.equals = FALSE){
  x <- unique(x)
  y <- unique(y)
  
  g <- function(i){
    z <- setdiff(y, x[seq_len(i - include.equals)])
    if(length(z)) cbind(x[i], z, deparse.level = 0)
  }
  
  do.call(rbind, lapply(seq_along(x), g))
}