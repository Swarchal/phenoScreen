#' ggplot plate object
#' 
#' internal function
#' 
#' @param platemap platemap dataframe produced by \code{plate_map}
#' @return ggplot object
#' @import ggplot2
#' @export

plt96 <- function(platemap){
    ggplot(data = platemap, aes(x = Column, y = Row)) +
        geom_point(data = expand.grid(seq(1, 12), seq(1, 8)), aes(x = Var1, y = Var2),
                   color = "grey90", fill = "white", shape = 21, size = 6) +
        geom_point(aes(fill = values), colour = "gray20", shape = 21, size = 10) +
        coord_fixed(ratio = (13 / 12) / (9 / 8), xlim = c(0.5, 12.5), ylim = c(0.5, 8.5)) +
        scale_y_reverse(breaks = seq(1, 8), labels = LETTERS[1:8]) +
        scale_x_continuous(breaks = seq(1, 12))
}

#' ggplot plate object
#' 
#' internal function
#' 
#' @param platemap platemap dataframe produced by \code{plate_map}
#' @return ggplot object
#' @import ggplot2
#' @export

plt384 <- function(platemap){
    ggplot(data = platemap, aes(x = Column, y = Row)) +
        geom_point(data = expand.grid(seq(1, 24), seq(1, 16)), aes(x = Var1, y = Var2),
                   color = "grey90", fill = "white", shape = 22, size = 3) +
        geom_point(aes(fill = values), colour = "gray20", shape = 22, size = 5) +
        coord_fixed(ratio = (24.5 / 24) / (16.5 / 16), xlim = c(0.5, 24.5), ylim = c(0.5, 16.5)) +
        scale_y_reverse(breaks = seq(1, 16), labels = LETTERS[1:16]) +
        scale_x_continuous(breaks = seq(1, 24))
}
