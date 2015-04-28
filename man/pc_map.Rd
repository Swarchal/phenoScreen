\name{pc_map}
\alias{pc_map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Principal component heatmap in plate layout.
}
\description{
Takes the values and well identifiers, calculates the first principal component, scales and plots the component as a heatmap in the form of a 96-well plate. A way to quickly show variation of multi-parametric data within a plate.
}
\usage{
pc_map(data, well, title = "", palette = "Spectral")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Vector of numerical data with which to calculate the first principcal component.
}
  \item{well}{
Vector containing well identifiers, i.e "A01".
}
  \item{title}{
String. Title of the plot
}
  \item{palette}{
RColorBrewer palette. Default is recommended.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Scott Warchal
}
\note{
Currently only works with 96-well plates. 394-well plate functionality to come later.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{z_map()} and \code{hit_map()}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, well, title = "", palette = "Spectral") 
{
    pca_data <- princomp(data)
    pc1 <- pca_data$score[, 1]
    z_map <- function(values, platemap, title = "", palette = "Spectral") {
        require(ggplot2)
        require(dplyr)
        require(RColorBrewer)
        platemap <- as.data.frame(platemap)
        names(platemap)[1] <- "well"
        platemap <- mutate(platemap, Row = as.numeric(match(toupper(substr(well, 
            1, 1)), LETTERS)), Column = as.numeric(substr(well, 
            2, 5)))
        values <- as.data.frame(values)
        scaled_data <- scale(values)
        platemap <- cbind(platemap, scaled_data[, 1])
        names(platemap)[4] <- "scaled_data"
        my_cols <- brewer.pal(3, palette)
        plt <- ggplot(data = platemap, aes(x = Column, y = Row)) + 
            geom_point(data = expand.grid(seq(1, 12), seq(1, 
                8)), aes(x = Var1, y = Var2), color = "grey90", 
                fill = "white", shape = 21, size = 6) + geom_point(aes(fill = scaled_data), 
            colour = "gray20", shape = 21, size = 10) + coord_fixed(ratio = (13/12)/(9/8), 
            xlim = c(0.5, 12.5), ylim = c(0.5, 8.5)) + scale_y_reverse(breaks = seq(1, 
            8), labels = LETTERS[1:8]) + scale_x_continuous(breaks = seq(1, 
            12)) + scale_fill_gradient2("z-score", low = my_cols[3], 
            high = my_cols[1], mid = my_cols[2]) + ggtitle(title) + 
            theme_bw()
        return(plt)
    }
    plot_pc_map <- z_map(pc1, well, title, palette)
    return(plot_pc_map)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
