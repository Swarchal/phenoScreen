\name{pc_map}
\alias{pc_map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Principal component heatmap in plate layout.
}
\description{
Takes the values and well identifiers, calculates the first principal component, scales and plots the component as a heatmap in the form of a 96 or 384-well plate. A way to quickly show variation of multi-parametric data within a plate.
}
\usage{
pc_map(data, well, plate = 96, title = "", palette = "Spectral")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Vector of numerical data with which to calculate the first principcal component.
}
  \item{well}{
Vector containing well identifiers, i.e "A01".
}
  \item{plate}{
Integer indicating number of wells in the plate. Options are currently 96 or 384-well plates.
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
pc_map(data = df[, 2:50],
       well = df$well,
       title = "PC plate heatmap",
       plate = 384)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
