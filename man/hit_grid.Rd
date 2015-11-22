\name{hit_grid}
\alias{hit_grid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots multiple platemaps with identified hits.
}
\description{
Converts numerical values and well labels into identified hits in the form of multiple microtitre plates. Hits are calculated as wells above and below a specified number of standard deviations.
}
\usage{
hit_grid(data, well, plate_id, threshold = 2, ncols = 2, plate = 96, title = "", scale = "all" palette = "Spectral")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Numerical values to be scaled and turned into a colour value
}
  \item{well}{
Vector of well identifiers, i.e "A01"
}
  \item{threshold}{
Numerical value of standard deviations away from the mean at which to define a hit.
}
  \item{ncols}{
Number of columns when plotting multiple plates.
}
  \item{plate_id}{
Vector of plate label/identifiers, i.e "Plate_1"
}
  \item{scale}{
Not yet added.
}
  \item{plate}{
Plate format; either 96 or 384
}
  \item{title}{
Title for plot
}
  \item{scale}{
  Not in use.
}
  \item{palette}{
RColorBrewer palette, default is recommended.
}
}
\details{
Values are scaled over the entire population. Option to scale values on a plate-by-plate basis to be added.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
http://bconnelly.net/2014/05/plotting-microtiter-plate-maps/
}
\author{
Scott Warchal
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
hit_grid(data = df$vals,
    well = df$well,
    plate_id = df$plate,
    plate = 96,
    title = "Plot Title")
}