\name{raw_grid}
\alias{raw_grid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots multiple platemaps with heatmap of the raw numerical values.
}
\description{
Converts univariate numerical data and well labels into a heatmap in the form of multiple microtitre plates.
}
\usage{
raw_grid(data, well, plate_id, ncols = 2, plate = 96, title = "", palette = "YlGnBu")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Numerical values to be scaled and turned into a colour value
}
  \item{well}{
Vector of well identifiers, i.e "A01"
}
    \item{ncols}{
Number of columns in order to place multiple platemaps.
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
  \item{palette}{
RColorBrewer palette, default is recommended.
}
}
\details{}
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
raw_grid(data = df$vals,
    well = df$well,
    plate_id = df$plate,
    plate = 96,
    title = "Plot Title")
}