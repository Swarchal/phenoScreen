\name{z_grid}
\alias{z_grid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots multiple heatmaps of values in a plate layout.
}
\description{
Converts numerical values and well labels into a heatmap in the form of a microtitre plate. Scales values via a z-score. Works for either 96 or 384-well plates.
}
\usage{
z_grid(data, well, plate_id, ncols = 2, plate = 96, title = "", palette = "Spectral")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Numerical values to be scaled and turned into a colour value
}
  \item{well}{
Vector of well identifiers, i.e "A01"
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
z_grid(data = df$vals,
      well = df$well,
      plate_id = df$plate,
      plate = 96,
      title = "Plot Title")
}