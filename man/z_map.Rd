\name{z_map}
\alias{z_map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots a heatmap of values in a plate layout.
}
\description{
Converts numerical values and well labels into a heatmap in the form of a microtitre plate. Scales values via a z-score. Works for either 96 or 384-well plates.
}
\usage{
z_map(data, well, plate = 96, title = "", palette = "Spectral")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Numerical values to be scaled and turned into a colour value
}
  \item{well}{
Vector of well identifiers, i.e "A01"
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

df <- data.frame(vals = rnorm(1:384),
  well = num_to_well(1:384, plate = 384))

z_map(data = df$vals,
      well = df$well,
      plate = 384,
      title = "Plot Title")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
