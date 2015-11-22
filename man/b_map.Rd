\name{b_map}
\alias{b_map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots a heatmap of b-score normalised values in a plate layout.
}
\description{
Transforms numerical values using the b-score normalisation process to account for row and column effects values and uses well labels to plot a normalised heatmap in the form of a microtitre plate. Works for either 96 or 384-well plates.
}
\usage{
b_map(data, well, normalise = FALSE, plate = 96, title = "", palette = "Spectral")
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
  \item{normalise}{
Not currently used
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
b_map(data = df$vals,
      well = df$well,
      plate = 96,
      title = "Plot Title")
      
b_map(data = df_384$vals,
      well = df_384$well,
      plate = 384)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
