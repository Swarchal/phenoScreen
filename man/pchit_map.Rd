\name{pchit_map}
\alias{pchit_map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Platemap to identify 'hits' in multivariate compound screens.
}
\description{
Produces a plot in the form of a micro-titre layout, taking the first principal component of multiple variables, with colours indicating wells above or below a nominated number of standard deviations from the plate average.
}
\usage{
pchit_map(data, well, plate = 96, threshold = 2, title = "", palette = "Spectral")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Vector(s) of numerical values to be scores.
}
  \item{well}{
Vector of well identifiers (i.e A01)
}
  \item{plate}{
Plate layout; either 96 or 384
}
  \item{threshold}{
Value of z-score/standard deviation from the mean to indicate hit. Default is +/- 2 standard deviations from the plate mean
}
  \item{title}{
Plot title
}
  \item{palette}{
RColorBrewer palette
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# example data
v1 <- rnorm(1:96)
v2 <- rnorm(1:96)
v3 <- rnorm(1:96)
wells <- num_to_well(1:96)
df <- data.frame(wells, v1, v2, v3)

pchit_map(data = df[, 2:4],
          well = df$wells,
          threshold = 1.5,
          title = "pc_hit: Example plot")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
