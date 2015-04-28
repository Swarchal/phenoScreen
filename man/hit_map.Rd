\name{hit_map}
\alias{hit_map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Platemap to identify 'hits' in compound screens.
}
\description{
Produces a plot in the form of a micro-titre layout, with colours indicating wells abova or below a nominated threshold.
}
\usage{
hit_map(values, platemap, plate = 96, threshold = 2, title = "", palette = "Spectral")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{values}{
Vector of numerical values to be scores.
}
  \item{platemap}{
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
hitmap(values = df$vals,
       platemap = df$well,
       plate = 384,
       threshold = 3,
       title = "Awesome plot")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
