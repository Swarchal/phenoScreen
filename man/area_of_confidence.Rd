\name{area_of_confidence}
\alias{area_of_confidence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate area contained within 2D KDE confidence boundary.
}
\description{
Produces a confidence boundary with 2D kernel density estimate from x,y points, and then calculates the area within the confidence boundary.
}
\usage{
area_of_confidence(x, y, confidence = 0.9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Values to be used on the x-axis
}
  \item{y}{
Values to be used on the y-axis
}
  \item{confidence}{
Confidence interval between 0 and 1, default is at 0.9, i.e 9\% confidence interval
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
x <- rnorm(1000)
y <- rnorm(1000)

area_of_confidence(x, y)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
