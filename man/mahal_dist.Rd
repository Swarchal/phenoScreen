\name{mahal_dist}
\alias{mahal_dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simplified Mahalanobis Distance
}
\description{
Calculates Mahalanobis distance of a dataframe or a matrix. Option for robust method for findin the center.
}
\usage{
mahal_dist(x, center = "mean", na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Dataframe or a matrix.
}
  \item{center}{
Method to calculate the group center, either mean or median,
}
  \item{na.rm}{
Whether to remove missing values before calculating center or not.
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
data(iris)

iris_values <- iris[,1:4]
mahal_dist(iris_values)
mahal_dist(iris_values, center = "median")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
