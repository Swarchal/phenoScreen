\name{cv_check}
\alias{cv_check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the covariance of variation across groups.
}
\description{
Calculates covariance of variation as a quality control measure.
}
\usage{
cv_check(data, group)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Vector of numerical values with which to calculate the coefficient of variation with.
}
  \item{group}{
Vector containing factors with which to divide / group the numerical values, calculates a coefficient of variation for each group.
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
a <- rnorm(100, 10, 1)
b <- rnorm(100, 10, 10)
vals <- c(a, b)
grouping <- c(rep('a', 100), rep('b', 100))
df <- data.frame(vals, grouping)
cv_check(data = df$vals,
          group = df$grouping)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
