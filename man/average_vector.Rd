\name{average_vector}
\alias{average_vector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate an average vector.
}
\description{
Caculates an average vector from rows of multiple vectors.
}
\usage{
average_vector(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Matrix of numeric dataframe. Columns are components of the vector, row per vector.
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Scott Warchal
}
\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- matrix(c(1,5,2,10,3,10), ncol = 2)
average_vector(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
