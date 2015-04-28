\name{z_factor}
\alias{z_factor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates z-factor for two distributions.
}
\description{
Calculates a factor for two distributions, used to assess the separation between positive and negative controls in high-throughput screening assays.
}
\usage{
z_factor(positive, negative)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{positive}{
Vector of numerical values for positve control
}
  \item{negative}{
Vector of numerical values for negative control
}
}
\details{
A value above 0.5 is considered well separated and corresponds to a difference of 12 standard deviations.
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
Will auto-select the positive and negative controls based on the means of the input vectors, will over-ride the argument selections.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- rnorm(100, 100)
y <- rnorm(100, 10)
z_factor(x, y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
