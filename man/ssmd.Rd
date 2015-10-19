\name{ssmd}
\alias{ssmd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stictly standardised mean differnce.
}
\description{
strictly standardized mean difference (SSMD) is a measure of effect size. SSMD is the mean divided by the standard deviation of a difference between two random values each from one of two groups.
}
\usage{
ssmd(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
Numerical vector of group a.
}
  \item{b}{
Numerical vector of group b.
}
}
\details{
As a statistical parameter SSMD, denoted as Beta is defined as the ratio of mean to standard deviation of two groups. Assume that one group has mean mu_a and variance sigma_(a)^2  and the other group as the mean mu_b and variance sigma_(b)^2. The covariance between the two groups is sigma_(ab)

Beta = mu_a - mu_b / sqrt(sigma_(a)^2 + sigma_(b)^2 - sigma_(ab))
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


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
a <- rnorm(100, 1)
b <- rnorm(100, 10)
ssmd(a, b)
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
