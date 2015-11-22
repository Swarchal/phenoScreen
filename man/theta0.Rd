\name{theta0}
\alias{theta0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Angle between a vector and the origin
}
\description{
Caculates and angle between a given vector and the origin.
}
\usage{
theta0(a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
Vector.
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{    # need to measure the angle from a set reference, so it's always anticlockwise
    # so in vector(x,y), if y is negative; need to add 180 to theta
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
\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
a <- c(1, 0)
theta0(a)

a <- c(-1, 0)
theta0(a)

a <- c(-1, -1)
theta0(a)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
