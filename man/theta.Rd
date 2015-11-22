\name{theta}
\alias{theta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Angle between two vectors.
}
\description{
Caculates and angle between two vectors, output in degrees.
}
\usage{
theta(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
First vector.
}
  \item{b}{
Second vector.
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
\note{theta <- function(a, b){
    
    # computes the angle bewteen two vectors ('a' and 'b')
    norm_vec <- function(x) sqrt(x %*% x)
    
    theta <- as.vector(
        acos(a %*% b / (norm_vec(a) * norm_vec(b)))) * 180/pi
    return(theta)
}

a <- as.vector(c(1, 2))
b <- as.vector(c(3, 1))df <- data.frame(vals = rnorm(1:384),
  well = num_to_well(1:384, plate = 384))
theta(a,b)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
a <- c(1, 2)
b <- c(3, 1)

theta(a,b)

a <- c(1, 2)
b <- c(-1, -2)
theta(a, b)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
