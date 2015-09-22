\name{norm_vector}
\alias{norm_vector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the norm of a vector.
}
\description{
Calculates the length (norm if > 2 elements) of a vector.
}
\usage{
norm_vector(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Input vector
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
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- c(3, 8)
norm_vec(x)

x <- c(3, 4, 5, 2)
norm_vec(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
