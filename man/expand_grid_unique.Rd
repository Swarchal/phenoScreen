\name{expand_grid_unique}
\alias{expand_grid_unique}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Unique combinations of two vectors.
}
\description{
If given two vectors will find unique combinations of the two vectors.
}
\usage{
expand_grid_unique(x, y, include.equals = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Vector
}
  \item{y}{
Vector
}
  \item{include.equals}{
Boolean, if FALSE will not return a combination that is the same element against itself.
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
x <- c('one', 'two', 'three')
y <- c('a', 'b', 'c')

expand_grid_unique(x, y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
