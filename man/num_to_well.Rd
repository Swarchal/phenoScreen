\name{num_to_well}
\alias{num_to_well}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts numbers to well labels.
}
\description{
Converts numerical values into corresponding well label (e.g A02) for either 96 or 384-well plates}
\usage{
num_to_well(numbers, plate = 96L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{numbers}{
Vector of integers to be converted.
}
  \item{plate}{
Plate format: either 96 or 384.
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
well_to_num()
}
\examples{
num_to_well(1:96)
num_to_well(1:96, plate = 384)

nums <- c(1:10, 20:40, 60:96)
num_to_well(nums)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
