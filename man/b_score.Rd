\name{b_score}
\alias{b_score}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates a B-score smooth for individual microtitre plates.
}
\description{
A two-way median smooth utilising medpolish() to normalise for row and column effects in microtitre plates. Normalises each row and column to have a median of zero.
}
\usage{
b_score(data, well, plate, normalise = FALSE, matrix = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Vector containing numerical values to normalise.
}
  \item{well}{
Vector of well labels, in the format of 'A01' for example.
}
  \item{plate}{
Integer, indicating number of wells in the plate. Either 96 or 384 well plates acceptable.
}
  \item{normalise}{
If true, will return a matrix containing the raw input values minus the residuals.
}
  \item{matrix}{
If TRUE will return the values in the form of a matrix representing the layout of a 96-well plate. If FALSE, returns a dataframe of well identifiers (platemap) and residual values.
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
Need all wells of the plate to be entered, use NA if missing certain wells, otherwise will not form the matrix in the correct layout.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
b_score(data = wells_example$val,
        well = wells_example$well)

b_score(data = df$values,
        well = df$well,
        plate = 384)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
