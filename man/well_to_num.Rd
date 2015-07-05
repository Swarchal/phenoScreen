\name{well_to_num}
\alias{well_to_num}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Coverts well labels to numbers.
}
\description{
Converts well labels into corresponding numbers for either 96 or 384-well plates. Assumes wells are counted in a row-wise fashion, either starting each new row at the left hand side, or in a snaking fashion. For either 96 or 384-well plates.}
\usage{
well_to_num(wells, style = "normal", plate = 96)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wells}{
String containing well identifier. Numbers have to be double digits, i.e "A01"
}
  \item{plate}{
Plate format: either 96 or 384.
}
    \item{style}{
Method of counting. 'normal' starts each new row at the left hand side, i.e (...A11, A12, B01..). Whereas 'snake' counts in a continuous manner i.e (...A11, A12, B12, B11..)
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
num_to_well()
}
\examples{
well_to_num("A01")

well_to_num("P12", plate = 384)

well_to_num("P12", plate = 384, style = "snake")

wells <- c("A01", "A02", "A03")
well_to_num(wells)
}