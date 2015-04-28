\name{read_map}
\alias{read_map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to annotate existing dataframes from an external matrix containing well information.
}
\description{
Annotates data frame with well identifiers, corresponding to a separate dataframe in a plate-layout containing values/labels. 
}
\usage{
read_map(data, map)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Existing dataframe, with wellID's under the header of 'well'
}
  \item{map}{
Matrix in plate-layout format containing information to be added to dataframe
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
%%  ~~who you are~~
}
\note{
Any missing wells (not a full plate) have to be filled with a placeholder to preserve row and column spacings.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
read_map(df_existing, concentrations)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
