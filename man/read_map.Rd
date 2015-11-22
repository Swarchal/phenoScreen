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
read_map(data, map, well_col = well, new_col_name = "header" verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Existing dataframe, with wellID's under the header of 'well'
}
  \item{map}{
Matrix in plate-layout format containing information to be added to dataframe
}
  \item{verbose}{
Boolean, is true will add details indicating the column and row for each well. If FALSE, will only return the annotations.
}
  \item{well_col}{
Name of column containing well_identifiers.
}
  \item{new_col_name}{
Name to give new column containing annotations.
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
Any missing wells (not a full plate) have to be filled with a placeholder to preserve row and column spacings.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
