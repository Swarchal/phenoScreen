\name{dist_map}
\alias{dist_map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots probability distribution of a given feature in a platemap format
}
\description{
Produces distribution plots facetted in a plate-layout format. Requires columns within a data frame labelled 'row' and 'column' indicating row and column, these can be produced with the num_to_well() function.
}
\usage{
dist_map(data, feature, row, column, title = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Dataframe containing all columns required.
}
  \item{feature}{
Numerical value indicating column index which contains the values to produce the distribution.
}
  \item{row}{
Numerical. Produced by the `num_to_well()` function.
}
  \item{column}{
Numerical. Produced by the `num_to_well()` function
}
  \item{title}{
String. Title of the plot.
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
\code{num_to_well()}}
\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
