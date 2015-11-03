\name{return_features}
\alias{return_features}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return heavily weighted features for a pca vector.
}
\description{
For a given principal component analysis, when given a vector this function will return the features (variables) that best describe the vector. This is calculated through the weighting the rotation values by the ratio of the vector.
}
\usage{
return_features(pca_data, pca_vector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pca_data}{
An object of class prcomp().
}
  \item{pca_vector}{
A two-element vector. e.g c(PC1, PC2), or c(x, y).
}
  \item{n}{
How many features to return for each element of the vector. I.e with n = 2, will return the top 4 features that best decribe that vector.
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
data(iris)
pca_iris <- prcomp(iris[,1:4])

# features for a vector pointing straight up, essentially just
# those for PC2 in a positive direction.
return_features(pca_iris, c(0,1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
