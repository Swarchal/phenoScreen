\name{z_factor_scan}
\alias{z_factor_scan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple z-factor calculations.
}
\description{
Calculates z-factors between two compounds for multiple variables and returns the z-factors and associated variables above a specified cut off: default = 0.5.
}
\usage{
z_factor_scan(x)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{data}{
Dataframe. Dataset to be used, containing only numerical columns of variables, and single column containing treatment labels.
}
  \item{treatments}{
Vector of factors containing treatment groups. Recommended to be positive and negative controls.
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
### Treatment groups must be under the column label 'header' ###
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
z_factor_scan(data = tidy_dataset, 
			 treatments = c("DMSO", "STS"),
			 cutoff = 0.3)

cntrols <- c("DMSO", "STS")
z_factor_scan(tidy_dataset, cntrols, 0.3)             
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
