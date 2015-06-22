\name{plot_confidence}
\alias{plot_confidence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to 2 dimensional plot scatter plot with confidence interval.
}
\description{
Plots a scatter plot with a bi-variate confidence band calculated via a two-dimensional kernal density estimation.
}
\usage{
plot_confidence(x, y, confidence = 0.95, title = "", xlab = "x", ylab = "y")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Values to be used on the x-axis
}
  \item{y}{
Values to be used on the y-axis
}
  \item{confidence}{
Confidence interval between 0 and 1, default is at 0.95, i.e 95\% confidence interval
}
  \item{title}{
Title for the plot
}
  \item{xlab}{
Axis label for the x-axis
}
  \item{ylab}{
Axis label for the y-axis
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
x <- rnorm(1000)
y <- rnorm(1000)
plot_confidence(x, y, title = "Example plot")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
