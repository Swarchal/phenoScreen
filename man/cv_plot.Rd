\name{cv_plot}
\alias{cv_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots coefficient of variation values for a given dataframe.
}
\description{
For a given data frame containing numerical values split into different factors, cv_plot() will calculate and plot coefficient of variation values.
}
\usage{
cv_plot(data, group, fancy = FALSE, trend = FALSE, rotate = FALSE, title = "CV Plot")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Vector of numerical values with which to calculate the coeffiecient of variation.
}
  \item{group}{
Vector of factors with which to split to values in `data` into separate groupings to compare.
}
  \item{fancy}{
Boolean. If TRUE uses ggplot to produce plot.
}
  \item{trend}{
Boolean. If TRUE adds a trend line to the points - useful for time groups.
}
  \item{rotate}{
Boolean. If TRUE rotates x-axis labels.
}
  \item{title}{
String. Title of the plot. Default is 'CV Plot'.
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
If additional graphical parameters are required, I recommend using cv_check and using the output to make a custom plot.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
a <- rnorm(100, 10, 1)
b <- rnorm(100, 10, 10)
vals <- c(a, b)
grouping <- c(rep('a', 100), rep('b', 100))
df <- data.frame(vals, grouping)

cv_plot(data = df$vals,
        group = df$grouping)

cv_plot(data = df$vals,
        group = df$grouping,
        fancy = TRUE,
        trend = TRUE,
        title = "Fancy CV Plot")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
