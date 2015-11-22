\name{raw_map}
\alias{raw_map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots values as heatmap in plate layout.
}
\description{
Produces a heatmap in the layout of a 96-well plate. Much like \code{z_map()}, but with no scaling of the numerical values.}
\usage{
raw_map(data, well, plate, title = "", palette = "YlGnBu")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Vector of numerical values to use for plotting.
}
  \item{well}{
Vector of characters to denote well identifiers. e.g ``A01''
}
  \item{plate}{
Number of wells in the full plate, either 96 or 384.
}
  \item{title}{
String, title of the plot.
}
  \item{palette}{
RColorBrewer palette.
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
\code{hit_map()}, \code{z_map()}, \code{pc_map()}
}
\examples{

df <- data.frame(vals = rnorm(1:384),
  well = num_to_well(1:384, plate = 384))

raw_map(data = df$vals,
        well = df$well,
        title = "Title of plot")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
