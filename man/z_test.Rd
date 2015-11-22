\name{z_test}
\alias{z_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multivariate Z test
}
\description{
Calculates differences in multivariate distributions with the kolmogorov smirnov test.
}
\usage{
z_test(df_a, df_b, plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df_a}{
Matrix or dataframe containing only numerical values.
}
  \item{df_b}{
Matrix or dataframe containing only numerical values
}
  \item{plot}{
Boolean, whether to produce a plot.
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
#sample data

a <- rnorm(100, 101, 0.5)
a2 <- rnorm(100, 5, 2)
a3 <- rnorm(100, 5, 0.5)
a4 <- rnorm(100, 0.1, 0.1)
a5 <- rnorm(100, 5, 0.1)
a6 <- rnorm(100, 100, 0.1)

sample_df_a <- data.frame(a,
                   a2,
                   a3,
                   a4,
                   a5,
                   a6)

b <- rnorm(100, 5, 0.5)
b2 <- rnorm(100, 5, 2)
b3 <- rnorm(100, 5, 0.5)
b4 <- rnorm(100, 5, 0.1)
b5 <- rnorm(100, 5, 0.1)
b6 <- rnorm(100, 5, 0.1)

sample_df_b <- data.frame(b,
                   b2,
                   b3,
                   b4,
                   b5,
                   b6)
                   
z_test(sample_df_a, sample_df_b)
z_test(sample_df_a, sample_df_b, plot = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
