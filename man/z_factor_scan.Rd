\name{z_factor_scan}
\alias{z_factor_scan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
z_factor_scan(data, treatments, cutoff = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{treatments}{
%%     ~~Describe \code{treatments} here~~
}
  \item{cutoff}{
%%     ~~Describe \code{cutoff} here~~
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, treatments, cutoff = 0.5) 
{
    z_factor <- function(positive, negative) {
        positive <- as.vector(positive)
        negative <- as.vector(negative)
        mu_p <- mean(positive, na.rm = TRUE)
        mu_n <- mean(negative, na.rm = TRUE)
        if (mu_n > mu_p) {
            mu_p <- mean(negative, na.rm = TRUE)
            mu_n <- mean(positive, na.rm = TRUE)
        }
        sigma_p <- sd(positive)
        sigma_n <- sd(negative)
        z_prime <- 1 - ((3 * (sigma_p + sigma_n))/(mu_p - mu_n))
        return(z_prime)
    }
    header_in <- which(colnames(data) == "header")
    nums <- (1:dim(data)[2])[-header_in]
    z_values <- c()
    z_names <- c()
    for (number in nums) {
        z_values[number] <- (as.vector(z_factor(subset(data[, 
            number], data$header == treatments[1]), subset(data[, 
            number], data$header == treatments[2]))))
        z_names[number] <- names(data)[number]
    }
    z_factors <- data.frame(z_names, z_values)
    z_factors_good <- subset(z_factors, z_factors$z_values > 
        cutoff)
    names(z_factors_good)[c(1, 2)] <- c("Feature", "Z_factor")
    return(z_factors_good[with(z_factors_good, order(-Z_factor)), 
        ])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
