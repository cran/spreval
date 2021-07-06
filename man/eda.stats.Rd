\name{eda.stats}
\alias{eda.stats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summary statistics of a numeric array.
%%  ~~function to do ... ~~
}
\description{Generates summary statistics of mean, median, skew, kurtosis, min, max and quartiles.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eda.stats(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric array}
%%     ~~Describe \code{x} here~~
}

\details{
requires timeDate library
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Named list with the following items:
\item{summary}{min, 1st quartile, median, mean, 3rd quartile, max}
\item{skew}{skewness}
\item{kurt}{kurtosis}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Garry Grabow
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{eda.shape}},
\code{\link[base]{summary}},\code{\link[timeDate]{skewness}},
\code{\link[timeDate]{kurtosis}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
require(timeDate)
# summary statistics for random normal data
# mean of 1. sd = 0.3
ndata<-rnorm(25,1,.3)
eda.stats(ndata)
#summary statistics for right-skewed data
#mean of 1, sd=1
rdata<-rexp(25,rate=1)
eda.stats(rdata)
}


