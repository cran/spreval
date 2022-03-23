\name{quart}
\alias{quart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute several levels of quantiles and interquartile range
%%  ~~function to do ... ~~
}
\description{generates quantiles at 0.1, 1,5,10-90, 95, 99, 99,9 percentiles.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
quart(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric array
%%     ~~Describe \code{x} here~~
}
}
\details{Uses `stats::quantile`
%%  ~~ If necessary, more details than the description above ~~
}
\value{
named list with  following items:
\item{q}{quantiles at 0.1, 1,5,10-90, 95, 99, 99,9 percentiles}
\item{d}{interquartile range - i.e., 75th quantile minus 25th quantile}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Garry Grabow \email{glgrabow@ncsu.edu}

%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link[stats]{quantile}},\code{\link{eda.stats}}

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# quantiles for random normal data
x<-rnorm(25,10,1)
xn<-quart(x)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
