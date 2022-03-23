\name{DU}
\alias{DU}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{DU of sprinkler irrigation system
%%  ~~function to do ... ~~
}
\description{Computes DU (distribution uniformity of low quarter) for sprinkler systems using catch can data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DU(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric array of catch can data. Array may include missing (NA) data.
NA values will be removed in function.
%%     ~~Describe \code{x} here~~
}
}
\details{
mean of lowest quarter of observations divided by overall mean X 100
}
\value{Distribution Uniformity (of low quarter) in percentage}

\references{
Keller, J., and R. D. Bliesner. 2000. Sprinkler and Trickle Irrigation. Caldwell, N.J.: Blackburn Press. ISBN 1-930665-19-9.
%% ~put references to the literature/web site here ~
}
\author{Garry Grabow \email{glgrabow@ncsu.edu}
%%  ~~who you are~~
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{CU}},\code{\link{DU.lh}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# data below are volumes caught in ml.  Catch cans must be of
# equal surface area at top when using volume data
x<-c(47,42,45,24,13,26,33,34,27,30,40,44,32,12,12)
xdu<-DU(x)
#round results
durnd<-round(xdu,2)
xdu
durnd

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
