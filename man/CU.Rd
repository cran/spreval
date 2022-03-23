\name{CU}
\alias{CU}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute CU for Sprinkler Irrigation Systems
%%  ~~function to do ... ~~
}
\description{Compute Christiansen Coefficient of Uniformity (CU or UC).
Coefficient is based upon the average deviation from the mean value and the mean value.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CU(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric array of catch can data. Array may include missing (NA) data.
NA values will be removed in function.
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
\deqn{CU=100(1.0-\sum X/(nm))}
where \eqn{X=|z-m|}; absolute deviation of catch \eqn{z} from average catch \eqn{m} based on \eqn{n} observations (catches).
}
\value{CU value in percentage.  Note that CU can take on a negative value if the
average deviation from the mean is greater than the mean.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Christiansen, J. E. 1942. Irrigation by sprinkling. California Agricultural Experiment Station Bulletin 670. Berkeley, Cal.: University of California.

Keller, J., and R. D. Bliesner. 2000. Sprinkler and Trickle Irrigation. Caldwell, N.J.: Blackburn Press.  ISBN 1-930665-19-9.
%% ~put references to the literature/web site here ~
}
\author{Garry Grabow \email{glgrabow@ncsu.edu}
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{DU}},\code{\link{DU.lh}}

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# data below are volumes caught in ml.  Catch cans must be of
# equal surface area at top when using volume data
x<-c(47,42,45,24,13,26,33,34,27,30,40,44,32,12,12)
xcu<-CU(x)
#round results
curnd<-round(xcu,2)
xcu
curnd

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
