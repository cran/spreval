\name{DU.lh}
\alias{DU.lh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute distribution uniformity of lower half
%%  ~~function to do ... ~~
}
\description{Computes uniformity of lower half of a solid set sprinkler system
vs. lower quarter as does DU.  Note that this computation uses the same method as low quarter, except it uses the observations of the low half rather than low quarter.  It does not use an empirical equation that converts DU (low quarter) to DUlh (lower half) as some have proposed.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DU.lh(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric array of catch depths or volumes.
%%     ~~Describe \code{x} here~~
}
}
\details{
mean of lower half of ranked data divided by overall mean x 100. %%  ~~ If necessary, more details than the description above ~~
}
\value{DU of lower half value in percentage.
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

\seealso{\code{\link{CU}},\code{\link{DU}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# data below are volumes caught in ml.  Catch cans must be of
# equal surface area at top when using volume data
x<-c(47,42,45,24,13,26,33,34,27,30,40,44,32,12,12)
xdulh<-DU.lh(x)
#round results
dulhrnd<-round(xdulh,2)
xdulh
dulhrnd

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
