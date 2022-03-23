\name{adper}
\alias{adper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute percentiles of area receiving less than and greater than target depth
%%  ~~function to do ... ~~
}
\description{
This returns measures of the percentage (fraction) of areas receiving
less or more application than the target depth, assuming catch cans
represent approximately equal areas.  It also a measure of adequacy
and efficiency determined from areas of a density curve receiving amounts equal to or
more of the target (adequacy) and less than or equal to target (efficiency).

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
adper(x,target,plot=TRUE)
}
%%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{array of catch can depths.}
  \item{target}{target depth meant to be applied.}
  \item{plot}{logical; plot density and ecdf plots of depths and target line.}
 }

%%     ~~Describe \code{x} here~~

\details{results are given as determined by both a density function and empirical cumulative
distribution function (ecdf). If target is equal to soil moisture depletion, then 1- efficiency is
the percentile of area that loses water to deep percolation (not held in root zone). Adequacy and efficiency (eff) percentiles should add to approximately 1.0. Note that definitions of adequacy and efficiency here based only on percentiles is not a standard definition as deviation from target depth is not considered.  See \code{\link{eff} for a more traditional approach.}
}
%%  ~~ If necessary, more details than the description above ~~

\value{named list, including;
 \item{adequacy.density}{percentile receiving >= target amount determined from density curve}
 \item{eff.density}{percentile receiving <=target amount as determined from density curve}
 \item{adequacy.ecdf}{same as adequacy.density but as determined from \code{\link[stats]{ecdf}} function}
 \item{eff.ecdf}{same as eff.density but as determined from \code{\link[stats]{ecdf}} function}
 }
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

\author{Garry Grabow \email{glgrabow@ncsu.edu}}
%%  ~~who you are~~

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{eff}},
\code{\link[stats]{density}},\code{\link[stats]{ecdf}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
#data from same exercise as PELQ example
x<-c(.23,.31,.24,.24,.28,.31,.27,.22,.26,.31,
.31,.25,.20,.22,.32)# catch can depths in inches caught during 1.0 hr. test
x<-x*23.5 # extend to 23.5 hour irrigation
target<-4.4 # replace soil water deficit of 4.4 in.
adper(x,target)
#change target to average catch
target2<-mean(x)
adper(x,target2)

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
