\name{overlap}
\alias{overlap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Superimpose catch can data
%%  ~~function to do ... ~~
}
\description{Simulate overlap from adjacent laterals using data from one lateral.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
overlap(sl,sc,lcdata,rcdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sl}{lateral spacing.}
  \item{sc}{catch can spacing perpendicular to lateral.}
  \item{lcdata}{data from cans left of lateral; order is proximal to distal of lateral.}
  \item{rcdata}{data from cans right of lateral; order as in lcdata.}
%%     ~~Describe \code{x} here~~
}

\details{Superimposes and sums one row of catch can data as if adjacent lateral had same catch pattern.  Repeat function for
multiple rows of catch cans between laterals, to normally include all rows of cans between two sprinklers.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A named list with the following items:
  \item{sum.left}{numeric array of summed overlap catch can data between tested lateral and simulated lateral to the left.}
  \item{sum.right}{numeric array of summed overlap catch can data between tested lateral and simulated lateral to the right.}
}

\author{Garry Grabow \email{glgrabow@ncsu.edu}
%%  ~~who you are~~
}
\note{both items of list will have same summed values but in inversely ordered. Only can data receiving water need to be entered into lcdata and rcdata.  The function will automatically produce "phantom cans" to fill between adjacent laterals if not entered and set catch to 0.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
sl<-60 #lateral spacing. Units are feet
sc<-10 #catch cans spacing along single "row". Units are feet
#note that neither lcdata or rcdata have 6 cans. function will generate "0" cans.
lcdata<-c(0.28,0.24,0.21,0.10)
rcdata<-c(0.23,0.21,0.3)
#generate summed catch for one row at 60 foot lateral spacing
spacing.60<-overlap(sl,sc,lcdata,rcdata)
spacing.60
#now generate summed catch assuming a 50 foot lateral spacing
sl<-50
spacing.50<-overlap(sl,sc,lcdata,rcdata)
spacing.50
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
