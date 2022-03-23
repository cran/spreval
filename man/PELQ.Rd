\name{PELQ}
\alias{PELQ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Potential application efficiency of low quarter for a sprinkler irrigation system.
%%  ~~function to do ... ~~
}
\description{This function determines PELQ as the average of the low quarter catch rates
divided by the average catch rate of applied water.  As such is is a measure of the potential of the
sprinkler irrigation system considering uniformity (low quarter) and any losses due to drift and evaporation (uses catch can rates).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PELQ(x,SI=TRUE,rate,ss,sl,dur)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric array of catch can depths.}
  \item{SI}{logical; units SI (mm, m, lpm) or US Customary (in., ft, gpm).  SI (TRUE) is default.}
  \item{rate}{sprinkler discharge rate (lpm or gpm).}
  \item{ss}{sprinkler spacing (m or ft).}
  \item{sl}{lateral spacing (m or ft).}
  \item{dur}{duration of irrigation event, hr.}
%%     ~~Describe \code{x} here~~
}

\details{catch can depths converted to catch rates.
%%  ~~ If necessary, more details than the description above ~~
}
\value{PELQ (potential application efficiency of low quarter, \%
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Mirriam and Keller, 1978. Farm System Irrigation Evaluation: A Guide for Management.  PP 41-43.
Utah State University, Logan, Utah. \url{https://pdf.usaid.gov/pdf_docs/PNAAG745.pdf}

%% ~put references to the literature/web site here ~
}
\author{Garry Grabow \email{glgrabow@ncsu.edu}
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{PELQT}},\code{\link{AELQ}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#see pp 41-43 of reference document
SI<-FALSE # use U.S. customary units
x<-c(.23,.31,.24,.24,.28,.31,.27,.22,.26,.31,
.31,.25,.20,.22,.32)# catch can depths caught during test converted to in/hr
rate<-4.6 # 4.6 gpm sprinkler discharge rate (measured)
sl<-50; ss<-30 # 30 x 50 ft sprinkler x lateral spacing
dur<-1 # 1 hr test duration (dur. in min)
PELQ(x,SI=FALSE,rate,ss,sl,dur)

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
