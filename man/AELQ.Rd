\name{AELQ}
\alias{AELQ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Application efficiency of the low quarter, (AELQ) for a sprinkler irrigation system
%%  ~~function to do ... ~~
}
\description{AELQ is a measure of both operation (duration of irrigation) and inherent sprinkler system uniformity.  AELQ is based upon soil moisture depletion (SMD) at time of irrigation, the system application rate, and irrigation duration. If the low quarter caught depth is less than or equal to the SMD, AELQ is calculated as the low quarter average caught depth (from catch cans) divided by the average applied depth (sys. app. rate x duration).  If the infiltrated (caught) low quarter depth is greater than SMD, AELQ is calculated as SMD/avg. applied depth in \%.  Whenever the irrigation (caught or infiltrated) in the low quarter exactly matches the SMD in the low quarter AELQ=PELQ.  If the duration of irrigation is such that the infiltrated depth exceeds the SMD, then AELQ will be less than PELQ.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AELQ(x, rate,ss,sl,dur,smd,SI=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{array of catch can caught rates - not depths (mm/hr or in/hr).}
  \item{rate}{sprinkler discharge rate (lpm or gpm).}
  \item{ss}{sprinkler spacing (m or ft).}
  \item{sl}{lateral spacing (m or ft).}
  \item{dur}{duration of irrigation event, hr}
  \item{smd}{soil moisture depletion at begin irrigation, mm or in.}
  \item{SI}{logical; units SI (mm, m, lpm) or US Customary (in., ft, gpm).  SI (TRUE) is default.}
%%     ~~Describe \code{x} here~~
x will be used to determine mode of AELQ computation by determining if the low quarter catch rate (and depth) is less than or greater than the SMD.
}
\value{AELQ (application efficiency of low quarter, \%)
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Mirriam and Keller, 1978. Farm System Irrigation Evaluation: A Guide for Management.  PP 43,44.
Utah State University, Logan, Utah. \url{https://pdf.usaid.gov/pdf_docs/PNAAG745.pdf}

%% ~put references to the literature/web site here ~
}
\author{Garry Grabow \email{glgrabow@ncsu.edu}
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{PELQ}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#see pp 41-44 of reference document
x<-c(.23,.31,.24,.24,.28,.31,.27,.22,.26,.31,
.31,.25,.20,.22,.32)# catch can depths caught during test converted to in/hr
SI<-FALSE # use U.S. customary units
smd<-4.4 #soil moisture depletion of 4.4 inches
rate<-4.6 # 4.6 gpm sprinkler discharge
sl<-50; ss<-30 # 30 x 50 ft sprinkler x lateral spacing
dur<-23.5 #23.5 hr duration (24 hour set)
AELQ(x,rate,ss,sl,dur,smd,SI)
# now for a lower applied depth for alternate mode of AELQ
#computation
dur<-11.5 #change irrigation duration to a 12 hour set (11.5 hrs)
AELQ(x,rate,ss,sl,dur,smd,SI)
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
