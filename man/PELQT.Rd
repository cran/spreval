\name{PELQT}
\alias{PELQT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Potential application efficiency of low quarter for a traveling irrigation system.
%%  ~~function to do ... ~~
}
\description{This function determines PELQ as the average of the low quarter catch depths
divided by the average catch depth of applied water.  As such is is a measure of the potential of the
sprinkler irrigation system considering uniformity (low quarter) and any losses due to drift and evaporation (uses catch can depths overlapped at specified lane spacing).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PELQT(x,SI=TRUE,rate,ls,ts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric array of catch can depths, overlapped.}
  \item{SI}{logical; units SI (mm, m, lpm) or US Customary (in., ft, gpm).  SI (TRUE) is default.}
  \item{rate}{sprinkler discharge rate (lps or gpm).}
  \item{ls}{lane spacing (m or ft).}
  \item{ts}{travel speed (m/min or ft/min).}
 %%     ~~Describe \code{x} here~~
}

\details{catch can depths overlapped to specified lane spacing.
%%  ~~ If necessary, more details than the description above ~~
}
\value{PELQT (potential application efficiency of low quarter for traveling system, \%
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Evans, R.O., Barker J.C., Smith J.T., Sheffield R.E. 1997b.  Field calibration procedures for animal wastewater application equipment, hard hose and cable tow traveler irrigation system. NC Cooperative Extension Service publication  AG-553-2.  Raleigh, NC
\url{https://p2infohouse.org/ref/32/31084/ag-553-2.pdf}

Liu, Z., G.L. Grabow, R.L. Huffman, J. Osborne, and R.O. Evans. 2012.  Factors Affecting Uniformity of Irrigation-Type Manure Application Systems. Applied Eng. in Agric.  28(1):43-56.

Mirriam and Keller, 1978. Farm System Irrigation Evaluation: A Guide for Management.  PP 97-107.
Utah State University, Logan, Utah. \url{https://pdf.usaid.gov/pdf_docs/PNAAG745.pdf}

%% ~put references to the literature/web site here ~
}
\author{Garry Grabow \email{glgrabow@ncsu.edu}
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{PELQ}},\code{\link{travunif}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#see pp 41-43 of reference document
SI<-FALSE # use U.S. customary units
left<-c(0.94,0.80,0.59,0.61,0.50,0.42,0.33,0.07)
right<-c(0.73,0.81,0.92,0.64,0.50,0.27,0.20,0.13)
ls<-224;gs<-20
#first call travunif to return overlapped data given gage spacing and lane spacing
out<-travunif(ls,gs,left,right)
x<-out$o.depths
rate<-197 # 197 gpm sprinkler discharge rate (measured)
PELQT(x,SI=FALSE,rate,ls,1.5)#call PELQ for traveling systems

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
