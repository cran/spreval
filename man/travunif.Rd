\name{travunif}
\alias{travunif}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute CU and DU of traveling gun irrigation systems
%%  ~~function to do ... ~~
}
\description{This function computes 3 values of sprinkler irrigation distribution uniformity -
CU (Christiansen's coefficient of uniformity), DU (dist. unif. of low quarter), DU.lh (dist. unif. of low half)
for traveling gun irrigation systems.  Data from a single transect of catch cans from one hard hose pull is used, and data is overlapped to account for additional contribution from adjacent traveler pulls.  Overlapped catch depths are also returned.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
travunif(ls,cs,lcdata,rcdata,site=NULL,plot=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ls}{lane spacing.}
  \item{cs}{catch can spacing.}
  \item{lcdata}{catch can data from cans left of hose - order is from closet to hose outward.}
  \item{rcdata}{catch can data from cans right of hose - order is from closet to hose outward.}
  \item{site}{optional character label for location (site) of evaluation  - used in plot titles.}
  \item{plot}{logical argument to invoke plotting, default is TRUE.}
%%     ~~Describe \code{x} here~~
}

\details{
%%  ~~ If necessary, more details than the description above ~~
It is not necessary that lcdata and rcdata be of equal length.  This might occur if windy conditions prevail and the wetted width of one side is greater than another, and data is not entered for empty cans.  The function will automatically assign 0 depths to the "missing" catch cans if not explicitly done. While using volume instead of depth data will result in the same uniformity values, depth is more intuitive and will be required if the `o.depths` output will be used to compute `PELQT`.
}
\value{A named list with the following items:
\item{o.depths}{collected depths as overlapped within travel lane}
\item{CU}{CU from catch can data with overlap from adjacent lanes}
\item{DUlh}{DU low half (using  function \code{\link{DU.lh}}) from catch can data with overlap from adjacent lanes}
\item{DU}{DU from catch can data with overlap from adjacent lanes}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Evans, R.O., Barker J.C., Smith J.T., Sheffield R.E. 1997b.  Field calibration procedures for animal wastewater application equipment, hard hose and cable tow traveler irrigation system. NC Cooperative Extension Service publication  AG-553-2.  Raleigh, NC

Liu, Z., G.L. Grabow, R.L. Huffman, J. Osborne, and R.O. Evans. 2012.  Factors Affecting Uniformity of Irrigation-Type Manure Application Systems. Applied Eng. in Agric.  28(1):43-56.
%% ~put references to the literature/web site here ~
}
\author{Garry Grabow \email{glgrabow@ncsu.edu}
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{PELQT}}}
%% ~~objects to See Also as \code{\link{help}}, ~~~

\examples{
# units can be SI or US Customary, e.g.
# SI - mm for catch can data and m for lane and catch
# can spacing
# U.S. cust. -  in. for catch can data and ft for lane
# and catch can spacing
left<-c(0.17,0.22,0.18,0.21,0.13,0.05,0.02,0) # units are in.
right<-c(0.16,0.2,0.21,0.2,0.21,0.13,0.06,0.02)
ls<-165;gs<-16 # units are ft
travunif(ls,gs,left,right)
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
