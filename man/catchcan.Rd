\name{catchcan}
\alias{catchcan}
\docType{data}
\title{catch can data
%%   ~~ data name/kind ... ~~
}
\description{Three catch can data sets, one each for lateral, hose pull, and solid set sprinkler systems.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("catchcan")
}
\format{A named list of 4 catch can data sets; lateral, traveler, solid.set and landscape.
    \describe{
    \item{\code{catchcan$lateral}}{6x7 matrix of catch can data, units are in./hr. Grid spacing of 10 ft x 10 ft}
    \item{\code{ln}}{a numeric vector of catch can data - nth can to left of lateral}
    \item{\code{rn}}{a numeric vector of catch can data - nth can to right of lateral}

    \item{\code{catachcan$traveler}}{16X2 matrix of catch can data.  Effective (lane) spacing is 224 ft in example.}
    \item{\code{station}}{distance (ft) of catch can relative to hose (0), neg. is left of hose in plan view}
    \item{\code{depth}}{collected depth, in.}

    \item{\code{catchcan$solid.set}}{4x4 matrix of catch can data, units are in. Can grid spacing of 20 ft x 20 ft inside of 4 operating sprinklers on 80x80 ft sprinkler x lateral spacing.}
    \item{\code{catchcan$landscape}}{9X7 matrix of catch can data, units are in ml.  Can grid spacing of 9 X 9 ft.}

     }
     }

\source{(lateral)Form II-1, item 10, p.29.
Utah State University, Logan, Utah. \url{https://pdf.usaid.gov/pdf_docs/PNAAG745.pdf}
%%  ~~ reference to a publication or URL from which the data were obtained ~~

(traveler)Table 1, pg. 8.
Evans, R.O., Barker J.C., Smith J.T., Sheffield R.E. 1997b.  Field calibration procedures for animal wastewater application equipment, hard hose and cable tow traveler irrigation system. NC Cooperative Extension Service publication  AG-553-2.  Raleigh, NC.

(solid set)Work Sheet 1. p. 13.
Evans, R.O., Barker J.C., Smith J.T., Sheffield R.E. 1997a.  Field calibration procedures for animal wastewater application equipment, stationary sprinkler irrigation system. NC Cooperative Extension Service publication AG 553-1.  Raleigh, NC.
}
\references{Mirriam and Keller, 1978. Farm System Irrigation Evaluation: A Guide for Management.
Form II-1, item 10, p.29.
Utah State University, Logan, Utah. \url{https://pdf.usaid.gov/pdf_docs/PNAAG745.pdf}

Evans, R.O., Barker J.C., Smith J.T., Sheffield R.E. 1997b.  Field calibration procedures for animal wastewater application equipment, hard hose and cable tow traveler irrigation system. NC Cooperative Extension Service publication  AG-553-2.  Raleigh, NC.
\url{https://p2infohouse.org/ref/32/31084/ag-553-2.pdf}

Evans, R.O., Barker J.C., Smith J.T., Sheffield R.E. 1997a.  Field calibration procedures for animal wastewater application equipment, stationary sprinkler irrigation system. NC Cooperative Extension Service publication AG 553-1.  Raleigh, NC.
\url{https://irrigation.wordpress.ncsu.edu/files/2017/01/ag-553-1-stationary-sprinkler.pdf}
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
library(fields) # for easy grid construction
data(catchcan)
use.data<-catchcan$lateral #matrix can be viewed as plan view of catch data
#x,y matrix 10 ft x 10 ft catch can spacing
x<-seq(-35,25,10) # x=0 is lateral position
y<-seq(55,5,-10) #plan view top to bottom
grd<-list(x,y) # prepare list for make.surface function [fields]
grid<-make.surface.grid(grd)
plot(grid)
labels<-matrix(t(use.data),ncol=1)#transpose matrix and stack rows into 1 column
text(grid[ ,1],grid[ ,2],labels,cex=0.8,pos=1) # plot catch data at collection point

## or plot using function plotss.  Shows test data from 1 lateral with no overlap.
cdata<-cbind(grid[ ,1],grid[ ,2],labels) #construct required catch can data matrix
sp.x<-rep(0,3);sp.y<-seq(60,0,-30)# sprinkler spacing (y) = 30 ft, plot top to bottom
sploc<-cbind(sp.x,sp.y) #construct required sprinkler location matrix
plotss(cdata,sploc)
%% maybe str(catchcan) ; plot(catchcan) ...
}
\keyword{datasets}
