\name{rotecdf}
\alias{rotecdf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Swap axis of ecdf plot and reverse y axis
%%  ~~function to do ... ~~
}
\description{plots points generated from an `stats::ecdf` object with x and y axis swapped
(x is cumulative frequency) and with y axis reversed so that min(y)=0 is at top.  Plots points
without step function lines.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rotecdf(x,target=NA,xlab=NULL,ylab=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric array}
  \item{target}{target, a constant, e.g., target depth of irrigation.}
  \item{xlab}{label for x axis, standard default provided.}
  \item{ylab}{label for y axis, e.g., depth applied. Default is  name of object x if not supplied.}
  \item{...}{further graphical parameters to pass to internal calls to
  \code{\link[graphics]{plot.default}}
}
}

\details{
A minimum y of 0 is forced such that the plot can be interpreted at 0 being ground surface. If `target`
is supplied, a horizontal line at the value of target is drawn. Points and area below the target line indicate
water applied in excess of the target amount.
%%  ~~ If necessary, more details than the description above ~~
}
\value{rotated ecdf plot.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\note{Wrapper functions for internally called plot and sub-plot functions within `rotecfd` prevent warnings and errors for graphical parameters passed via `...` that are not recognized by subplolot functions, i.e., \code{\link[graphics]{abline}}, \code{\link[graphics]{text}}. Therefore not all graphical parameters passed via `...` will be realized in the generated plot.}

\author{Garry Grabow \email{glgrabow@ncsu.edu}
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{sfplot}}, \code{\link[stats]{ecdf}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
target<-3
# generate data with mean of target amount and high variability (non-uniformity)
x<-rnorm(25,3,1.75)
rotecdf(x,target,ylab="depth applied (caught)")
# generate data with mean of target amount and low varibility (uniformity)
xx<-rnorm(25,3,0.5)
#no y axis label provided - defaults to array object name.  Pass additional non-default
#graphical parameters
rotecdf(xx,target,lty=2,pch=2)
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
