\name{sfplot}
\alias{sfplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create step function plot (of catch can depths).
%%  ~~function to do ... ~~
}
\description{Uses an `stats::ecdf` object to plot a step function plot
with cumulative frequency on x axis and input array (catch can depths) on y axis with y-axis reversed (min(y)= 0 at top of y-axis). Top of plot represents soil surface.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sfplot(x,target=NA,xlab=NULL,ylab=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric array}
  \item{target}{target, a constant, e.g., target depth of irrigation.}
  \item{xlab}{label for x axis, standard default provided.}
  \item{ylab}{label for y axis, e.g., depth applied. Default is  name of object x if not supplied.}
  \item{...}{further graphical parameters to pass to internal calls to \code{\link[graphics]{plot.default}}
%%     ~~Describe \code{x} here~~
}
}

\details{Plot includes step function lines unlike \code{\link{rotecdf}}. A minimum y of 0 is forced such that the plot can be interpreted as 0 being ground surface. If "target" is supplied, a horizontal line at the value of target is drawn. Points and area below the target line indicate water applied in excess of the target amount.
%%  ~~ If necessary, more details than the description above ~~
}
\value{Step function plot as empirical cumulative distribution function with x on y-axis and f(x), i.e., cum. prob., on x-axis.
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

\seealso{\code{\link{rotecdf}},\code{\link[stats]{stepfun}},\code{\link[stats]{ecdf}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
target<-3
# generate data with mean of target amount and high variability (non-uniformity)
x<-rnorm(25,3,1.75)
sfplot(x,target,ylab="depth applied (caught)")
# generate data with mean of target amount and low varibility (uniformity)
xx<-rnorm(25,3,0.5)
sfplot(xx,target)#no y axis label provided - defaults to array object name
#customize graphical parameters
sfplot(xx,target,main="Good uniformity",xlab="cum. probability",lwd=2, col="blue")
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
