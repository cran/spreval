\name{eda.shape}
\alias{eda.shape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate distribution plots of an array.
%%  ~~function to do ... ~~
}
\description{Generate histogram, boxplot, density, and quartile plots.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eda.shape(x,title=NULL,qq=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric array}
  \item{title}{character; optional title for plots, default is NULL (no title).}
  \item{qq}{logical; for plotting quartile plot, default=TRUE}
%%     ~~Describe \code{x} here~~
}

\details{This function is modified from the SPlus version of the same name, see references.
%%  ~~ If necessary, more details than the description above ~~
}
\value{Four plots on one page [mfrow = c(2, 2)]- histogram, boxplot, density, and quartile.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{TIBCO Spotfire S+ 8.2 2010. Guide to Statistics, vol. 1, p.124ff.
%% ~put references to the literature/web site here ~
}
\author{Garry Grabow \email{glgrabow@ncsu.edu
}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{eda.stats}},\code{\link[graphics]{hist}},
\code{\link[graphics]{boxplot}},\code{\link[stats]{density}},
\code{\link[stats]{qqnorm}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#generate and plot univariate normally distributed data
require(graphics)
x<-rnorm(25,1,0.25)
eda.shape(x,title="my normal data")
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
