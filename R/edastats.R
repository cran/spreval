#function to do exploratory data analysis of object array
#histogram, boxplot, density and quartile plots constructed

"eda.shape" = function(x,qq=TRUE,main=NULL,xlab,...)
{
  oldpar<-par(no.readonly = TRUE) #get current plot parameters
  on.exit(par(oldpar))
  par(mfrow = c(2, 2))
  #extract name of array as default label
  #ann=FALSE will suppress labels in boxplot
  nm<- deparse(substitute(x))
  # to suppress main, x, y label defaults pass ann=FALSE at call
  if(is.null(main))main=""# to prevent default main titles, explicitly provide null string
  lhist<-function(...,notch,axes,log,ylab,ylim,cex.label,frame.plot) hist(...)
  lboxplot<-function(...,axes,log,xlab,xlim,cex.label) boxplot(...)
  lplot<-function(...,notch,axes,ylab,cex.label,frame.plot) plot(...)
  lqqnorm<-function(...,notch,xlab,ylab,log,axes,xlim,ylim,cex.label) qqnorm(...)
  if(missing(xlab))
    lhist(x,main=main,xlab=nm,...)
  else
    lhist(x,main=main,xlab=xlab,...)
  if(missing(xlab))
    lboxplot(x, main=main,ylab=nm,plot = TRUE,border=1,
             staplelty = 1, staplewex = 1,
             staplehex = 1, outchar = TRUE, outpch = NA, outline = TRUE, outwex
             = 1,...)
  else
    lboxplot(x,main=main,ylab=xlab,plot = TRUE,border=1,
             staplelty = 1, staplewex = 1,
             staplehex = 1, outchar = TRUE, outpch = NA, outline = TRUE, outwex
             = 1,...)
  iqd <- summary(x)[5] - summary(x)[2] #interquartile range ... not used
  if(missing(xlab))
   lplot(density(x,na.rm = TRUE),main=main,xlab=nm,...)
  else
    lplot(density(x,na.rm = TRUE),main=main,xlab=xlab,...)
  if(qq){lqqnorm(x,main=main,...)}
  if(qq){qqline(x)}
  invisible()
}

#function to spit out summary stats, skewness and kurtosis
"eda.stats" = function(x)
{

  summary<-summary(x)
  skew<-timeDate::skewness(x)#point to timeDate function for skew and kurtosis
  kurt<-timeDate::kurtosis(x)
  return(list("summary"=summary,"skew"=skew,"kurt"=kurt))
}

#function to do quantiles for 0.001 to 0.999
#and return interquartile range
"quart" = function(x)
{
  q<-quantile(x, c(0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,
                0.8, 0.9, 0.95, 0.99, 0.999))
  d<-diff(quantile(x, c(.25, .75)))
  return(list("quantiles"=q,"inter"=d))
}
