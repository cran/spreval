#rotate ecdf object for plot with reversed axes, and inverted y axis
"rotecdf"=function(x,target=NA,xlab=NULL,ylab=NULL,...){
  #wrapper function lplot for plot.default". Disallow some additional (...)graphical parameters
  lplot<-function(...,title,title.col,asp,xlim,ylim,lwd) plot(...) # title and title.col are legend parameters
  labline<-function(...,title,title.col,asp,xlim,ylim) abline(...)
  ltext<-function(...,log, axes, frame.plot, panel.first, panel.last,lty,lwd,title,title.col,col) text(...)
  xecdf<-ecdf(x)#create ecdf object
  fx<-xecdf(x)
  if(is.null(ylab)){ytitle<-nm <-deparse(substitute(x))}else
  {ytitle=ylab}
  if(is.null(xlab)){xtitle<-"cum. frequency"}else
  {xtitle=xlab}
  lplot(fx,x,ylim=rev(c(0,max(x))),xlim=c(0,1),xlab=xtitle,ylab=ytitle,...)
  labline(h=target,...)
  if(!(is.na(target))){ltext(0.2,target,"target depth",adj=c(0,1),...)}
  }

"sfplot"=function(x,target=NA,xlab=NULL,ylab=NULL,...){
  #plot a step function with y (depth) axis inverted (ground at top)
  lplot<-function(...,title,title.col,asp,lwd) plot(...) # title and title.col are legend parameters
  labline<-function(...,title,title.col,asp,xlim,ylim) abline(...)
  ltext<-function(...,log, axes, frame.plot, panel.first, panel.last,lty,lwd,title,title.col,col) text(...)
  xecdf<-ecdf(x) #create ecdf object
  fx<-xecdf(x)
  df1<-as.data.frame(cbind(x,fx))
  ordered.df1<-df1[order(df1$x), ]
  xx<-ordered.df1$x
  yy<-ordered.df1$fx
  xx1<-c(min(xx),xx) # need 1 additional y point
  sf<-stepfun(yy,xx1)
  if(is.null(ylab)){ytitle<-nm <-deparse(substitute(x))}else
  {ytitle=ylab}
  if(is.null(xlab)){xtitle<-"cum. frequency"}else
  {xtitle=xlab}
  lplot(sf,ylim=rev(c(0,max(xx1))),xlim=c(0,1),xlab=xtitle,ylab=ytitle,...)
  labline(h=target,...)
  if(!(is.na(target))){ltext(0.2,target,"target depth",adj=c(0,1),...)}
}



