#rotate ecdf object for plot with reversed axes, and inverted y axis
"rotecdf"=function(x,target=NA,ylab=NULL){
  xecdf<-ecdf(x)#create ecdf object
  fx<-xecdf(x)
  if(is.null(ylab)){ytitle<-nm <-deparse(substitute(x))}else
  {ytitle=ylab}
  plot(fx,x,ylim=rev(c(0,max(x))),xlim=c(0,1),xlab="cum. frequency.",ylab=ytitle)
  abline(h=target,col=2)
  if(target){text(0.2,target,"target depth",srt=0,adj=c(0,1))}
  }

"sfplot"=function(x,target=NA,ylab=NULL){
  #plot a step function with y (depth) axis inverted (ground at top)
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
  plot(sf,ylim=rev(c(0,max(xx1))),xlim=c(0,1),xlab="cum. frequency",ylab=ytitle, main=NULL)
  abline(h=target,col=2)
  if(target){text(0.2,target,"target depth",srt=0,adj=c(0,1))}
}



