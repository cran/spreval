plotss=function(cdata,sploc,con=TRUE,xlab=NULL,ylab=NULL,title=NULL,labelpoints=TRUE,spklab=NULL,imcol=FALSE,edastat=FALSE)
{
#cdata is n x 3 matrix of catch can data; 1st column x, 2nd column y can locations, 3rd column catch depths
#sploc is n x 2 matrix of 1st column x, second column y sprinkler location.  x=4, y = 4 for 4 sprinklers
#with cans in-between
# xlab and ylab for plan view plot of sprinklers and catch cans, provide units too if wish
#setup plotting space
#plot.new()
oldpar<-par(no.readonly = TRUE) #get current plot parameters
#on.exit(par(oldpar)) # at exit return to originating plot par on device
#size<-min(par("din")[1],par("din")[2])  # get minimum length of plot height and width by starting with device size
#size<-size-par("mai")[1]# now subtract space for labels location 1 "x-axis" has greatest default space
#par(pin=c(size,size))#set for square aspect ratio.  asp overides xlim and ylim so need this alternative
par(pty="s")#set square plotting area for 1:1 aspect ratio. Prevents issues with asp option.
sprinklerx<-sploc[ ,1];sprinklery<-sploc[ ,2]
cx<-cdata[ ,1];cy<-cdata[ ,2]
depth<-cdata[ ,3]
densigram<-interp::interp(cx,cy,depth)
col<-gray.colors(12, start = 0.9, end = 0.3, gamma = 2.2, alpha = NULL, rev = FALSE)#default image color is b/w
if(imcol){col = hcl.colors(12, "YlOrRd", rev = TRUE)} #color image which is normal default for image function
# set range of plot to either maximum space of sprinklers (external to cans) or to catch can range, e.g., 1 lateral
# add a border for room for can and sprinkler labels
border.x<-par("pin")[1]/40*(max(cy)-min(cy))
border.y<-par("pin")[2]/40*(max(cy)-min(cy))
xmin<-min(min(sprinklerx),min(cx))-border.x;xmax<-max(max(sprinklerx),max(cx))+border.x
ymin<-min(min(sprinklery),min(cy))-border.y;ymax<-max(max(sprinklery),max(cy))+border.y
image(densigram, col=col,xlim=c(xmin,xmax),ylim=c(ymin,ymax),
      xaxs="i",yaxs="i")
points(sprinklerx,sprinklery,pch=16)
title(main=title,xlab=xlab,ylab=ylab)
#image.legend(densigram,30,80,size=c(1.5,.10),hor=TRUE,lab=c(5,0,7),cex=0.75,nint=25)
if(con){
contour(densigram, add=TRUE, plotit=TRUE,labex=0.6)
}
points(cx,cy,pch=1,col=1)#plot rain gages -plan view
if(labelpoints){
  offset.x<-par("pin")[1]/50
  offset.y<-par("pin")[2]/50*(max(cy)-min(cy))
  label.locx<-cx-offset.x # start left of center with label
  label.locy<-cy+offset.y #start above point
  gage.labels<-depth
  text(label.locx,label.locy,gage.labels,cex=0.7,col=1)
}
#label sprinklers if labels are provided (!null)
if(!is.null(spklab)){
  text(sprinklerx-offset.x, sprinklery+offset.y,spklab,cex=0.7,col=1)
}

if(edastat){
eda.shape(depth,title=title)
eda.stats(depth)
}

}

