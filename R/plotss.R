plotss=function(cdata,sploc,con=TRUE,xlab=NULL,ylab=NULL,xlim=NULL,ylim=NULL,main=NULL,labelpoints=TRUE,pos=1,spklab=NULL,
pch=16,cex=0.8,edastat=FALSE,...)
{
#cdata is n x 3 matrix of catch can data; 1st column x, 2nd column y can locations, 3rd column catch depths
#sploc is n x 2 matrix of 1st column x, second column y sprinkler location.  x=4, y = 4 for 4 sprinklers
#with cans in-between
# xlab and ylab for plan view plot of sprinklers and catch cans, provide units too if wish
# added "..." to pass graphcial paramters.  Removed imcol argument as color can now be passed as customized.
# asp can be passed in plotss extra arguments ,..., e.g., asp=1 for true x/y spatial rendering.
#user input of pch other than default of 16 changes only sprinkler location symbols, catch cans are fixed at pch=1.
#col as other graphical parameter imput only applies to image plot. See wrapper functions for exclusions.
oldpar<-par(no.readonly = TRUE) #get current plot parameters
on.exit(par(oldpar)) # at exit return to originating plot par on device. Should not need this as no par() set.
#user coordinates will come back as old coordinates not current plot so adding elements like arcs and radial
#lines for sprinkler pattern extents is problematic.  Would need to return current plot user coordinates
#par$usr
#write wrappers for plot and plot related functions
limage<-function(...,xaxs,yaxs,axes,log,labcex,pos) image(...) # can pass asp=1 for true x/y ratio rendering
lcontour<-function(...,xlim,ylim,col,pos) contour(...) # can pass 'labcex' in contour for line label (not 'cex.lab')
lpoints<-function(...,log, axes, frame.plot, panel.first, panel.last,labcex,pos,lty,col) points(...)
ltext<-function(...,log, axes, frame.plot, panel.first, panel.last,labcex,col,lty) text(...)
sprinklerx<-sploc[ ,1];sprinklery<-sploc[ ,2]
cx<-cdata[ ,1];cy<-cdata[ ,2]
nmx<- deparse(substitute(cx));nmy<- deparse(substitute(y)) # get default x and y array names for default xlab, ylab
depth<-cdata[ ,3]
densigram<-interp::interp(cx,cy,depth)
# set range of plot to either maximum space of sprinklers (external to cans) or to catch can range, e.g., 1 lateral
# add a border for room for can and sprinkler labels
xpin<-par("pin")[1];ypin<-par("pin")[2]; as.ratio<-xpin/ypin #fetch aspect ratios in pin
border.x<-0.06*(max(cy)-min(cy)) #just use 5 or so %
border.y<-0.06*(max(cy)-min(cy)) #ditto
xmin<-min(min(sprinklerx),min(cx))-border.x;xmax<-max(max(sprinklerx),max(cx))+border.x
ymin<-min(min(sprinklery),min(cy))-2*border.y;ymax<-max(max(sprinklery),max(cy))+border.y
#print(ymin);print(ymax);print(border.y)
#set asp=1 for image.  added points will follow true aspect ratio
if(is.null(xlim)) {xlim=c(xmin,xmax)}; if(is.null(ylim)) {ylim=c(ymin,ymax)}
#print(xlim)
limage(densigram,xlim=xlim,ylim=ylim,
      xaxs="i",yaxs="i",...)
plotss.usr<-par("usr") # get user coordinates of current plot to use if later desired
# to add things like arcs and radial lines for sprinkler wetted radius. Par will still
# be reset but plotss.usr can be called up prior to adding low level plot items.
lpoints(sprinklerx,sprinklery,pch=pch,cex=cex,...)
title(main=main,xlab=xlab,ylab=ylab)

if(con){
lcontour(densigram, add=TRUE, plotit=TRUE,...)
}
lpoints(cx,cy,pch=1,cex=cex,...)#plot rain gages -plan view
if(labelpoints){
   if(is.null(pos)) pos=1
   gage.labels<-depth
   ltext(cx,cy,gage.labels,pos=pos,cex=cex,...)
}
#label sprinklers if labels are provided (!null).  Bold italic (font=4)
#to distinguish from can labels.
if(!is.null(spklab)){
  ltext(sprinklerx, sprinklery,spklab,pos=pos,cex=cex,font=4,...)
}

if(edastat){
eda.shape(depth,main=main)
eda.stats(depth)
}
return(plotss.usr) #return user coordinates for possible use with low level additions
}

