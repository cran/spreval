"travunif"=function(ls,cs,lcdata,rcdata,site=NULL,plot=TRUE)
{
  # Set lcdata or rcdata to maximum of either.  Need to fill "empty cans of shorter array
  # setting shorter array to max array length will fill shorter arrays with NA for missing values
 no.gages<-max(length(lcdata),length(rcdata))*2
 length(lcdata)<-no.gages/2;length(rcdata)<-no.gages/2
  lcdata[is.na(lcdata)]<-0 #set NAs to 0 for later summing of overlap
  rcdata[is.na(rcdata)]<-0
  depths<-c(lcdata,rcdata)#concatenate left and right of pull catch can data.  Order is increasing from hose outward,
  #e.g., can 1 is closest to hose (middle of pull) for both left and right of hose.
  if(plot){
    oldpar<-par(no.readonly = TRUE) #get current plot parameters
    on.exit(par(oldpar)) # at exit return to originating plot par on device
    leg.colors<-c(1,3,1);leg.pch<-c(1,1,NA);leg.cex<-0.7;leg.lines<-c(0,0,2)
    leg.names<-c("catch cans", "ovelap cans","lane spacing")
  lane.space<-ls
  gage.space<-cs
  #plot locations of gages and show overlap lanes
  #create array for overlapped pulls
  y.left.pull<-rep(10+10,no.gages) # yposition of pull to left
  y.right.pull<-rep(10+10,no.gages)# yposition of pull to right
  #plot locations - plot base locations and full range only once
    far.gage<-(no.gages/2)*gage.space-gage.space/2
    gage.loc.left<-seq(-gage.space/2, by=-gage.space,-far.gage)
    gage.loc.right<-seq(gage.space/2, by=gage.space,far.gage)
    gage.loc<-append(gage.loc.left,gage.loc.right)#append will work with R
    xmin<--far.gage
    xmax<-far.gage
    x<-c(xmin,xmax)#range of overlapped pulls
    y<-c(0,100)#arbitrary y range to display pulls in offsets
    plot(x,y,type="n",yaxt="n",xlab = "feet",ylab="")
    y.pull<-rep(10,no.gages)#create y position of original gages
    points(gage.loc,y.pull,type="p",pch=1, col=1)
    gage.labels<-depths
    text(gage.loc,y.pull+3,gage.labels,cex=0.5,col=1)
    segments(-lane.space/2,0,-lane.space/2,10+10,lty=2)#R requires explicit x1,y1,x2,y2
    segments(lane.space/2,0,lane.space/2,10+10,lty=2)#R requires explicit x1,y1,x2,y2
    segments(0,0,0,10+10,lty=1,lwd=2,col=1)# hose in center of plot
  #now plot adjacent left and right pulls with catch can locations given lane spacing, and catch can depths
  #print(i);print(j);print(left.pull);print(right.pull)
  left.pull<-gage.loc-lane.space
  right.pull<-gage.loc+lane.space
  points(left.pull,y.left.pull,type="p",pch=1, col=3)
  points(right.pull,y.right.pull,type="p",pch=1, col=3)
  text(left.pull,y.left.pull+3,gage.labels,cex=0.5,col=1)
  text(right.pull,y.right.pull+3,gage.labels,cex=0.5,col=1)
 # segments(-lane.space/2,10,-lane.space/2,10+10,lty=2)#R requires explicit x1,y1,x2,y2
 # segments(lane.space/2,10,lane.space/2,10+10,lty=2)#R requires explicit x1,y1,x2,y2
  if(!is.null(site)){text(0,100,site)} #plot site on plan view plot if provided
  leg.x<-far.gage/2
  legend("topright",leg.names,col=leg.colors,lty=leg.lines,pch=leg.pch,cex=leg.cex)
  }
  ###################################################################################
  # now identify overlap and gages for superpositioning and compute CU,DU
  outside.lane<-gage.loc[gage.loc[ ]>lane.space/2]#gage locations outside of effective width on one side
  no.out<-length(outside.lane)#number of gages outside effective width on one side
  #correct catch depths by superpositioning gages outside of effective width
  corr.depths<-depths#fill array with raw data to start
  start.left<-no.gages/2-no.out;start.right<-no.gages-no.out
  #re-establish left and right pull locations. gage.loc is constant, lane.space variable
  left.pull<-gage.loc-lane.space
  right.pull<-gage.loc+lane.space
  #interpolate depths of overlapped gages
  depth.over.left<-approx(left.pull,depths,gage.loc)
  depth.over.right<-approx(right.pull,depths,gage.loc)
  k=1
  #if lane spacing boundary falls on gage location - need to adjust counter
  l=0
  if(lane.space/2==gage.loc[start.right]){l=1}
  #Enter outside travel lane loop
  for (kk in 1:(length(outside.lane)+l)){
    #print(i);print(j);print(k);print(l)
    #check to see if beyond interpolable bounds, if so use last real overlap gage (not interp)
    #print(depth.over.left$y[no.gages/2-no.out-k+1])

    #doesn't return TRUE when =="NA" so use test with is.na
    if(no.gages/2-no.out-k+1>0){
      if(is.na(depth.over.left$y[no.gages/2-no.out-k+1]))
      {add.left<-depths[no.gages]}
      else add.left<-depth.over.left$y[no.gages/2-no.out-k+1]
    }else add.left<-depths[no.gages]#added in version 10 with xtra overlaps
    #print(add.left)
    corr.depths[start.left-k+1]=depths[start.left-k+1]+add.left #left side
    if(is.na(depth.over.right$y[no.gages-no.out-k+1]))
    {add.right<-depths[no.gages/2]}
      else
      add.right<-depth.over.right$y[no.gages-no.out-k+1]
    #print(add.right)
    corr.depths[start.right-k+1]=depths[start.right-k+1]+add.right # right side
    #print(corr.depths[start.left-k+1]);print(depths[start.left-k+1])
    #print(add.left)
    #print(corr.depths[start.right-k+1]);print(depths[start.right-k+1]);print(add.right)
    k=k+1
  #  print("k=");print(k);print("no.out");print(no.out);print("per over");print(per.over[k])
  }#end corrected depth loop
  #print(i);print(j);print(corr.depths)
  #use only gages inside effective width
  use.left<-corr.depths[1:start.left]#usable left gages
  use.left.loc<-gage.loc[1:start.left]
  beg.right<-no.gages/2+1#in beg:end array beg and end of array range must be defined variable
  end.right<-no.gages-no.out
  use.right<-corr.depths[beg.right:end.right]#usable right gages
  use.right.loc<-gage.loc[beg.right:end.right]
  use.depths<-append(use.left,use.right)
  #print(use.depths)
  use.locations<-append(use.left.loc,use.right.loc)
  #calculate CU
  CU.curr<-CU(use.depths)
  #calculate DUlh
  DUlh.curr<-DU.lh(use.depths)
  #calculate DU
  DU.curr<-DU(use.depths)

#if plot requested -  plot cross-section of depths
  if(plot){

    leg.colors<-c(1,1);leg.pch<-c(1,NA);leg.cex<-0.7;leg.lines<-c(0,1)
    leg.names<-c("raw catch depth", "ovelap depth")
    #scale up y axis for superimposed depths
    #predict scaling by using DUlh
    stretch.depths<-depths/(DUlh.curr/100)
    ######################################################################
    #determine scaling factors for plotting labels on plot
    ######################################################################
    maxy<-max(stretch.depths);maxx<-max(gage.loc);range.x<-maxx-min(gage.loc);middle.x<-mean(gage.loc)
    raw.loc.depths<-data.frame(cbind(gage.loc,depths,stretch.depths))#make dataframe for subsequent sort
    col.names<-dimnames(raw.loc.depths)[[2]]# per.over title doesn't want to print so force it in
    col.names[2]<-c("depths");col.names[3]<-c("stretch.depths")
    dimnames(raw.loc.depths)[[2]]<-col.names #force in column names after cbind
    #print(raw.loc.depths)
    #plotit.raw<-sort.col(raw.loc.depths,columns.to.sort="@ALL",columns.to.sort.by=1,ascending=TRUE)
    plotit.raw<-with(raw.loc.depths,raw.loc.depths[order(raw.loc.depths$gage.loc), ])#R equivalent sort
    # set plot and plot raw depths

    plot(xlim<-c(min(plotit.raw[ ,1]),max(plotit.raw[ ,1])),ylim<-c(0,max(use.depths*1.1)),type="n",xlab="distance from gun",ylab="depth") #set plot bounds
    points(plotit.raw[ ,1],plotit.raw[ ,2])# plot raw catch can depths

    plotit.use<-data.frame(cbind(use.locations,use.depths))
    #print(plotit.use)
    plotit.use<-with(plotit.use,plotit.use[order(plotit.use$use.locations), ])#R sort to draw lines by location
    lines(plotit.use[ ,1],plotit.use[ ,2],col=1,lty=1)

    leg.x<-mean(c(maxx,middle.x)*1.3)
    legend("topright",leg.names,col=leg.colors,lty=leg.lines,pch=leg.pch,cex=leg.cex)

  }
  uniList<-list("o.depths"=use.depths,"CU"=CU.curr,"DUlh"=DUlh.curr,"DU"=DU.curr)
  return(uniList)
}
