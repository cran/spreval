"overlap"=function(sl,sc,lcdata,rcdata){
  gag.loc.left<-seq(-sc/2,by=-sc,-sl+sc/2) # assume full array of can data (lcdata)
  gag.loc.right<-seq(sc/2,by=sc,+sl-sc/2) # use full array of can data (lcdata)
 #fill missing can data (lcdata,rcdata) with NA so full "data set" between adjacent laterals
  lclist<-list(gag.loc.left,lcdata);rclist<-list(gag.loc.right,rcdata)#create a list to use lapply
  n<-max(lengths(lclist))
  left<-lapply(lclist,`length<-`,n)
  m<-max(lengths(rclist))
  right<-lapply(rclist,`length<-`,m)
  lcdata<-left[[2]];rcdata<-right[[2]]
  lcdata[is.na(lcdata)]<-0 #set NAs to 0 for later summing of overlap
  rcdata[is.na(rcdata)]<-0
  gdata.left<-cbind(gag.loc.left,lcdata)# bind data to location
  gdata.right<-cbind(gag.loc.right,rcdata)
  #now check to see if either can array extends past adjacent lateral.  If so, trunctate to within lateral inter-distance
lcdata.index<-which(gdata.left[,1]>=-sl); lcdata.use<-lcdata[lcdata.index]# fetch cans within sl
rcdata.index<-which(gdata.right[,1]<=sl);rcdata.use<-rcdata[rcdata.index]

#move cans by Sl
lc.over<-rev(lcdata.use) #invert order to overlap.  rev in base package
rc.over<-rev(rcdata.use)
#print(lc.over);print(rc.over)
sum.left<-lcdata.use+rc.over;sum.right<-rcdata.use+lc.over
return(list("sum.left"=sum.left,"sum.right"=sum.right)) #array index is left to right in plan view.
}


