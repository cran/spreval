## ----setup, echo=FALSE--------------------------------------------------------
library(spreval)
library(plotrix)

## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,
  comment = "#>",
   dev = "png", dev.args = list(type = "cairo-png"))

## -----------------------------------------------------------------------------
raw.data<-catchcan$landscape #import data from catchcan.rda
raw.data.in<-raw.data/((7.2/2)^2*pi)/2.54 #ml (cc) to cm depth and divide by 2.54 cm/in.
#need to convert matrix to vector for na.exclude, otherwise entire row with any NAs will be excluded
eff<-eff(na.exclude(as.vector(raw.data.in)),mean(raw.data.in,na.rm=TRUE)) # compute eff and adeq.
eff.out<-eff$appeff # extract for later use
adeq.out<-eff$appadeq # extract for later use
knitr::kable(raw.data,format="pipe",caption="Raw catch can data matrix, mls. 'NA' are phantom cans.")

## ---- echo=TRUE, results='asis',fig.align='left',fig.show='hold',fig.width = 5,fig.height = 5,fig.cap="Catch data with cans at 6 x 6 ft spacing."----
# create a x,y location grid from fields::
# catch can spacing is 6 ft x 6 ft
x<-seq(-27,27,9)
y<-seq(75,3,-9) # order - start at top (r1)
grd<-list(x,y)
grid<-fields::make.surface.grid(grd)
plot(grid, ylim=c(0,80),xlim=c(-30,30),xlab="ft",ylab="ft")
labels<-matrix(t(raw.data),ncol=1)
text(grid[ ,1],grid[ ,2]+3,labels,cex=0.7)
arrows(20,68,21.5,71,lw=3,length=0.1) #draw north arrow
text(23,75,"N",srt=-25) # north arrow

## ---- echo=TRUE,include=TRUE,fig.align='left',fig.show='hold',fig.width = 5,fig.height = 5,fig.cap="Catch data as densigram. 'Hotter' colors are greater collected depths. Black dots with bold italic  labels are sprinkler locations and numbers"----
labels[is.na(labels)]<-0 # interp will not accept missing values - set to 0
can.data<-cbind(grid[ ,1],grid[ ,2],labels) # prep for ssplot function
spr.x<-c(0,8,-11,0);spr.y<-c(18,39,48,60) # sprinkler locations bottom to top in plan
spr.loc<-cbind(spr.x,spr.y)
plotss(can.data,spr.loc,spklab=c("1","2","3","4"),ylab="ft", xlab="ft") 

## ---- echo=TRUE,fig.align='left',fig.show='hold',fig.width = 5,fig.height = 5,fig.cap="Catch rates (in/hr). Graphics parameter asp=1 for true x/y scaling."----
#now plot application rates
inches<-labels/((7.2/2)^2*pi)/2.54 #ml (cc) to cm depth and divide by 2.54 cm/in.
in.hr<-inches*4 # 15 minute run time in audit, i.e., both zones ran 15 minutes
in.hr.data<-cbind(grid[ ,1],grid[ ,2],round(in.hr,2))
# plot(in.hr.data). send plot to object for recall of par("usr")
# set asp=1 for true scale plot and non-skewed sprinkler wetted radius arcs
main.plot<-plotss(in.hr.data,spr.loc,spklab=c("1","2","3","4"),xlab="ft", ylab="ft",asp=1)
#reset to user coordinates used in plotss for subsequent low level plot drawing (arcs,lines)
par(usr=main.plot) # use this to reset par("usr") for low level plots (arc and lines)
cz1<-1;cz2<-1 # color for zone 1 and 2 arcs
#sprinkler 1
draw.arc(x=spr.x[1],y=spr.y[1],radius=27,deg1=-40,deg2=185,col=cz1,lwd=1.5)
draw.radial.line(0,27,center=c(spr.x[1],spr.y[1]),deg=-40,col=cz1,lwd=1.5)
draw.radial.line(0,27,center=c(spr.x[1],spr.y[1]),deg=185,col=cz1,lwd=1.5)
#sprinkler 2
draw.arc(x=spr.x[2],y=spr.y[2],radius=23,deg1=-20,deg2=175,col=cz2,lwd=1.5,lty=2)
draw.radial.line(0,23,center=c(spr.x[2],spr.y[2]),deg=-20,col=cz2,lwd=1.5,lty=2)
draw.radial.line(0,23,center=c(spr.x[2],spr.y[2]),deg=175,col=cz2,lwd=1.5,lty=2)
#sprinkler 3
draw.arc(x=spr.x[3],y=spr.y[3],radius=20,deg1=-5,deg2=220,col=cz2,lwd=1.5,lty=2)
draw.radial.line(0,20,center=c(spr.x[3],spr.y[3]),deg=-5,col=cz2,lwd=1.5,lty=2)
draw.radial.line(0,20,center=c(spr.x[3],spr.y[3]),deg=220,col=cz2,lwd=1.5,lty=2)
#sprinkler 4
draw.arc(x=spr.x[4],y=spr.y[4],radius=21,deg1=-20,deg2=195,col=cz2,lwd=1.5,lty=2)
draw.radial.line(0,22,center=c(spr.x[4],spr.y[4]),deg=-20,col=cz2,lwd=1.5,lty=2)
draw.radial.line(0,22,center=c(spr.x[4],spr.y[4]),deg=195,col=cz2,lwd=1.5,lty=2)

## -----------------------------------------------------------------------------
#determine CU,DU.lh, and DU (low quarter) based on catch rates
in.hr.actual<-in.hr[in.hr>0] # do not use "0" data as that was done for interp::interp
in.hr.avg<-mean(in.hr.actual) # compute average catch rate for late use (AELQ)
uni<-c(CU(in.hr.actual),DU.lh(in.hr.actual),DU(in.hr.actual))
table<-round(uni,0)
knitr::kable(t(table),col.names=c("CU","DU.lh","DU"))#transpose array (table) for display

## -----------------------------------------------------------------------------
aelq.table<-array(1:20,dim=c(5,4))
dur=c(60,90,120,150,180) # set first duration
catch<-array(length(dur))
smd=c(0.2,0.3,0.4,0.5) # set first SMD
for (i in 1:5){
  catch[i]<-dur[i]/60*in.hr.avg
  for(j in 1:4){
    if(catch[i]*(uni[3]/100)>=smd[j]){aelq.table[i,j]<-smd[j]/catch[i]}#mult. catch by DU so AELQ doesn't exceed PELQ (DU)
    else{aelq.table[i,j]<-NA}
    }
}
aelq.table<-round(aelq.table,2)*100 # round and convert to percent
dimnames(aelq.table)[[1]]<-as.character(dur)
knitr::kable(aelq.table,row.names=TRUE,col.names=as.character(smd),caption= "AELQ by duration (min, rows) and SMD (in., columns). NA for caught depths less than SMD.") 

## ----fig.align='left',fig.show='hold',fig.width = 5,fig.height = 5------------
sfplot(as.vector(na.exclude(raw.data.in)*4), mean(raw.data.in,na.rm=TRUE)*4,ylab="inches",main=NULL)# convert to in/hr for 1 hour irr.

## -----------------------------------------------------------------------------
effad.table<-array(1:10,dim=c(2,5))
target=c(0.2,0.3,0.4,0.5,0.6) # target depths
for (j in 1:5){
   entry<-eff(as.vector(na.exclude(raw.data.in*4)),target[j])
   effad.table[1,j]<-entry$appeff 
   effad.table[2,j]<-entry$appadeq
}

effad.table<-round(effad.table,2)*100 # round and convert to percent
dimnames(effad.table)[[1]]<-c("efficiency","adequacy")
knitr::kable(effad.table,row.names=TRUE,col.names=as.character(target),caption= "Efficiency and 
Adequacy for a 1 hour duration irrigation, by target depth, in. (columns)") 

