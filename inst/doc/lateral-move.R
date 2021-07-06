## ----setup, echo=FALSE--------------------------------------------------------
library(spreval)
knitr::opts_knit$set(global.par = TRUE)

## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "png", dev.args = list(type = "cairo-png")
)

#set graphics device so that pkgdown::build_site() will run, seems to default to ragg



## ---- echo=TRUE---------------------------------------------------------------
data(catchcan)
cc.data<-catchcan$lateral
knitr::kable(cc.data,format="html")

## ----echo=TRUE,results='asis',fig.show='hold',fig.width = 4.5,fig.height = 4.5,fig.cap="Raw catch can data"----
x<-seq(-35,25,10) # x=0 is lateral position. 10 x 10 catch can spacing
y<-seq(55,5,-10)
grd<-list(x,y) # prepare list for make.surface function [fields]
grid<-fields::make.surface.grid(grd)
rates<-matrix(t(cc.data),ncol=1)#transpose matrix and stack rows into 1 column
cdata<-cbind(grid[ ,1],grid[ ,2],rates) #construct required catch can data matrix
sp.x<-rep(0,3);sp.y<-seq(0,60,30)# sprinkler spacing (y) = 30 ft
sploc<-cbind(sp.x,sp.y) #construct required sprinkler location matrix
spr.lab<-c("s4","s5","s6")# labels for sprinklers
plotss(cdata,sploc,imcol=TRUE,spklab=spr.lab)# call function

## ---- echo=TRUE---------------------------------------------------------------
sl<-50 # 50 ft lateral spacing (pass to overlap)
sc<-10 # 10 ft catch can spacing perpindicular to lateral (pass to overlap)
#split data into left and right of lateral
left<-cc.data[ ,1:4];right<-cc.data[ ,5:7]#first 4 columns left of lateral
super.l=matrix(data=NA,nrow=nrow(left),ncol=sl/sc) # columns will be limited to between 2 adjacent laterals
super.r=matrix(data=NA,nrow=nrow(right),ncol=sl/sc)
for (i in 1:nrow(left)){
  lcdata<-rev(left[i,]);rcdata<-right[i, ]#overlap requires order from proximal lateral  to distal so rev (left)
  super<-overlap(sl,sc,lcdata,rcdata)
  super.l[i, ]<-super$sum.left;super.r[i, ]<-super$sum.right
}
knitr::kable(super.l,format="html")
knitr::kable(super.r,format="html")

## ----echo=TRUE,fig.show='hold',fig.width = 4.5,fig.height = 4.5---------------
x<-seq(5,45,10) # x=0 is lateral position; look at right of test lateral
y<-seq(55,5,-10) # start at "top" y position, as a matrix
grd<-list(x,y) # prepare list for make.surface function [fields]
grid<-fields::make.surface.grid(grd)
o.rates<-matrix(t(super.r),ncol=1)#transpose matrix and stack rows into 1 column
o.cdata<-cbind(grid[ ,1],grid[ ,2],o.rates) #construct required catch can data matrix
sp.x.o<-c(sp.x,rep(50,3));sp.y.o<-c(sp.y,seq(0,60,30))#add second lateral location for overlap
sploc.o<-cbind(sp.x.o,sp.y.o) #sprinkler location matrix

## ----echo=TRUE,fig.show='hold',fig.width = 4.5,fig.height = 4.5,fig.cap="Overlapped catch can data at 50 ft lateral spacing"----
spr.lab<-rep(c("s4","s5","s6"),2)# labels for sprinklers
plotss(o.cdata,sploc.o,imcol=TRUE,spklab=spr.lab)# call function


## ---- echo=TRUE---------------------------------------------------------------
lower<-super.r[4:6,];upper<-super.r[1:3,]#use superimposed data
upper.uni<-c(CU(upper),DU(upper),DU.lh(upper),PELQ(upper,SI=FALSE,rate=4.6,ss=30,sl=50,dur=1))# use U.S. cust. units
lower.uni<-c(CU(lower),DU(lower),DU.lh(lower),PELQ(lower,SI=FALSE,rate=4.6,ss=30,sl=50,dur=1))
table<-round(rbind(upper.uni,lower.uni),0)
knitr::kable(table,row.names=TRUE,col.names=c("CU","DU","DU.lh","PELQ"))

## ---- echo=FALSE--------------------------------------------------------------
sl<-40 # 40 ft lateral spacing (pass to overlap)
sc<-10 # 10 ft catch can spacing perpindicular to lateral (pass to overlap)
#split data into left and right of lateral
left<-cc.data[ ,1:4];right<-cc.data[ ,5:7]#first 4 columns left of lateral and 3 to right of lateral
super.l.40=matrix(data=NA,nrow=nrow(left),ncol=sl/sc) # columns will be limited to between 2 adjacent laterals
super.r.40=matrix(data=NA,nrow=nrow(right),ncol=sl/sc)
for (i in 1:nrow(left)){
  lcdata<-rev(left[i,]);rcdata<-right[i, ]#overlap requires order from proximal lateral  to distal so rev (left)
  super.40<-overlap(sl,sc,lcdata,rcdata)
  super.l.40[i, ]<-super.40$sum.left;super.r.40[i, ]<-super.40$sum.right
}
knitr::kable(super.r.40,caption ="40 ft lateral spacing",format="html")
sl<-60 # 60 ft lateral spacing (pass to overlap)
sc<-10 # 10 ft catch can spacing perpindicular to lateral (pass to overlap)
#split data into left and right of lateral
super.l.60=matrix(data=NA,nrow=nrow(left),ncol=sl/sc) # columns will be limited to between 2 adjacent laterals
super.r.60=matrix(data=NA,nrow=nrow(right),ncol=sl/sc)
for (i in 1:nrow(left)){
  lcdata<-rev(left[i,]);rcdata<-right[i, ]#overlap requires order from proximal lateral  to distal so rev (left)
  super.60<-overlap(sl,sc,lcdata,rcdata)
  super.l.60[i, ]<-super.60$sum.left;super.r.60[i, ]<-super.60$sum.right
}
knitr::kable(super.r.60,caption= "60 ft lateral spacing",format="html")

## ---- echo=FALSE--------------------------------------------------------------
lower<-super.r.40[4:6,];upper<-super.r.40[1:3,]#use superimposed data
upper.uni<-c(CU(upper),DU(upper),DU.lh(upper),PELQ(upper,SI=FALSE,rate=4.6,ss=30,sl=40,dur=1))# use U.S. cust. units
lower.uni<-c(CU(lower),DU(lower),DU.lh(lower),PELQ(lower,SI=FALSE,rate=4.6,ss=30,sl=40,dur=1))
table<-round(rbind(upper.uni,lower.uni),0)
knitr::kable(table,row.names=TRUE,col.names=c("CU","DU","DU.lh","PELQ"),caption="40 ft lateral spacing")
lower<-super.r.60[4:6,];upper<-super.r.60[1:3,]#use superimposed data
upper.uni<-c(CU(upper),DU(upper),DU.lh(upper),PELQ(upper,SI=FALSE,rate=4.6,ss=30,sl=60,dur=1))# use U.S. cust. units
lower.uni<-c(CU(lower),DU(lower),DU.lh(lower),PELQ(lower,SI=FALSE,rate=4.6,ss=30,sl=60,dur=1))
table<-round(rbind(upper.uni,lower.uni),0)
knitr::kable(table,row.names=TRUE,col.names=c("CU","DU","DU.lh","PELQ"),caption="60 ft lateral spacing")

