## ----setup, echo=FALSE--------------------------------------------------------
library(spreval)

## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
   dev = "png", dev.args = list(type = "cairo-png")
)

## ---- echo=TRUE,results='asis'------------------------------------------------
data(catchcan)
cc.data<-catchcan$solid.set
knitr::kable(cc.data,format="html")

## ----echo=TRUE,results='asis',fig.show='hold',fig.width = 5,fig.height = 5,fig.cap="Raw catch can data"----
#x,y matrix 20 ft x 20 ft catch can spacing
x<-seq(10,70,20) # catch can x coordinates. 
y<-seq(-10,-70,-20) #catch can y coordinates. 
grd<-list(x,y) # prepare list for make.surface function [fields]
grid<-fields::make.surface.grid(grd)
depths<-matrix(t(cc.data),ncol=1)#transpose matrix and stack rows into 1 column
cdata<-cbind(grid[ ,1],grid[ ,2],depths) #construct required catch can data matrix (x,y, depth)
sp.x<-c(0,0,80,80);sp.y<-c(0,-80,0,-80)# sprinkler spacing 80 x 80 ft
sploc<-cbind(sp.x,sp.y) #construct required sprinkler location matrix
spr.lab<-c("s12","s22","s13","s23")# label sprinklers as in reference
plotss(cdata,sploc,imcol=TRUE,spklab=spr.lab)# call function

## ---- echo=TRUE,results='asis'------------------------------------------------
ss.uni<-c(CU(cc.data),DU(cc.data),DU.lh(cc.data),PELQ(cc.data,SI=FALSE,rate=16.98,ss=80,sl=80,dur=2.25))# use U.S. cust. units
table<-round(ss.uni,0)
knitr::kable(t(table),col.names=c("CU","DU","DU.lh","PELQ"))#transpose array (table) for display


## ----echo=TRUE,results='asis',fig.show='hold',fig.width = 5,fig.height = 5,fig.cap="Distribution of catch can data in example"----
eda.shape(as.vector(cc.data)) # need to convert matrix to vector

## ----echo=TRUE, results='asis',fig.show='hold',fig.width = 5,fig.height = 4,fig.cap="Percentile plot with target depth for example"----
results<-adper(cc.data,target=0.57)
headings<-c(results[[1]][1],results[[3]][1],results[[5]][1],results[[7]][1])#extract label elements
row.2<-c(results[[2]][1],results[[4]][1],results[[6]][1],results[[8]][1]) #extract value elements
adper.table<-rbind(headings,round(row.2,3))
knitr::kable(adper.table,row.names=FALSE,caption="adequacy and efficiency using density and cumulative distributions")

## ---- echo=TRUE,results='asis'------------------------------------------------
eff.table<-eff(cc.data,target=0.57)# call function
eff.table<-round(c(eff.table$appeff,eff.table$appadeq),3)
labels<-c("efficiency","adequacy")
eff.table<-cbind(labels,eff.table)
knitr::kable(eff.table)#

## ----echo=TRUE, results='asis',fig.show='hold',fig.width = 4,fig.height = 5,fig.cap="Rotated ECDF plot with target depth for example"----
sfplot(as.vector(cc.data),target=0.57,ylab="depth caught, in.")

## ----echo=FALSE, results='asis'-----------------------------------------------
targets<-c(0.4,0.5,0.6,0.7)
effic<-rep(0,length(targets))  #intialize efficiency output vector
adeq<-rep(0,length(targets))#initialize adequacy output vector
for (i in 1:length(targets)){
 out<-eff(as.vector(cc.data),target=targets[i])
 effic[i]<-out$appeff
 adeq[i]<-out$appadeq
}
body<-rbind(targets, effic,adeq)
knitr::kable(body,digits=2,caption="Efficiency and adequacy for various target depths, using example catch can data")#


