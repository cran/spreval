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
cc.data<-catchcan$traveler
knitr::kable(t(cc.data),format="html",caption="Raw data")

## ----echo=TRUE, results='asis',fig.show='hold',fig.width = 4,fig.height = 4,fig.cap="Catch can collected depths with station"----
plot(cc.data)

## ---- echo=TRUE,results='asis',fig.show='asis',fig.width = 4.5,fig.height = 4.5,fig.align="center"----
lcdata<-cc.data[1:8,2] #left data is first 8 observations
rcdata<-cc.data[9:16,2]#right data is last 8 observations
out<-travunif(ls=224,cs=20,lcdata,rcdata)#omit site name, and allow plot

## ----echo=FALSE,results='asis'------------------------------------------------
header<-c(names(out)[2],names(out)[3],names(out)[4])#extract names of returned list
values<-c(out$CU,out$DUlh,out$DU) #explicitly extract numeric values
knitr::kable(t(round(values,1)),format="html",col.names=header,caption="Uniformity measures")

## ----echo=TRUE,results='asis'-------------------------------------------------
#overlapped depths are first in list returned from
#`travunif` 
PELQT(out$o.depths,SI=FALSE,rate=197,ls=224,ts=1.75)

