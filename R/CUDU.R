#scripts to compute traditional measures of sprinkler uniformity
# see IA "Irrigation" text and others
###function to calculate CU, Christiansen's uniformity coefficient


"CU" = function(x)
{
  ((mean(x) - (sum(abs(x - mean(x)))/length(x)))/mean(x)) * 100
}

#function to to distribution uniformity
#of low-quarter
"DU"=function(x)
{
  #get subarray of low quarter
  x<-sort(x)
  end<-round(length(x)/4,digits=0)
  mean(x[1:end])/mean(x)*100
}

#function to compute Distribution uniformity of low half
"DU.lh"=function(x)
{
  x<-sort(x)
  end<-round(length(x)/2,digits=0)
  mean(x[1:end])/mean(x)*100
}


# find  percentile of area receiving target depth or more (by area covered assuming equal area per catch can)
# x is array of catch can data, target is target application depth.  If target is soil moisture
# deficit, then similar concept as PELQ, except catch rather than application rate is used.
# to be analogous to PELQ or AELQ, a target equal to a percentile of  87.5 (mid of low quartile or 25%) for adequacy assuming a normal
# distribution, would be returned.  With a small standard deviation, however, the percentile could "look bad" but a good AELQ
# could result since this is based on averages of low quartile and overall average rather than percentile.
# Best use of percentile would be for application efficiency with a target equal to SMD, so that anything greater than SMD
# would in theory not be held in the root zone so would be inefficient. Otherwise, depending upon application uniformity, it may be
# a poor measure of system performance.  In short it is a measure of efficiency relative to a target (keep application less
# than or equal to target) rather than uniformity.  The larger the target amount, the greater the efficiency,
# assuming that less than target with stay in root zone, but a lower adequacy will result.
# Use consistent units (SI, or US custom)

"adper"=function(x,target,plot=TRUE)
{
  out<-ecdf(x) (target)
  adeq<-1-out # frame as receiving target or more versus target as a percentile of catches
  ae<-out # percentile <= target amount.  If target is SMD, then percentile in root zone at SMD
  d<-density(x,n=512,cut=3)
  xx=d$x;yy=d$y;dx <- xx[2L] - xx[1L];C <- sum(yy) * dx  ## sum(yy * dx)
  adeq.unscaled <- sum(yy[xx >= target]) * dx;adeq.scaled <- adeq.unscaled / C # estimate of adequacy from density plot
  eff.unscaled<-sum(yy[xx <= target]) * dx;eff.scaled<-eff.unscaled/C # estimate of efficiency (at or below target)
  if(plot)
  {
    plot(d,main="Density Plot of Catch Data", xlab="catch depth")
    abline(v=target,col=2)
    text(target,0.0,"target depth",srt=90,adj=c(0,1))
   #print(xx);print(yy);print(2L);print(1L);print(C)
    xl<-c(min(x),max(x))
    plot(ecdf(x),verticals=TRUE,main="cumulative distribution",ylab="cum. prob.",xlab="depth applied (caught)",xlim=xl)
    abline(v=target,col=2)
    text(target,0.5,"target depth",srt=90,adj=c(0,1))
    }

  return(list("adequacy.density=",adeq.scaled,"eff.density=",eff.scaled,"adequacy.ecdf=",adeq,"eff.ecdf=",ae))
}

  "eff"=function(x,target){
  # get areas under curve for efficiency
  # use values of x (catch depth) with density to get areas above target and below target
  # sum(yy) for those values of xx less than or greater than target
    d<-density(x,n=512,cut=3)
    xx=d$x;yy=d$y;dx <- xx[2L] - xx[1L];C <- sum(yy) * dx  ## sum(yy * dx)
    above.x<-xx[xx>target];above.x<-above.x-target# array of x - target for above target, i.e., excess irrigation
    above.y<-yy[xx > target];tot.y<-sum(yy);yfract<-above.y/tot.y #y above target as ratio of total y
    excess<-sum(above.x*yfract)#multiply arrays and sum excess
    #print(excess)
    eff<-1-excess/target
  # above and below in units of length (mm, in.) for eff + adeq will not sum to 1.0 as did in adper
    below.x<-xx[xx<=target];below.x<-below.x-target# array of x - target for above target, i.e., excess irrigation
    below.y<-yy[xx <= target];tot.y<-sum(yy);yfract<-below.y/tot.y #y above target as ratio of total y
    deficit<-sum(below.x*yfract)
    #print(deficit)
    adeq<-1-abs(deficit)/target
   #print(eff);print(adeq)
  return(list("appeff"=eff,"appadeq"=adeq))
}


