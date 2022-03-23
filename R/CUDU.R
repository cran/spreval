#scripts to compute traditional measures of sprinkler uniformity
# see IA "Irrigation" text and others
###function to calculate CU, Christiansen's uniformity coefficient


"CU" = function(x)
{
  x<-na.exclude(x)
  ((mean(x) - (sum(abs(x - mean(x)))/length(x)))/mean(x)) * 100
}

#function to to distribution uniformity
#of low-quarter
"DU"=function(x)
{
  x<-na.exclude(x) # guard against padded data e.g., for created square/rectangular region from irregular area
  #get subarray of low quarter
  x<-sort(x)
  end<-round(length(x)/4,digits=0)
  mean(x[1:end])/mean(x)*100
}

#function to compute Distribution uniformity of low half
"DU.lh"=function(x)
{
  x<-na.exclude(x)
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
  ae<-out # percentile <= target amount.  If target is SMD, then percentile of area
  # receiving less or equal to SMD.
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
  # unlike adper, this function needs to quantify total volumes of water applied,
  # and volume below target depth (e.g., SMD or that below root zone) that is inefficient
  # and volume of under-irrigation (inadequate).  These volumes can be visualized in 2 dimensions
  # as a cumulative distribution plot of applied amounts assuming an unbiased sampling across the
  # irrigated area (even catch can distribution). So it must be more than areas under a density plot, and
  # must include the applied depth (x-axis) in the computations of efficiency and adequacy (by volume) if using
  # a density plot.  This can be acheived by weighting the density (relative # observations)
  # by mutiplying by applied depth, e.g. yy*dx*xx  Alternative would be to accumulate the density plot
  # and operate on that smoothed ecdf curve.
  # use values of x (catch depth) with density to get areas above target and below target
  # sum(yy) for those values of xx less than or greater than target
  # adequacy just looks at land area receiving equal to or more than the target, i.e., no credit for areas
  # receiving more than target amount. As target depth decreases relative to the mean applied depth
  # appeff decreases and appadeq increases (and vice-versa)
    d<-density(x,n=512,cut=3)
    xx=d$x;yy=d$y;dx <- xx[2L] - xx[1L];C <- sum(yy) * dx  ## sum(yy)* dx (total area). This is just area
    #under curve and does not account for magnitude of depth difference from target
    total<-sum(yy*xx)*dx # total area weighted by depth - this will equal the mean (or very close)
    above.x<-xx[xx>target];above.x<-above.x-target# array of x - target for above target, i.e., excess irrigation
    # i.e., horizontal lines from right side of curve to target
    above.y<-yy[xx > target] # pull all y values (density or relative occurrences) to right of target
    excess<-sum(above.y*above.x)*dx # weighted area greater (to right of) target depth (inefficient)
    eff<-1-excess/total # CC is total area weighted by depth
    below.x<-xx[xx<=target];below.x<-target-below.x# array of target-x for each x below target, i.e., array of 1D deficits
    below.y<-yy[xx <= target];deficit<-sum(below.y*below.x)*dx #weighted area under curve to left of target (inadequate)
    target.area<-sum(yy*target)*dx # target area (target depth * all entries*dx)
    #print(total);print(deficit);print(excess);print(target.area)
    adeq<-1-(deficit/target.area) # 1- deficit area/(area of target and less * target)

  return(list("appeff"=eff,"appadeq"=adeq))
}


