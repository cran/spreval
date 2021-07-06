#function to do exploratory data analysis of object array
#histogram, boxplot, density and quartile plots constructed

"eda.shape" = function(x,title=NULL,qq=TRUE)
{
  oldpar<-par(no.readonly = TRUE) #get current plot parameters
  on.exit(par(oldpar))
  par(mfrow = c(2, 2))
  #extract name of array
  nm <-deparse(substitute(x))
  hist(x, xlab = nm, ylab = "no. observations",main=title)
  boxplot(x, plot = TRUE, notch = TRUE, col = NULL,border=1,
          staplelty = 1, staplewex = 1,
          staplehex = 1, outchar = TRUE, outpch = NA, outline = TRUE, outwex
          = 1, main=title,ylab=nm)
  iqd <- summary(x)[5] - summary(x)[2]
  if(is.null(title)){title=""}
  plot(density(x, width = 2 * iqd, na.rm = TRUE), xlab = nm, ylab = NULL,
       type = "l",main=title)
  if(qq){qqnorm(x, main=title,pch = 1)}
  if(qq){qqline(x)}
  invisible()

}

#function to spit out summary stats, skewness and kurtosis
"eda.stats" = function(x)
{

  summary<-summary(x)
  skew<-timeDate::skewness(x)#point to timeDate function for skew and kurtosis
  kurt<-timeDate::kurtosis(x)
  return(list("summary"=summary,"skew"=skew,"kurt"=kurt))
}

#function to do quantiles for 0.001 to 0.999
#and return interquartile range
"quart" = function(x)
{
  q<-quantile(x, c(0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,
                0.8, 0.9, 0.95, 0.99, 0.999))
  d<-diff(quantile(x, c(.25, .75)))
  return(list("quantiles"=q,"inter"=d))
}
