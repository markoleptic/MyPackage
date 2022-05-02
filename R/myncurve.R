#' @name myncurve
#' @title myncurve
#'
#' @description display curve, shade area between curve and x axis from  -inf to x=a and calc the area
#'
#' @details A function from Lab 9 in MATH 4753
#'
#' @param mu the mean
#' @param sigma standard deviation
#' @param a xlim
#' @return display curve
#' @export
#'
#' @examples
#' \dontrun{myncurve(mu=2,sigma=3,a=3)
#' }
#'
myncurve = function(mu, sigma,a){
  x <- NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")
  area<-dnorm(a,mean=mu,sd=sigma)-dnorm(mu-3*sigma,mean=mu,sd=sigma)
  arear<-round(area,4)
  arear
}
