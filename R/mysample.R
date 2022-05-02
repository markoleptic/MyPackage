#' @name mysample
#' @title mysample
#'
#' @description generates a sample with a time factor
#'
#' @details A function from MATH 4753
#'
#' @param n sample size
#' @param iter number of iterations
#' @param time time intervals
#' @return display curve
#' @export
#'
#' @examples
#' \dontrun{mysample(n=10,iter=100)
#' }
#'
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}
