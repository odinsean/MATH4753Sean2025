#' myclt function
#'
#' @param n one quantitative vector
#' @param iter one quantitative vector
#' @param a one quantitative vector
#' @param b one quantitative vector
#'
#' @returns A quantitative vector
#' @export
#'
#' @examples
#' myclt(n=50,iter=10000,a=5,b=10)
#'
myclt=function(n,iter,a=0,b=5){
  x=c()
  y=stats::runif(n*iter,a,b)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=graphics::hist(sm,plot=FALSE)
  graphics::hist(sm,col=grDevices::rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  graphics::curve(stats::dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),from = min(sm), to = max(sm),add=TRUE,lwd=2,col="Blue")
  sm
}
