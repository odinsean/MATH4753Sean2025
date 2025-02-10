#' mybin function
#'
#' @param iter one quantitative vector
#' @param n one quantitative vector
#' @param p quantitative vectors
#'
#' @returns A quantitative vector
#' @export
#'
#' @examples
#'
#'

mybin = function(iter,n,p){
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  succ=c()
  for( i in 1:iter){
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    succ[i]=sum(sam.mat[,i])
  }
  succ.tab=table(factor(succ,levels=0:n))
  succ.tab=table(factor(succ,levels=0:n))
}
