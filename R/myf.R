#' myf function
#'
#' @param x one quantitative vector
#' @param xk one quantitative vector
#' @param coef quantitative vectors
#'
#' @returns A quantitative vector
#' @export
#'
#' @examples myf(2,2,c(2,3,4))
#'
myf = function(x,xk,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)
}
