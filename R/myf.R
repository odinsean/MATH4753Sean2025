#' myf function
#'
#' @param x one quantitative vector
#' @param xk one quantitative vector
#' @param coef one quantitative vector
#'
#' @returns A quantitative vector
#' @export
#'
#' @examples
#' \dontrun{ myf(2,2,3) }
#'
myf = function(x,xk,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)
}
