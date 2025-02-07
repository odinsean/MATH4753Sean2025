#' Mysqaure function
#'
#' @param x one quantitative vector and one data set
#'
#' @returns A quantitative vector
#' @export
#'
#' @examples
#' \dontrun{ rsq(20,spruce.df) }
#'
rsq = function(xk,data){
  df=within(data, X<-(BHDiameter-xk)*(BHDiameter>xk))
  lmp=lm(Height ~ BHDiameter + X, data=df)
  tmp = summary(lmp)
  tmp$r.squared
}
