#' myncurve function
#'
#' @param mu one quantitative vector
#' @param sigma one quantitative vector
#' @param a one quantitative vector
#'
#' @returns A quantitative vector
#' @export
#'
#' @examples
#' myncurve(10,5,6)
#'

myncurve = function(mu, sigma, a){
  x<-c()
  graphics::curve(stats::dnorm(x, mean = mu, sd = sigma),
        xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        col = "blue", lwd = 2)
  x_vals <- seq(mu - 3 * sigma, a, length.out = 1000)
  y_vals <- stats::dnorm(x_vals, mean = mu, sd = sigma)
  graphics::polygon(c(mu - 3 * sigma, x_vals, a), c(0, y_vals, 0), col = grDevices::rgb(0, 0, 1, 0.5), border = NA)
  probability <- stats::pnorm(a, mean = mu, sd = sigma)
  list(mu = mu, sigma = sigma, a = probability)
}
