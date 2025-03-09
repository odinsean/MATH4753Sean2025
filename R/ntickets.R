#' ntickets function
#'
#' @param N one quantitative vector
#' @param gamma one quantitative vector
#' @param p one quantitative vector
#'
#' @returns A quantitative vector
#' @export
#'
#' @examples
#' ntickets(400,0.02,0.95)
#'
ntickets <- function(N, gamma, p) {
  nd <- stats::qbinom(1 - gamma, N, p)
  nc <- stats::qnorm(1 - gamma, N * p, sqrt(N * p * (1 - p)))
  discrete_objective <- function(n) abs(1 - gamma - stats::pbinom(N, n, p))
  continuous_objective <- function(n) abs(1 - gamma - stats::pnorm(N + 0.5, n * p, sqrt(n * p * (1 - p))))
  n_values <- seq(N, N * 1.2, by = 1)
  discrete_values <- sapply(n_values, discrete_objective)
  continuous_values <- sapply(n_values, continuous_objective)
  nd_opt <- n_values[which.min(discrete_values)]
  nc_opt <- stats::optimize(continuous_objective, interval = c(N, N * 1.2))$minimum
  graphics::par(mfrow = c(2, 1))
  plot(n_values, discrete_values, type = "l", col = "black", lty = 1,
       main = paste("Objective Vs n to find optimal tickets sold\n (",nd_opt,") gamma=", gamma, " N=",N, " discrete"),
       xlab = "n", ylab = "Objective")
  graphics::points(n_values, discrete_values, pch = 20, col = "blue")
  graphics::abline(v = nd_opt, col = "red", lwd = 2)
  graphics::abline(h = 0, col = "red", lwd = 2)
  plot(n_values, continuous_values, type = "l", col = "black",
       main = paste("Objective Vs n to find optimal tickets sold\n (",nc_opt,") gamma=", gamma, " N=",N, " continuous"),
       xlab = "n", ylab = "Objective")
  graphics::abline(v = nc_opt, col = "blue", lwd = 2)
  graphics::abline(h = 0, col = "blue", lwd = 2)
  return(list(nd = nd_opt, nc = nc_opt, N = N, p = p, gamma = gamma))
}
