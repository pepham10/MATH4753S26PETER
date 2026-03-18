#' @title Optimal number of airline tickets to sell
#'
#' @param N Number of seats on the aircraft
#' @param gamma Maximum acceptable probability that the flight is overbooked
#' @param p Probability that a ticketed passenger shows up
#'
#' @returns A named list containing nd (optimal tickets through discrete binomial), nc (optimal tickets through normal approximation), N, p, and gamma
#' @export
#'
#' @examples
#' \dontrun{ntickets(N = 400, gamma = 0.02, p = 0.95)}
ntickets <- function(N, gamma, p) {
  n_low <- N
  n_high <- N + ceiling(N * 0.10)

  n_seq <- n_low:n_high
  obj_d <- 1 - gamma - pbinom(N, size = n_seq, prob = p)

  nd <- n_seq[which.min(abs(obj_d))]

  plot(n_seq, obj_d, type = "b", pch = 21, bg = "blue", col = "black", xlab = "n", ylab = "Objective", main = paste0("Objective Vs n to find optimal tickets sold\n(", nd, ") gamma=", gamma, " N=", N, " discrete"))
  abline(v = nd, h = 0, col = "red", lwd = 2)

  obj_c_fn <- function(n) {
    1 - gamma - pnorm(N + 0.5, mean = n * p, sd = sqrt(n * p * (1 - p)))
  }

  opt <- optimize(function(n) abs(obj_c_fn(n)), interval = c(n_low, n_high))
  nc <- opt$minimum

  n_cont <- seq(n_low, n_high, length.out = 1000)
  obj_c <- sapply(n_cont, obj_c_fn)

  plot(n_cont, obj_c, type = "l", lwd = 2, col = "black", xlab = "n", ylab = "Objective", main = paste0("Objective Vs n to find optimal tickets sold\n(", round(nc, 4), ") gamma=", gamma, " N=", N, " continuous"))
  abline(v = nc, h = 0, col = "blue", lwd = 2)

  result <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  print(result)
}
