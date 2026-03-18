#' @title Normal curve shading and probability
#'
#' @param mu Mean of the distribution
#' @param sigma Standard  deviation of the distribution
#' @param a The x-value to calculate P(X <= a)
#'
#' @returns A list containing mu, sigma, and the calculated probability
#' @export
#'
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#'
#' @examples
#' \dontrun{myncurve(mu = 10, sigma = 5, a = 6)}
myncurve = function(mu, sigma, a){
  curve(dnorm(x, mean=mu, sd=sigma),
        xlim = c(mu - 3*sigma, mu + 3*sigma),
        main = paste("Normal Distribution: P(X <=", a, ")"),
        ylab = "Density", xlab = "X")
  xcurve = seq(mu - 3*sigma, a, length=1000)
  ycurve = dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(mu - 3*sigma, xcurve, a), c(0, ycurve, 0), col="mediumpurple1")
  prob = pnorm(a, mean=mu, sd=sigma)
  list(mu = mu, sigma = sigma, prob = prob)
}
