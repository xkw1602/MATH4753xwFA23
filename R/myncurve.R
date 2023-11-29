#' Title
#'
#' @param mu sample mean
#' @param sigma sample standard deviation
#' @param a upper limit
#'
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#'
#' @return a plot of a normal distribution with the area from negative infinity to a colored
#' @export myncurve
#'
#' @examples
#' myncurve(mu = 10, sigma = 5, a = 6)
#' 
myncurve = function(mu, sigma, a){
    x <- NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve = seq(-1000, a, length = 10000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(-1000, xcurve, a), c(0, ycurve, 0), col = "orange")
  area = pnorm(a, mu, sigma)
  area = round(area, 4)
  return (area)
}