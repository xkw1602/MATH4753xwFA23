#' Title
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a a
#'
#' @return a plot of a normal distribution with 
#' @export myncurve
#'
#' @examples
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve = seq(-1000, a, length = 10000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(-1000, xcurve, a), c(0, ycurve, 0), col = "orange")
  area = pnorm(a, mu, sigma)
  area = round(area, 4)
  return (area)
}