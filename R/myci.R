#' Title
#'
#' @param x a vector of data
#'
#' @importFrom stats qt
#'
#' @return a 95% confidence interval for mu
#' @export
#'
#' @examples
#' myci(fire$DAMAGE)
#' 
myci <- function(x){
  ybar = mean(x)
  std = sd(x)
  n = length(x)
  mp = c(-1, 1)
  alpha = 0.05
  t = qt(1-alpha/2, n-1)
  
  ybar + mp * t * std/sqrt(n)
}