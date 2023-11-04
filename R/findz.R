#' Title
#'
#' @param x input vector
#'
#' @return z-score for each element
#' @export
#' @importFrom stats sd
#'
#' @examples
findz <- function(x){
  z=(x-mean(x))/sd(x)
}
