#' Title
#'
#' @param N number of seats on the flight
#' @param gamma probability a plane is indeed overbooked
#' @param p probability of a show
#'
#' @importFrom graphics abline curve layout
#' @importFrom stats qbinom pbinom pnorm uniroot
#'
#' @return 2 plots (discrete and continuous) representing the number of tickets and a list of variable values
#' @export
#'
#' @examples
#' ntickets(N = 400, gamma = 0.02, p = 0.95)
#' 
ntickets <- function(N, gamma, p){
  
  # stores the objective function
  objective <- function(N, gamma, p){
    qbinom(1 - gamma, N:(N*1.2), p)
  }
  # finds nd
  minimum <- function(N, gamma, p) {
    index <- which(N == qbinom(1 - gamma, N:(N*1.2), p))
    c(N:(N*1.2))[index]
  }
  
  n <- objective(N = N, gamma = gamma, p = p)
  nd <- minimum(N = N, gamma = gamma, p = p)
  
  # for y position of dots in discrete plot
  discy <- function(n, N, gamma, p){
    1 - gamma - pbinom(N, n, p)
  }
  
  # for continuous curve
  cont <- function(n){
    1 - gamma - pnorm(N, mean=n*p, sd=sqrt(n*p*(1-p)))
  }
  
  layout(matrix(1:2,ncol =1, nrow = 2, byrow = TRUE))
  
  plot(x = n,
       y = discy(n = n, N = N, gamma = gamma, p = p),
       xlab = "n", 
       ylab = "Objective",
       xlim = c(N, (N*1.1)),
       ylim = c(0, 1.0),
       pch = 19,
       col = "blue",
       type = "b",
       cex = 0.5,
       lty = 1,
       main = "Objective Vs n to find optimal tickets sold (412) gamma = 0.02, N = 400 discrete")
  
  abline(v = nd, col = "red", lwd = 1)
  abline(h = 0, col = "red", lwd = 1)
  
  curve(cont(n),
        xname = "n", 
        ylab = "Objective",
        xlim = c(N, (N*1.1)),
        ylim = c(0, 1.0),
        type = "l",
        lty = 1,
        main = "Objective Vs n to find optimal tickets sold (411.4949) gamma = 0.02, N = 400 continuous")
  
  
  abline(h = 0, col = "blue")
  
  droot <- uniroot(cont, interval=c(N, N*1.3))
  
  abline(v = droot$root, col = "blue")
  
  list(nd = nd, nc = droot$root, N = N, gamma = gamma, p = p)
  
}