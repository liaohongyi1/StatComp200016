#' @title maximum number of extreme points
#' @description maximum number of extreme points of each sample with respect to the other sample
#' @param x the first sample
#' @param y the second sample
#' @return the maximum number of extreme points
#' @export
max_extremeout <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  return(max(c(outx, outy)))
} 

#' @title Laplace funcrion
#' @description compute the value of laplace function
#' @param x the independent variable of laplace function
#' @return the value of laplace funcrion
#' @export
lap_fun<-function(x){
  exp(-abs(x))/2  
} 

#' @title Random walk sampling
#' @description Use random walk sampling to generate samples from laplace distribution
#' @param sigma the parameter of varience
#' @param x0 the initial value
#' @param N the number of simulation
#' @return the random samples from laplace distribution
#' @importFrom stats runif dnorm rnorm
#' @useDynLib StatComp20016
#' @export
rw.Metropolis <- function(sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    num<-lap_fun(y)*dnorm(x[i-1], y, sigma)
    den<-lap_fun(x[i-1])* dnorm(y,x[i-1], sigma)
    if (u[i] <= num/den){
      x[i] <- y 
    }
    else {
      x[i] <- x[i-1]
      k <- k + 1
    }
  }
  return(list(x=x, k=k))
}
