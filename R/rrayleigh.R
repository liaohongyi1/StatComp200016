#' @title Generate random numbers from a Rayleigh distribution
#' @description Using inverse transformation method to generate rayleigh distributed samples
#' @param n the number of samples
#' @param sig the parameter of rayleigh distribution
#' @return a random sample from of rayleigh distribution size \code{n}
#' @examples
#' \dontrun{
#' x <- rrayleigh(10,2)
#' }
#' @importFrom stats runif
#' @export
rrayleigh<-function(n,sig){
U<-runif(n)
x<-sqrt(-2*sig^2*log(1-U))
x
}

