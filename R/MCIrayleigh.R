#' @title Monte carlo integration of the probability density function of rayleigh distribution
#' @description Using a simple algorithm to compute integration of the PDF of rayleigh distribution
#' @param a the lower bound of integration
#' @param b the upper bound of integration
#' @param sig the parameter of rayleigh distribution
#' @param n the number of random uniform distributed samples
#' @return the value of integration
#' @importFrom stats runif 
#' @export
MCIrayleigh<-function(a,b,sig,n){
U<-runif(n,a,b)
y<-U/sig^2
x<-mean(y*exp(-y/2))
x
}
