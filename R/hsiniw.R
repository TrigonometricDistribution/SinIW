#' Generates a vector of hazard rate from a SinInverseWeibull probability distribution.
#'
#' @param x vector of quantiles.
#' @param alpha Alpha parameter.
#' @param theta Theta parameter.
#' @return A vector with n observations of the SinInverseWeibull distribution.
#' @examples
#' hsiniw(x, 0.5, 1.1)
#' hsiniw(x, 1, 1.9)

hsiniw <- function(x,alpha,theta){
  ((alpha*theta*pi)/2)*x^(-theta-1)*exp(-alpha*x^(-theta))*cos((pi/2)*exp(-alpha*x^(-theta)))/(1 - sin((pi/2)*exp(-alpha*x^(-theta))))
}
