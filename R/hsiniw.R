#' The hazard rate function of the SinInverseWeibull probability distribution.
#' @export
#'
#' @param x vector of quantiles.
#' @param alpha Alpha parameter.
#' @param theta Theta parameter.
#' @return A vector with n observations of the SinInverseWeibull distribution.
#' @examples
#' hsiniw(0.5,0.5,1.1)
#' hsiniw(0.5,1,1.9)

hsiniw <- function(x,alpha,theta){
  ((alpha*theta*pi)/2)*x^(-theta-1)*exp(-alpha*x^(-theta))*cos((pi/2)*exp(-alpha*x^(-theta)))/(1 - sin((pi/2)*exp(-alpha*x^(-theta))))
}
