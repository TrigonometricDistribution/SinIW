#' The survival function of the SinInverseWeibull probability distribution.
#'
#' @param x vector of quantiles.
#' @param alpha Alpha parameter.
#' @param theta Theta parameter.
#' @return A vector with n observations of the SinInverseWeibull distribution.
#' @examples
#' ssiniw(x,1,1)
#' ssiniw(x,1,0.1)

ssiniw <- function(x,alpha,theta){
  (1 - psiniw(x,alpha,theta))
}
