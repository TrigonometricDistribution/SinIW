#' The survival function of the SinInverseWeibull probability distribution.
#' @export
#'
#' @param x vector of quantiles.
#' @param alpha Alpha parameter.
#' @param theta Theta parameter.
#' @return A vector with n observations of the SinInverseWeibull distribution.
#' @examples
#' ssiniw(0.1, 1, 1)
#' ssiniw(0.1, 1, 0.1)

ssiniw <- function(x,alpha,theta){
  (1 - psiniw(x,alpha,theta))
}
