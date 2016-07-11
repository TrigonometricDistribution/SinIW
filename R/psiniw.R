#' The cumulative function of the SinInverseWeibull probability distribution.
#' @export
#'
#' @param q vector of quantiles.
#' @param alpha Alpha parameter.
#' @param theta Theta parameter.
#' @param lower Lower parameter.
#' @param log.p Log.p parameter.
#' @return A vector with n observations of the SinInverseWeibull distribution.
#' @examples
#' psiniw(q,1,1,TRUE,FALSE)
#' psiniw(q,0.5,0.7,TRUE,FALSE)

psiniw <- function(q,alpha,theta,lower = TRUE,log.p = FALSE){

  if (log.p == TRUE) {
    if (lower == TRUE){
      log(sin((pi/2)*exp(-alpha*q^(-theta))))
    }else{
      log((1 - sin((pi/2)*exp(-alpha*q^(-theta)))))
    }
  } else {
    if (lower == TRUE){
      sin((pi/2)*exp(-alpha*q^(-theta)))
    }else{
      (1 - sin((pi/2)*exp(-alpha*q^(-theta))))
    }
  }

}
