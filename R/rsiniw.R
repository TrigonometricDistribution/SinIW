#' Generates random deviates from a SinInverseWeibull probability distribution.
#' @export
#'
#' @param n Number of observations to be generated.
#' @param alpha Alpha parameter.
#' @param theta Theta parameter.
#' @return A vector with n observations of the SinInverseWeibull distribution.
#' @examples
#' rsiniw(1000,0.1,0.9)
#' rsiniw(1000,0.2,0.8)

rsiniw <- function(n,alpha,theta){
  library(stats)
  library(pracma)
  library(fdrtool)

  accept <- c()
  count <- 0
  while (length(accept) < n){

    U <- fdrtool::rhalfnorm(1)
    x <- fdrtool::rhalfnorm(1)

    if(U <= dsiniw(x, alpha, theta)/(sqrt(pi)*fdrtool::dhalfnorm(x)/sqrt(2))) {
      accept[count] <- x
      count <- count + 1
    }
  }
  return(accept)
}
