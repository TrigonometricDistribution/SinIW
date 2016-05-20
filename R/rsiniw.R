#' Generates random deviates from a SinInverseWeibull probability distribution.
#'
#' @param n Number of observations to be generated.
#' @param alpha Alpha parameter.
#' @param theta Theta parameter.
#' @return A vector with n observations of the SinInverseWeibull distribution.
#' @examples
#' rsiniw(1, 1, 1)
#' rsiniw(1, 0.5, 0.7)

rsiniw <- function(n,alpha,theta){
  library(stats)
  library(pracma)
  library(fdrtool)

  accept <- c()
  count <- 0
  while (length(accept) < n){

    U <- rhalfnorm(1)
    x <- rhalfnorm(1)

    if(U <= dsiniw(x, alpha, theta)/(sqrt(pi)*dhalfnorm(x)/sqrt(2))) {
      accept[count] <- x
      count <- count + 1
    }
  }
  return(accept)
}
