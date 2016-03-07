# psiniw
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

psiniw <- function(q,alpha,theta,lower = T,log.p = FALSE){
  if (lower == T){
    sin((pi/2)*exp(-alpha*q^(-theta)))
  }else{
    (1 - sin((pi/2)*exp(-alpha*q^(-theta))))
  }
}
