# qsiniw
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

qsiniw<-function(p,alpha,theta,lower = T,log.p = FALSE){
  library(stats)
  library(pracma)
  if (lower == T){
    (-log((2/pi)*asin(p))/alpha)^(-1/theta)
  }else{
    (-log((2/pi)*asin(1-p))/alpha)^(-1/theta)
  }
}
