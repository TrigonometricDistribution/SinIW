# rsiniw
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

rsiniw<-function(n,alpha,theta){
	library(stats)
  library(pracma)
	y=asin(0.999999999)
	limsup = (-log((2/pi)*y)/alpha)^(-1/theta)
	accept = c()
	count = 0
	while (length(accept) < n){
	  U = runif(1,0,limsup)
	  x = runif(1,0,limsup)
	  if(dunif(x, 0, limsup)*U <= dsiniw(x,alpha,theta)) {
	    accept[count] = x
	    count = count + 1
	  }
	}
	return(accept)
}
