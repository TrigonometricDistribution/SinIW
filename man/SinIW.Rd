\name{SinIW}
\alias{SinIW}
\alias{dsiniw}
\alias{psiniw}
\alias{qsiniw}
\alias{rsiniw}
\alias{ssiniw}
\title{
The SinIW Distribution
}
\description{
Density, distribution function, quantile function, random generation and survival function for the sine weibull inverse distribution.
}
\usage{
dsiniw(x, alpha, theta)
psiniw(q, alpha, theta, lower = TRUE, log.p = FALSE)
qsiniw(p, alpha, theta, lower = TRUE, log.p = FALSE)
rsiniw(n, alpha, theta)
ssiniw(x, alpha, theta)
}
\arguments{
  \item{x, q}{
  vector of quantiles.
}
  \item{p}{
  vector of probabilities.
  }
  \item{n}{
  number of observations. If length(n) > 1, the length is taken to be the number required.
  }
  \item{alpha, theta}{
  parameters.
  }
  \item{log.p}{
  logical; if TRUE, probabilities p are given as log(p).
  }
  \item{lower}{
  logical; if TRUE (default), probabilities are \eqn{P[X \leq x]}, otherwise, \eqn{P[X > x]}.
  }
}
\details{
If lower or log.p are not specified they assume the default values of TRUE and FALSE, respectively.
}
\value{
dsiniw gives the density, psiniw gives the distribution function, qsiniw gives the quantile function, rsiniw generates random deviates and ssiniw gives the survival function.

The length of the result is determined by n for rsiniw, and is the maximum of the lengths of the numerical arguments for the other functions.

The numerical arguments other than n are recycled to the length of the result.
}
\references{
SOUZA, L. New Trigonometric Class of Probabilistic Distributions. 219 p. Thesis (Doctorate in Biometry and Applied Statistics) - Department of Statistics and Information, Federal Rural University of Pernambuco, Recife, Pernambuco, 2015.

BRITO, C. C. R. Method Distributions generator and Probability Distributions Classes. 241 p. Thesis (Doctorate in Biometry and Applied Statistics) - Department of Statistics and Information, Federal Rural University of Pernambuco, Recife, Pernambuco, 2014.
}
\author{
Dr. Luciano Souza, Luciano Serafim de Souza, Ms. Lucas Gallindo.
}

\seealso{
The \href{http://artax.karlin.mff.cuni.cz/r-help/library/actuar/html/InverseWeibull.html}{inverse weibull} distribution.
}
\examples{
x = rsiniw(1000,0.2,0.9)
hist(x,probability = TRUE,border = 0,ylim = c(0,1),xlim = c(0,5),main = "",ylab = "Density",lty = 2,lwd=2)
curve(dsiniw(x,0.2,0.9),add = T,lty=2,lwd=2,col=1)
}
