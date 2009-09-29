\name{to.or}
\alias{to.or}
\title{To odds ratios}
\description{
 A Logit-Log translation that converts relative risks into odds ratios.
}
\usage{
	to.or(rr,limit=c(1,0),scaled=TRUE,oneSided=FALSE)
}

\arguments{
	\item{rr}{A vector of relative risks,including the constant.}
	\item{limit}{A vector or matrix of variable limits
	  (upper,lower). If a matrix, there should be no row for the
	  constant. A vector will be expanded into a matrix. }
	\item{scaled}{If \code{FALSE}, raw coefficients from \code{glm()} will be expected.}
	\item{oneSided}{\code{TRUE} if only the upper limit is to be used.}
}
\value{
	A vector of odds ratios including the constant.
}

\details{
      This function estimates odds ratios by using Logit-Log translations from a GLM log fit to
      coefficients for a GLM logistic fit. The translation is not perfect,
      but the errors are modest.

    }
    \note{
Logit-Log translations are obtained by equating the coefficients for \eqn{p} of
the logistic and log models.
      }
\author{
	Bob Wheeler \email{bwheelerg@gmail.com}
	
	Please cite this program as follows:

Wheeler, R.E. (2009). to.or \emph{RelativeRisk}. The R project for statistical computing \url{http://www.r-project.org/}
}

\keyword{htest}

