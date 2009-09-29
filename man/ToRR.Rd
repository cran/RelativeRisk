\name{to.rr}
\alias{to.rr}
\title{To relative risk}
\description{
 A Logit-Log translation that converts odds ratios into relative risks.
}
\usage{
	to.rr(or,limit=c(1,0),theta=1,scaledIn=TRUE,scaledOut=TRUE,oneSided=FALSE)
}

\arguments{
	\item{or}{A vector of odds ratios,including the constant.}
	\item{limit}{A vector or matrix of variable limits
	  (upper,lower). If a matrix, there should be no row for the
	  constant.  A vector will be expanded into a matrix. }
	\item{theta}{\eqn{theta*nr} for case-control, where \eqn{theta} is the ratio
	  of marginal probabilities \eqn{theta=(1-P)/P}, and
	  \eqn{nr=(no. Cases)/(no. Controls)}}
	\item{scaledIn}{If \code{FALSE}, the raw \code{glm()} coefficients will be
	  expected.}
	\item{scaledOut}{If \code{FALSE}, unscaled \eqn{log(rr)} will be output.}
	\item{oneSided}{\code{TRUE} if only the upper limit is to be used.}
}
\value{
	A vector of relative risks including the constant.
      }

\details{
      This function estimates relative risks by using Logit-Log translations from a GLM logistic fit into
      coefficients for a GLM log fit. The translation is not perfect,
      but the errors are modest.

      For case-control, the cases and controls are assumed to be random
      samples drawn from two populations. The probability that an
      individual will belong to the case population is \eqn{P}, and the
      probability for control is \eqn{(1-P)}.
    }
    \note{
Logit-Log translations are obtained by equating the coefficients for \eqn{p} of
the logistic and log models.
      }
      \examples{
      to.rr(to.or(c(.3,1.5,2,.7,.3)))
      round(or<-to.or(c(.3,1.5,2,.7,.3)),2)
	  round(to.rr(or,theta=5),2)
}
\author{
	Bob Wheeler \email{bwheelerg@gmail.com}
	
	Please cite this program as follows:

Wheeler, R.E. (2009). to.rr \emph{RelativeRisk}. The R project for statistical computing \url{http://www.r-project.org/}
}


\keyword{htest}

