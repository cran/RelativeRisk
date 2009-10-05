\name{as.afactor}
\alias{as.afactor}
\title{As a factor}
\description{
 Changes columns of a data frame into factors
}
\usage{
	as.afactor(cName,data,levOrder,curLevs)
}

\arguments{
	\item{cName}{The quoted column name to change}
	\item{data}{A data frame, or matrix. }
	\item{levOrder}{The desired ordering of the levels.}
	\item{curLevs}{The current levels in the same order as in levOrder}
}
\value{
	A data frame with changes in the column \code{cName}. 
}

\details{

	Makes the column with name \code{cName} into a factor.

	if \code{levOrder} is present, these will be the levels of the factor. When \code{curLevs} is missing,the values in \code{levOrder} must be 
	the complete set of unique values in the \code{cName} columnn. 
	 
	If \code{curLevs} is present, the \code{levOrder} values will be mapped into them in the \code{cName} column; that is,
	if \code{curLevs=1:3}, and \code{levOrder=c("a","c","b")} then any value 2 in the \code{cName} column will be replaced
	by "c".
	 

}

\author{
	Bob Wheeler \email{bwheelerg@gmail.com}
	
	Please cite this program as follows:

Wheeler, R.E. (2009). as.afactor \emph{RelativeRisk}. The R project for statistical computing \url{http://www.r-project.org/}
}

\keyword{htest}
