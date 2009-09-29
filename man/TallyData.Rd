\name{tally.data}
\alias{tally.data}
\title{Tallies data}
\description{
 Tallies data into strata according to the values of environmental variables.
}
\usage{
	tally.data(sName,fName,indexName,data,sigDigits=2)
}

\arguments{
	\item{sName}{The quoted name of the data column containing either success counts, or values in (0,1) to be pooled: 1 is success.}
	\item{fName}{If present, the quoted name of the data column containing
	  the failure counts matching the success counts in sName.}
	\item{indexName}{The quoted name of the index variable for the counts in sName: 1 is success.}
	\item{data}{A data frame, whose columns contain the values of
	  environmental variables defining the strata.}
	\item{sigDigits}{The number of significant digits used to compare numerical environmental variable levels.}
}
\value{
	A list with elements (\emph{log}, \emph{Y}, \emph{X}).  \emph{log} will contain a comment. \emph{Y} will contain a matrix,
	with columns (success,failure) containing counts
	for each stratum. The matrix \emph{X} will contain the values of the environmental variables for the stratum.
}

\details{
	This function may be used before performing a GLM analysis
	for binary data to prepare a matrix of success, failure counts
	and associated environmental variable values.  \code{tally.data()} will be called by \code{est.rr()} if \code{doTally}
	in \code{est.rr()} is set to \code{TRUE}. For large data sets, \code{tally.data()} may take a while, so it is best to run it 
	separately from \code{est.rr()} if possible.

	The strata are created by pooling together all data rows with equivalent sets of environmental variables. Numerical values are
	compared after rounding to \code{sigDigits}. The first set of
	environmental variables in a stratum is taken as typical and then output as
	the stratum's description in the output matrix.

      }
      \note{
	 All columns in data will be used to define the
	 strata. Redundant columns should be eliminated before calling
	 this function. if \code{tally.data()} is called from \code{est.rr()},
	 redundant columns not appearing in the formula will be
	 discarded automatically.
	
	}

\examples{

	data(TitanicMat)
	ad<-tally.data("count",indexName="survived",data=TitanicMat)
	ad$Y
	ad$X
}
	
      

\author{
	Bob Wheeler \email{bwheelerg@gmail.com}
	
	Please cite this program as follows:

Wheeler, R.E. (2009). tally.data \emph{RelativeRisk}. The R project for statistical computing \url{http://www.r-project.org/}
}


\keyword{htest}


