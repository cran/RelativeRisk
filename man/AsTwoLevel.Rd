\name{as.twolevel}
\alias{as.twolevel}
\title{As two level}
\description{
 Changes columns of a data frame into two level vectors
}
\usage{
	as.twolevel(cName,data,levs)
}

\arguments{
	\item{cName}{The quoted column name to change}
	\item{data}{A data frame. }
	\item{levs}{A vector of levels. All variable values that are in
	  levels will be changed to 0, and all others to 1.}
}
\value{
	A data matrix with changes in column cName.
}


\author{
	Bob Wheeler \email{bwheelerg@gmail.com}
	
	Please cite this program as follows:

Wheeler, R.E. (2009). as.twolevel \emph{RelativeRisk}. The R project for statistical computing \url{http://www.r-project.org/}
}

\keyword{htest}
