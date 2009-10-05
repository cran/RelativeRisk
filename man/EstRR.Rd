\name{est.rr}
\alias{est.rr}
\title{Estimates Relative Risk}
\description{
 Estimates relative risk using GLM and Logit-Log translations
}
\usage{
	est.rr(frml,data,indexed=FALSE,theta,doLog=TRUE,doTally=FALSE,sigDigits=2,cProb=c(0.025,0.975))
}

\arguments{
	\item{frml}{a standard formula like \eqn{y~X1+X2}, or a special one like \eqn{y1|y2~X1+X2}, where \eqn{y1} and \eqn{y2} are both columns in data}
	\item{data}{A data frame.}
	\item{indexed}{If \code{TRUE}, \eqn{y2} in the formula is an index with values \eqn{(0,1)}, indexing the counts in \eqn{y1}. If 
		\code{FALSE}, \eqn{y2} in the formula is the "failure" column corresponding to the "success" column in \eqn{y1}.}
	\item{theta}{For case-control data. The ratio of population
	  probabilites. A two column (case,control) response is
	  required.}
	\item{doLog}{If \code{FALSE}, GLM estimation will be suppressed, and
	  the relative risks will be estimated by the Logit-Log translation only.}
	\item{doTally}{If \code{TRUE}, \code{tally.data()} will be used to tally the
	  data into strata. }
	\item{sigDigits}{Comparison digits used by \code{tally.data()}.}
	\item{cProb}{Confidence probabilites.}
}
\value{
	A list with elements ( \code{log}, \code{glm}, \code{table}).
	The \code{glm} element will be present if
	 a GLM fit with log link was successful and \code{doLog=TRUE}, otherwise
	\code{glmLogit} will appear. \code{glm} contains the output of
	\code{glm()}. \code{table} gives the relative risk estimates and their
	confidence intervals plus the odds ratios. The relative risk values in
	\code{table} may differ from those
	obtainable from the coefficients in \code{glmLogit}, since the
	jackknife produces a slightly different estimate.
	Confidence intervals will be shown in \code{table} if either the glm fit with log link was
	successful or the jackknife worked. The heading of the relative risk column will be labeled \code{LL-rr}
	if the Logit-Log translation was used.
	
	\code{log} will contain comments.  
}

\details{
	This function estimates relative risks from either prospective or
	retrospective data. It should never fail to produce an
	estimate. It may however fail to return confidence intervals.

	A vignette giving further details is availble. To access it, type
	
	vignette("RelativeRisk")
	
	If \code{doLog==TRUE}, it first attempts to estimate relative risks
	using \code{glm()} with a log link. If \code{glm()} fails,
	it tries \code{glm()} with modified data (see note). If this
	fails, it estimates relative risk with a logit link, uses
	Logit-Log translation (see the \code{to.rr()} documentation) to translate the
	resulting odds ratios to relative risks, and attempts to produce confidence limits with the jackknife. 

	Several types of response are recognized: 

		(1) The response may be a two column matrix defined in the calling environment, with columns 
		(success,failure), or (case,control), containing  counts. 

		(2) The response may be a single variable, defined in the calling environment, 
		or the name of a variable in \code{data}. The
		single variable should  contain values \eqn{(0,1)}, where 1 denotes
		success (if \code{doTally=TRUE}, such a variable will be tallied into a two column
		matrix with success and failure columns).

		(3) The response may be the names of two variables in data containing counts of both success and failure, with the 
		two names separated by a "|", like \eqn{y1|y2}. 
		
		(4) The response may be the names of two variables in data with the names separated by a "|", like \eqn{y1|y2}. The first
		variable \eqn{y1} should contain counts of both success and failure, and the second \eqn{y2} index values \eqn{(0,1)} 
		to distinguish the two types: 1 denotes success. \code{indexed} should be set to \code{TRUE} in this case , and 
		\code{doTally} will be set to \code{TRUE} by the function. 


	A dot may be used after the \code{"~"} to indicate that all
	variables in data are to be included linearally in the model

	The model is \eqn{\log(p)=a+b'x}{log(p)=a+b'x}, where \eqn{x} is a vector of environmental
	variables. Relative risks are \eqn{\exp(b)}{exp(b)} when the variables are
	coded \eqn{(0,1)}. When they are coded \eqn{(l,u)}, the relative risk is
	\eqn{\exp((u-l)b)}{exp((u-l)b)}.  \code{coef(glm)} for \code{glm} an element of the output list, gives \eqn{b} values. \code{est.rr()}
	returns \eqn{\exp((u-l)b)}{exp((u-l)b)}. The function \code{as.twolevel()} may be used to
	code  data into two \eqn{(0,1)} levels. The meaning of relative risks
	for variables with multiple levels is not always clear; however,
	factors may be used. \code{est.rr()} uses \code{contr.treatment} when
	factors are present. This is a \eqn{(0,1)} coding. \code{est.rr()} uses a form that contrasts the
	various levels with the last level; thus a three level factor
	for variable \code{X}, will have coefficients for \code{X1}, and \code{X2}, representing
	the relative risk of the first and second levels with
	respect to the third level; i.e., the risk for the last level is in the denominator.

	The formula may be used to create product and other terms. The
	meaning of relative risk for such terms is not obvious, and they
	often create problems for the glm algorithm. .

	Most data will support only a few variables: over parameterization can cause convergence problems.
	In addition, GLM is no different from multivariate regression with respect to substantial
	correlation among the independent variables. If anything, it is
	more sensitive. Correlation among the variables introduces
	redundancy into the model, and the resulting estimates become
	partially confounded, which is why the coefficients sometimes
	change drastically when a variable is added or deleted. 

	Retrospective case-control data may be analyzed by setting
	\code{theta} to the ratio of population probabilities. It is
	assumed that there are two populations: one from which the
	cases are drawn and the other from which the controls are
	drawn. The marginal probabilities for these are \eqn{P} and \eqn{(1-P)}, and
	\eqn{theta=(1-P)/P}. \code{doTally} should be set to \code{TRUE} in order to produce
	a response with two columns (case, control). The estimates
	are fairly robust to misspecification of theta. The only time a modest theta
	will have much effect, is when the cases greatly outnumber the controls.

	
      }

      \note{GLM with a log link may fail to converge. One reason for this is the fact
	that the weight function used in solving the likelihood
	equations can become infinite, Lumley et.al. (2006). The following
	procedure, with the flavor of a continuity correction,  is used to
	avoid this problem:  Given a vector \eqn{y} of \eqn{(0,1)} response values, the vector
	is replaced by a [success,failure] two column matrix with values
	\eqn{[(yK+(1-y)),y+K(1-y)]}, where \eqn{K} is a large number, say \eqn{1000}. When
	\eqn{y} is a two column matrix it is modified to \eqn{[yK+(y==0)]}. This
	divides the variances of the estimates by \eqn{K}, without changing
	the scale of the estimates. It also multiplies the
	log-likelihood by \eqn{K}. As \eqn{K} increases the estimates converge to
	the correct values. For finite values, the estimates are
	slightly biased. \code{K=1000} is used in \code{est.rr}.

	}
\references{
      
       Lumley, T.,Kronmal, R. and Shuangge, M. (2006). Relative risk
       regression in medical research: models, contrasts, estimators,
       and algorithms. \emph{UW Biostatistics Working Paper Series}
       Paper 293. University of Washington. \url{http://www.bepress.com/uwbiostat/paper293}
}
\examples{

	data(gradData)
	aa<-est.rr(Count|Admitted~.,data=gradData,indexed=TRUE)
	names(aa)
	aa$log
	aa$table

	aa<-est.rr(Count | Admitted ~.,data=gradData,indexed=TRUE,doLog=FALSE)
	aa$table

	# Simpson's paradox, since the observed relative risks for Males in each department
	# tend to be greater than unity, while the overal all risk is less than unity.
	aa<-est.rr(Count|Admitted~Dept*Male,indexed=TRUE,data=gradData)
	aa$table

	# Convergence difficulties example:
	data(TitanicMat)
	# Reverse the sex variable
	TitanicMatR<-as.twolevel("sex",TitanicMat,1)
	# glm() fails to converge upon reversing the variable.
	# Dropping the Age variable allows convergence in this particular case.
	# as including information in the log.
	aa<-est.rr(count|survived~.,indexed=TRUE,data=TitanicMat)
	at<-est.rr(count|survived~.,indexed=TRUE,data=TitanicMatR)
	aa$table
	at$table

	# Interpreting factors example:
	# The relative risks are for cancer, and the factors are all compared 
	# to their most extreme level: tobacco for example is compared to the heaviest users.
	# This is case-control data, and since cancer is rare, theta should be large, which
	# tends to make the relative risks the same as the odds ratios.
	aa<-est.rr(ncases|ncontrols~.,esoph,theta=1000)
	aa$table

	# Retrospective sampling example with 600 Cases and 300 Controls. 
	# The true relative risks are (0.3,1.5,2.0,0.7,0.3)
	data(simData)
	aa<-est.rr(Success|Failure~.,data=simData,theta=2.2)
	aa$table

        # Very large odds raio:
        data(Melanoma)
        aa<-est.rr(NarrowExc|WideExc~.,Melanoma)
        aa$table


}

\author{
	Bob Wheeler \email{bwheelerg@gmail.com}
	
	Please cite this program as follows:

Wheeler, R.E. (2009). est.rr \emph{RelativeRisk}. The R project for statistical computing \url{http://www.r-project.org/}
}


\keyword{htest}

