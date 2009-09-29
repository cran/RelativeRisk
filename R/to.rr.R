to.rr<-
function(or,limit=c(1,0),theta=1,scaledIn=TRUE,scaledOut=TRUE,oneSided=FALSE) {
# Converts odds ratios to relative risks
# or: a vector of odds ratios assuming that the enviromental variables have range as per limit
# theta: The product of theta and nr
# limit: If oneSided==TRUE, a scalar or vector of length length(rr)-1. If a scalar, it will be expanded into a vector.
#        If oneSided==FALSE, a two element vector or matrix of dim (length(rr)-1,2). If a vector it will be exanded to a matrix
# scaledIn: If scaledIn=FALSE, or contains the raw glm coefficients produced with the logit link
# scaledOut: If scaledOut=FALSE, the output rr contains the coefficients
# oneSided: if TRUE, one sided calculations will be done
# returns a vector of relative risks

	n<-length(or)-1

	if (!oneSided) {
		if (!is.matrix(limit)) {
			limit<-matrix(limit,n,2,byrow=TRUE) 
		}
		rng<-limit[,1]-limit[,2]
		if (scaledIn) {
			cf<-log(or)
			cf[-1]<-cf[-1]/rng # the constant term is not scaled
		} else {
			cf<-or
		}

		rr<-exp(rng*cf[-1])*((theta+exp(cf[1]+limit[,2]*cf[-1]))/(theta+exp(cf[1]+limit[,1]*cf[-1])))
		if (!scaledOut) {
			rr<-log(rr)/rng
			rr<-c(log(exp(cf[1])/(1+exp(cf[1]))),rr)
		} else {
			rr<-c(exp(cf[1])/(1+exp(cf[1])),rr)
		}
	} else {
		if (!is.vector(limit)) {
			limit<-rep(limit,n)
		}

		if (scaledIn) {
			cf<-log(or)
			cf[-1]<-cf[-1]/(2*limit) # the constant term is not scaled
		} else {
			cf<-or
		}
		rr<-exp(limit*cf[-1])*( (theta+exp(cf[1]))/(theta+exp(cf[1]+limit*cf[-1])))
		if (!scaledOut) {
			rr<-log(rr)/limit
			rr<-c(log(exp(cf[1])/(1+exp(cf[1]))),rr*rr)
		} else {
			rr<-c(exp(cf[1])/(1+exp(cf[1])),rr*rr)
		}
	}

	rr
}
