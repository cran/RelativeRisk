to.or<-
function(rr,limit=c(1,0),scaled=TRUE,oneSided=FALSE) {
# Converts relative risks to odds ratios
# rr: a vector of relative risks assuming that the enviromental variables have range as per limit
# limit: If oneSided==TRUE, a scalar or vector of length length(rr)-1. If a scalar, it will be expanded into a vector.
#        If oneSided==FALSE, a two element vector or matrix of dim ((length(rr)-1),2). If a vector it will be exanded to a matrix
# scaled: If scaled=FALSE, rr contains the raw glm coefficients prodced with the log link
# oneSided: if TRUE, one sided calculations will be done
# returns a vector of odds ratios

	n<-length(rr)-1

	if (!oneSided) {
		if (!is.matrix(limit)) {
			limit<-matrix(limit,n,2,byrow=TRUE)
		}
		rng<-limit[,1]-limit[,2]

		if (scaled) {
			cf<-log(rr)
			cf[-1]<-cf[-1]/rng # the constant term is not scaled
		} else {
			cf<-rr
		}
		

		or<-c(exp(cf[1])/(1-exp(cf[1])),exp(rng*cf[-1])*((1-exp(cf[1]+limit[,2]*cf[-1]))/(1-exp(cf[1]+limit[,1]*cf[-1]))))
	} else {
		if (!is.vector(limit)) {
			limit<-rep(limit,n)
		}

		if (scaled) {
			cf<-log(rr)
			cf[-1]<-cf[-1]/(2*limit) # the constant term is not scaled
		} else {
			cf<-rr
		}
		or<-exp(limit*cf[-1])*( (1-exp(cf[1]))/(1-exp(cf[1]+limit*cf[-1])))
		or<-c(exp(cf[1])/(1-exp(cf[1])),or*or)
	}

	or
}
