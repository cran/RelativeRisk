as.afactor<-
function(cName,data,levOrder,curLevs){
# Changes variable cName into a factor. 
# If present, the order of the levels will be as in levOrder
# If present, the levels in levOrder will be mapped into the corresponding levels in curLevs
# Note: a data frame is returned.
	data<-as.data.frame(data)
	a<-data[,cName]
	if (!missing(levOrder)) {
		b<-data[,cName]
		b<-as.vector(b)
		if (missing(curLevs))
			un<-levOrder
		else
			un<-curLevs
		if (length(levOrder)==length(un)) {
			m<-length(un)
			d<-rep(0,length(b))
			for (i in 1:m){
				d[un[i]==b]<-levOrder[i]
			}
			data[,cName]<-factor(d,levels=levOrder)
		} else
			stop("levOrder and curLevs missmatch in as.afactor().")
	} else {
		data[,cName]<-as.factor(a)
	}

	data
}
