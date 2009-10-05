as.twolevel<-
function(cName,data,levs){
# Changes all values in data[,cName] corresponding to elements of levs to 0, and sets the remaining values to 1
# Returns a data frame

	data<-as.data.frame(data)
	x<-data[,cName]
	y<-rep(0,length(x))
	a<-as.logical(colSums(outer(levs,x,FUN="==")))
	y[a]<-0
	y[!a]<-1
	data[,cName]<-y

	data
}
