as.twolevel<-
function(cName,data,levs){
# Changes all values in data[,cName] corresponding to elements of levs to 0, and sets the remaining values to 1

	x<-data[,cName]
	a<-as.logical(colSums(outer(levs,x,FUN="==")))
	x[a]<-0
	x[!a]<-1
	data[,cName]<-x

	data
}
