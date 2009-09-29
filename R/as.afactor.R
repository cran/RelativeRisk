as.afactor<-
function(cName,data){
# Changes variable cName into a factor. Returns a data frame
	data<-as.data.frame(data)
	data[,cName]<-as.factor(data[,cName])

	data
}
