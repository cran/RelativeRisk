tally.data<-
function(sName,fName,indexName,data,sigDigits=2){
# Tallies data into a two column matrix with columns (success, failure), and the remaining variables in data

# sName: the name of the column in data containing the success counts, or values in [0,1] if it is the only response: 1 is success
# fName: the name of the column in data containing the failure counts
# indexName: The name of the variable containing the success failure codes for the counts in sName: 1 is success, 0 is failure
# data: a data frame
# sigDigits: the number of significant digits used to compare numerical values


# sigCompare ##################################
# Compares numerical data to the accuracy of sigDigits
# Returns TRUE if they are equal to that accuracy
# Compares character data and returns TRUE if they are equal

	sigCompare<-function(x,y,sigDigits,cType){
		n<-length(x)
		for (i in 1:n){
			if (cType[i]=="character") {
			if (x[[i]]!=y[[i]]) return(FALSE)
		}
		else {
			#if (abs(x[[i]]-y[[i]])>abs(x[[i]])*sigDigits) return(FALSE)
			if (signif(x[[i]],sigDigits)!=signif(y[[i]],sigDigits)) return(FALSE)

			}
		}
		TRUE
	}

# fillTab ##############################
# Finds success-failure in sName and fName for next set of matching environmental vars
# Returns a list (success,failure, data variable values)

	fillTab<-function(i,data,sName,fName,indexName,responseType,eVars,cType,sigDigits){
		n<-nrow(data)
		j<-0
		cVars<-data[i,eVars] # note this is a list. It is also the vector that is output 
		 # as the typical vector for this set of matching environmental variables

		while ((i+j)<=n && sigCompare(data[i+j,eVars],cVars,sigDigits,cType[eVars])) j<-j+1

		tab<-rep(0,2)
		for (k in i:(i+j-1)) {
			if (responseType=="Single") {
				# there are 0 1 values in only one column -- sum them up
				a<-(2-data[k,sName]) # 1 is success, put it in the first column
				tab[a]<-tab[a]+1
			}
			else 
			if (responseType=="Double") { # there are counts in both columns -- pool them
				tab[1]<-tab[1]+data[k,sName]
				tab[2]<-tab[2]+data[k,fName]
			} else{ # The indexName column is an indicator column for the counts in the sName column
				a<-(2-data[k,indexName]) # 1 is success, put it in the first column
				tab[a]<-tab[a]+data[k,sName]
			}
		}
		list(i=i+j,tab=c(tab,unlist(cVars)))
	}


# checkTwoLevel #######################
# Checks a variable to make sure it has two levels, and that they are 0 and 1

	checkTwoLevel<-function(x){
		if (length(unique(x))!=2) return(FALSE)
		if (max(x)!=1) return(FALSE)
		if (min(x)!=0) return(FALSE)
		TRUE
	}

# toNumericVectors ###################
# Changes factors to vectors and  character vectors to numeric
# argument names may be numerical but coded as character, so change these to numeric

	toNumericVectors<-function(data,argNames){
		n<-ncol(data)
		colNames<-colnames(data)
		for (i in 1:n) {
			if (is.factor(data[,i])) {
				data[,i]<-as.vector(data[,i])
			}
			if (any(colNames[i]==argNames)) {
				if (is.character(data[,i])) {
					data[,i]<-as.numeric(data[,i])
				}
			} else
			if (is.character(data[,i])) {
				a<-data[,i]
				b<-rep(0,length(a))
				un<-unique(a)
				for (j in 1:length(un)) {
					b[a==un[j]]<-j-1# note the coding of characters starts at 0
				}
				data[,i]<-b
			}
		}
		data
	}

# Makes a vector indicating factor type
# 0 no factor, 1 factor, 2 ordered factor

	getFactors<-function(data) {
		#apply(data,2,is.factor) doesn't work
		n<-ncol(data)
		vec<-rep(FALSE,n)
		for (i in 1:n) {
			if (is.ordered(data[,i])) vec[i]<-2
			else if (is.factor(data[,i])) vec[i]<-1
			else vec[i]<-0
		}
		names(vec)<-colnames(data)
		vec
	}

# creates factors
makeFactors<-function(data,fType) {

	n<-ncol(data)
	for (i in 1:n) {
		if (fType[i]==2) data[,i]<-factor(data[,i],ordered=TRUE)
		else if (fType[i]==1) data[,i]<-factor(data[,i])
	}
	data
}

# colType ##############################3
# returns the column types in a vector
# Note the column names are attached to the vector

	colType<-function(data){

	apply(data,2,typeof)

	}


################################################
### Calculations start here


	if (!inherits(data,"data.frame")) {
		dataNames<-colnames(data)
		data<-data.frame(data)   # make sure it is a data frame
		colnames(data)<-dataNames
	}

	# figure out the response type

	eVars<-colnames(data)    # a vector of environmental variables excluding the arg names
	eVars<-eVars[eVars!=sName]

	argNames<-sName # begin collecting the argument names

	responseType<-"Single" 

	if (missing(fName)) {
		fName<-paste("not",sName,sep="_") # used only for output decoration
	}
	else {
		eVars<-eVars[eVars!=fName]
		argNames<-c(argNames,fName)
		responseType<-"Double"
	}

	if (missing(indexName)) {
		indexName<-NA
	} else {
		eVars<-eVars[eVars!=indexName]
		responseType="Count"
	}
	fType<-(getFactors(data))[eVars] # 0 not a factor, 1 a factor, 2 ordered factor 

	data<-toNumericVectors(data,argNames) 
	#data<-data.matrix(data) Not used because it can't deal with character vectors

	# Certain columns must have 0 1 values
	if (responseType=="Single") {
		if (!checkTwoLevel(data[,sName])) {
		stop(sName, " must have two levels, 0 and 1")
		}
	}

	if (responseType=="Count") {
		if (!checkTwoLevel(data[,indexName])) {
			stop(indexName, " must have two levels, 0 and 1")
		}
	}

	# remove any row with NA
	miss<-apply(is.na(data[,eVars]),1,any)
	if (any(miss)) {
		data<-data[!miss,]
	}

	# Get a vector of column types
	cType<-colType(data)

	# sort the environmental variables 
	for (i in eVars) {
		data<-data[sort(data[,i],index.return=TRUE)$ix,]
	}
	# fill a list with the resulting vectors, whose lead elements are (success,failure)
	n<-nrow(data)
	i<-1
	lst<-list(NA)
	repeat{
		if (i>n) break
		reslt<-fillTab(i,data,sName,fName,indexName,responseType,eVars,cType,sigDigits)
		lst<-c(lst,reslt$tab)
		i<-reslt$i
	}
	lst<-lst[-1]
	nn<-length(lst)
	if(responseType=="Single") {
		m<-ncol(data)+1
	} else
		m<-ncol(data)


	nn<-nn/m

	mat<-matrix(unlist(lst),nn,m,byrow=TRUE)
	colnames(mat)<-c(sName,paste("not",sName,sep="_"),eVars)
	y<-mat[,1:2]
	mat<-mat[,-(1:2)]
	mat<-as.data.frame(mat)
	colnames(mat)<-eVars # sometimes the names are lost
	mat<-makeFactors(mat,fType)
	return(list(log="All OK",Y=y,X=mat))
}
