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
# All vectors must be numeric
# Changes factors to vectors and  character vectors to numeric
# argument names may be numerical but coded as character, so change these to numeric

	toNumericVectors<-function(data,argNames){
		n<-ncol(data)
		colNames<-colnames(data)
		for (i in 1:n) {
			b<-data[,i]
			aFactor<-is.factor(b)
			if (aFactor) {
				levs<-levels(b)
				b<-as.vector(b)
			}
			if (any(colNames[i]==argNames)) {
				if (is.character(b)) {
					b<-as.numeric(b)
				}
			} 
			if (is.character(b)) {
				a<-data[,i]
				a<-rep(0,length(b))
				if (aFactor)  # use the original level sequence
					un<-levs
				else 
					un<-unique(b)

				for (j in 1:length(un)) {
					a[b==un[j]]<-j 
				}
				b<-a
			}
			data[,i]<-b
		}
		colnames(data)<-colNames
		data
	}

# Returns a vector with TRUE for factors in data
	get.factors<-function(data) {
		n<-ncol(data)
		vec<-rep(FALSE,n)
		for (i in 1:n) {
			if (is.factor(data[,i])) vec[i]<-TRUE
		}
		names(vec)<-colnames(data)
		vec
	}

# gets levels
	get.levels<-function(data){
		fac<-get.factors(data)
		cnames<-colnames(data)
		levs<-list(start=0)
		if (any(fac)){
			levNames<-cnames[fac]
			n<-length(levNames)
			levs<-list(start=n)
			for (i in 1:n) {
				theLevs<-unique(levels(data[,levNames[i]]))
				levs<-c(levs,list(ab=theLevs))
				names(levs)[i+1]<-levNames[i]
			}

		}

		levs		
	}


# creates factors
makeFactors<-function(mat,fType,fLevs) {

	n<-ncol(mat)
	for (i in 1:n) {
		if (fType[i]) {
			nm<-names(fType[i])
			lst<-fLevs[[nm]]
			mat[,i]<-factor(lst[mat[,i]],levels=lst)
		} 
	}
	mat
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

	outNames<-c(sName,paste("not",sName,sep="_"))
	if (!missing(fName)) {
		eVars<-eVars[eVars!=fName]
		argNames<-c(argNames,fName)
		responseType<-"Double"
		outNames<-c(sName,fName)
	}
	if (missing(indexName)) {
		indexName<-NA
	} else {
		eVars<-eVars[eVars!=indexName]
		argNames<-c(argNames,indexName)
		responseType="Count"
		outNames<-c(indexName,paste("not",indexName,sep="_"))
	}
	fType<-(get.factors(data))[eVars] 
	fLevs<-get.levels(data)
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
	if (length(eVars==1)) {
		miss<-is.na(data[,eVars])	# apply doesn't seem able to handle a single column
	} else {
		miss<-apply(is.na(data[,eVars]),1,any)
	}
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
	y<-mat[,1:2]
	y<-matrix(as.numeric(y),nn,2)
#	colnames(y)<-c(sName,paste("not",sName,sep="_"))
	colnames(y)<-outNames
	mat<-mat[,-(1:2)]
	mat<-as.data.frame(mat)
	colnames(mat)<-eVars 
	mat<-makeFactors(mat,fType,fLevs)
	return(list(log="All OK",Y=y,X=mat))
}
