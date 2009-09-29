est.rr<-
function(frml,data=sys.frame(sys.parent()),indexed=FALSE,theta,doLog=TRUE,doTally=FALSE,sigDigits=2,cProb=c(0.025,0.975)){

# estimates relative risk using glm with either a log or logit link, and if that fails, transforms logit
# frml: a formula. A single variable or a pair separated by "|", like "A|B"
# data: a matrix or data frame
# indexed: If TRUE, B in the response is a (0,1) indicator variable for the counts in A, other wise B is a count
# theta: if present, case control will be done; theta is the ratio of marginal probabilites  -- theta=(1-P)/P
# doLog: If FALSE, the GLM calculation will be suppressed and estimates will be by the LL-translation only.
# doTally: if TRUE, the data will be tallied before GLM -- otherwise, the response must have two columns (success, failure) counts
# sigDigits: sigDigits for tally.data()
# cProb: probs for confidence intervals

# returns a list, the first element of which is log containing a comment

# defines
modify<-TRUE  # use the modify algorithm
modConstant<-1000
tryGLMbeforeModify<-TRUE # Use ordinary glm before data is modified
jackknife<-TRUE
tolJack<-2 # tollerance comparing jackknife coef with logistic -- to detect big changes only

###########################

	# finds Name in string. Returns 0 if not found
	findName<-function(name,string) {
		return(regexpr(name,string,fixed=TRUE))
	}

	# replaces response with yName
	replaceResp<-function(frml,yName) {
		frmlString<-deparse(frml,500)
		loc<-findName("~",frmlString)
		frmlString<-substring(frmlString,loc)
		frmlString<-paste(yName,frmlString) 

		frml<-as.formula(frmlString) # The new formula with the yVal as response
		frml
	}

	# get second name in response from formula, returns a list containing name and new formula
	# returns NA in list element secondName if there is no second name
	getSecondName<-function(frml) {
		frmld<-deparse(frml,500)
		loc<-findName("|",frmld)
		if (loc==-1) return(list(secondName=NA))
		loc2<-findName("~",frmld)
		secondName<-substr(frmld,loc+2,loc2-2) # try to remove extra white space around name
		yName<-substr(frmld,0,(loc-1))
		frml<-replaceResp(frml,yName)
		return(list(secondName=secondName,frml=frml))
	}

	# returns TRUE if the model will have a constant
	isa.constant<-function(frml) {
		frmlString<-deparse(frml,500)
		loc<-findName("-1",frmlString)
		if (loc==-1) return(TRUE)
		return(FALSE)
	}

	# adds a string to the result list
	addResult<-function(string){
		if (exists("result")) {
			mat<-rbind(result[[1]],string)
			rownames(mat)<-1:nrow(mat)   # odd row names otherwise
			result<-list(log=mat)
		} else {
			result<-list(log=matrix(string,1,1))
		}
		result
	}


	# Returns TRUE for factors in data
	get.factors<-function(data) {
		n<-ncol(data)
		vec<-rep(FALSE,n)
		for (i in 1:n) {
			if (is.factor(data[,i])) vec[i]<-TRUE
		}
		vec
	}


	# If data has factors, sets the contrasts to contr.treatment with base level the last row
	set.contr<-function(data) {
		fac<-get.factors(data)
		if (any(fac)) {
			n<-length(fac)
			for (i in 1:n) {
				if (fac[i]) {
					m<-nlevels(data[,i])
					contrasts(data[,i])<-contr.treatment(m,base=m)
				}
			}
		}
		data
	}


	# runs glm with options changes. In particular it sets the contrasts to contr.treatment with base as the last row
	run.glm<-function(frml,data,link,wts,start) {
		tO<-as.numeric(options("warn"))
		options(warn=-1) 
		if (missing(wts)) {
			if (missing(start))
				glmOut<-try(glm(frml,data=data,family=binomial(link=link)),silent=TRUE)
			else 
				glmOut<-try(glm(frml,data=data,family=binomial(link=link),start=start),silent=TRUE)
		} else {
			if (missing(start))
				glmOut<-try(glm(frml,data=data,family=binomial(link=link),weights=wts),silent=TRUE)
			else
				glmOut<-try(glm(frml,data=data,family=binomial(link=link),weights=wts,start=start),silent=TRUE)
		}
		options(warn=tO)
		glmOut
	}

	# makes an output table
	makeTable<-function(coef,ci,or,colNames,cProb,LL=FALSE) {
		if (missing(ci)) {
			table<-matrix(round(c(exp(coef),exp(or)),3),length(coef),2)
			colnames(table)<-c("LL-rr","or")
		} else {
			table<-matrix(round(c(exp(coef),exp(ci),exp(or)),3),length(coef),4)
			prLab<-paste(round(100*cProb,1),"%")
			if (LL==FALSE)
				colnames(table)<-c("rr",prLab[1],prLab[2],"or")
			else 
				colnames(table)<-c("LL-rr",prLab[1],prLab[2],"or")
		}
		rownames(table)<-colNames
		table
	}




####################### End of functions

	# Check the formula for a pair of names, and extract them if present
	fName<-NA
	indexName<-NA
	val<-getSecondName(frml)
	if (!is.na(val$secondName)) {
		if (indexed==FALSE)
			fName<-val$secondName
		else
			indexName<-val$secondName
		frml<-val$frml
	}

	if (missing(data)) { # Create a data matrix from the global variables in frml
		frmla<-formula(paste("~-1+",paste(all.vars(frml),sep="",collapse="+"),sep=""))
		data<-data.frame(model.matrix(frmla))
	}
	else {
		if (!inherits(data,"data.frame")) {
			dataNames<-colnames(data)
			data<-data.frame(data)   # make sure it is a data frame
			colnames(data)<-dataNames
		}
	} 
	if (!isa.constant(frml))
#EXIT
		stop("The model must have a constant")

	# Make formula in case of dot argument
	if (-1!=findName(".",deparse(frml,500))) {
		dataNames<-colnames(data)
		respVar<-(all.vars(frml))[1]
		dataNames<-dataNames[respVar!=dataNames]
		if (!is.na(indexName))
			dataNames<-dataNames[indexName!=dataNames]
		if (!is.na(fName))
			dataNames<-dataNames[fName!=dataNames]
		frml<-as.formula(paste(respVar,"~",paste(dataNames,sep=" ",collapse="+")))
	}

	data<-set.contr(data)

	dataNames<-all.vars(frml)
# ************ Put the response vector or matrix in yVal, and make sure it has column names.
	yArg<-dataNames[1]
	if (exists(yArg,envir=parent.frame())) { # the response is external
		yVal<-get(yArg,parent.frame())

		if (is.data.frame(yVal)) { # try to work with a data frame
			if (ncol(yVal)==2) { 
				yVal<-as.matrix(yVal)
			} else 
				yVal<-as.vector(yVal)
		}

		if (is.matrix(yVal)) { # a (success,failure) matrix
			yNames<-colnames(yVal) # response names
			if (is.null(yNames)) { # must name the columns because the response name in formula is the name of the matrix containing the columns.
				yNames<-c("Success","Failure")
				colnames(yVal)<-yNames
			}
			responseType<-"External"     # (success, failure) external response matrix
		} else{
			yNames<-yArg# single response name
			yVal<-as.matrix(yVal)
			responseType="Single"
			colnames(yVal)<-yNames
		}
	} else { # response is in data
#		if (!missing(fName) && !missing(indexName)) {
#			stop("Either fName or indexName, but not both.")
#EXIT
#		}
		yVal<-as.matrix(data[,yArg])
		yNames<-yArg   # response name
		responseType<-"Single"
		if (!is.na(fName)) {
			yVal<-cbind(yVal,data[,fName])
			yNames<-c(yNames,fName)
			responseType<-"Double"
		} 
		colnames(yVal)<-yNames
		if (!is.na(indexName)) { # data contains a (0,1) variable telling how to assign the counts in yVal
			doTally<-TRUE
			yVal<-cbind(yVal,data[,indexName])
			yNames<-c(yNames,indexName)
			colnames(yVal)<-yNames
			responseType<-"Count"  # (count, index) matrix
		} 

	}
	if (!is.numeric(yVal)) {
		stop("The response must be numerical")
#EXIT
	}
	if (responseType=="Single") {
		yv<-unique(yVal)
		if (length(yv)!=2 || !all(c(0,1)%in%yv)) {
			stop("The response must have (0,1) values only")

#EXIT
		}				
	}
#**************** Tally the data
	if (doTally) { 
		result<-addResult("The data was tallied.")
		dataNames<-dataNames[dataNames!=yArg] # remove the response name from the list of data names
		if (responseType=="Count") {
			dataNames<-dataNames[dataNames!=indexName] # remove the index name from the list of data names
		}
		if (responseType=="Double") {
			dataNames<-dataNames[dataNames!=fName] # remove the falure name from the list of data names
		}

		X<-data[,dataNames, drop=FALSE]
		data<-cbind(yVal,data[,dataNames,drop=FALSE]) # prepare the data for tally by pasting the dependent on the front
		# Tally the data with (success,failure) as leading two columns followed by the environmental variables
		if (responseType=="External" || responseType=="Double") {
			val<-tally.data(sName=yNames[1],fName=yNames[2],data=data,sigDigits=sigDigits)
		} else if (responseType=="Count") {
			val<-tally.data(sName=yNames[1],indexName=yNames[2],data=data,sigDigits=sigDigits)
		}
		else { # single name with 0 1 values
			val<-tally.data(sName=yNames,data=data,sigDigits=sigDigits)
		}
		if (val$log=="All OK") {
			tal<-val$X
			yVal<-val$Y
		}
		else {
			stop(val$log)
#EXIT
		}

		tal<-as.data.frame(tal) 
	} else {
		tal<-data
	}

	tal<-set.contr(tal)

	# remove any data with (0,0) for response
	if (ncol(yVal)==2) {
		zeros<-apply(yVal==0,1,all)
		if (any(zeros)) {
			yVal<-yVal[!zeros,]
			tal<-tal[!zeros,]
			result<-addResult(paste(sum(zeros)," rows were removed because response had all zeros."))
		}
	}

# ******************** find limit
	frmlT<-replaceResp(frml,"")
	dmT<-model.matrix(frmlT,tal)
	limit<-matrix(0,ncol(dmT),2)
	limit[,1]<-apply(dmT,2,max)
	limit[,2]<-apply(dmT,2,min)
	rng<-limit[,1]-limit[,2]
	rng[1]<-1
	colNames<-colnames(dmT) 
	dmT<-0 # save memory

#***************************** Run glm() with logit link to get start for log link and for to.rr() if log fails 
	frml<-replaceResp(frml,"yVal")
	glmLogit<-run.glm(frml,tal,"logit") # Do this using yVal
	if (class(glmLogit)[1]=="try-error") {
#EXIT	
		stop("GLM with logit link failed.") # should be impossible unless data is screwed up
	}

#******************** Try glm with a log link
	if (doLog) { 
		yValC<-yVal
		if (!missing(theta)){
			if (ncol(yVal)<2) {
			stop("The response must have two columns: set doTally=TRUE")
#EXIT
			}
			NC<-colSums(yVal)
			yValC[,2]<-round(yValC[,2]*theta*(NC[1]/NC[2]))
		} 

		frml<-replaceResp(frml,"yValC")

		scalConst<-1 # const to scale SD

		# start glm() with est from logit
		start<-to.rr(coef(glmLogit),limit=limit[-1,],scaledIn=FALSE,scaledOut=FALSE)

		doModify<-TRUE
		if (tryGLMbeforeModify) { # set to FALSE to test the modify method
			doModify<-FALSE
			glmOut<-run.glm(frml,tal,"log",start=start)
			if (class(glmOut)[1]=="try-error") { # Failed, try without start
				glmOut<-run.glm(frml,tal,"log")
				if (class(glmOut)[1]=="try-error")
					doModify<-TRUE		# Failed again
			}
		}
		if (doModify && modify==TRUE){  # modify the data and try again
			result<-addResult("Trying GLM with modified data")
			if (dim(yVal)[2]==1) {
				yValT<-yValC*modConstant+(1-yValC)
				yValC<-cbind(yValT,yValC+modConstant*(1-yValC))
			} else {
				yValC<-yValC*modConstant+(yValC==0)
			}

			glmOut<-run.glm(frml,tal,"log",start=start)
			if (class(glmOut)[1]=="try-error") { # Failed, try without start
				glmOut<-run.glm(frml,tal,"log")
			}
			scalConst<-sqrt(modConstant) # const to scale SD
		}

		if (class(glmOut)[1]!="try-error") { # glm worked, so summarize and return
			sumry<-summary.glm(glmOut)$coefficients
			coef<-sumry[,1]*rng
			se<-(sumry[,2]*rng)*scalConst
			or<-coef(glmLogit)*rng
			ci<-c(coef+qnorm(cProb[1])*se,coef+qnorm(cProb[2])*se)
			table<-makeTable(coef,ci,or,colNames,cProb)
			result<-addResult("GLM successful.")
#EXIT
			return(c(result,list(glm=glmOut,table=table)))
		}  else { # glm did not work
			result<-addResult("GLM failed")
			glmOut<-glmLogit
		}
	} else { # log calculations have been skipped
		glmOut<-glmLogit
	}
# ******************************* Do transformed logit

	if (!missing(theta)) {
		if (ncol(yVal)<2) {
			stop("The Response must have two columns: set doTally=TRUE")
#EXIT
		}
		NC<-colSums(yVal)
		theta<-theta*(NC[1]/NC[2])
	} else {
		theta<-1
	}

	or<-coef(glmOut)
	rr<-log(to.rr(or,limit=limit[-1,],scaledIn=FALSE,theta=theta)) # log RR
	or<-or*rng
#**************** jackknife

	if (jackknife) {
		errJack<-FALSE
		talM<-cbind(1,tal) # If tal has a single column, deleting rows will loose dimension and column names
		yValM<-yVal
		n<-nrow(tal)
		m<-length(rr)
		rrI<-matrix(0,n,m)

		for (i in 1:n) {
			ind<-1:n
			ind<-ind[ind!=i]
			tal<-talM[ind,]
			yVal<-yValM[ind,]

			glmO<-run.glm(frml,tal,"logit")

			if (class(glmO)[1]=="try-error" || any(is.na(coef(glmO))) || m!=length(coef(glmO))) { # Serious error reduces coef length??
				result<-addResult("logit GLM failed in jackknife")
				errJack<-TRUE
				break
			} else {
				th<-log(to.rr(coef(glmO),limit=limit[-1,],scaledIn=FALSE,theta=theta)) # log RR
				rrI[i,]<-n*rr-(n-1)*th
			}
		}

		if (errJack) {
			table<-makeTable(rr,or=or,colNames=colNames)
			retVal<-c(result,list(glmLogit=glmOut,table=table))
		} else {
			rrEst<-colSums(rrI)/n

			if (any(abs(rr-rrEst)/(abs(rr)+0.1)>tolJack)) { # Don't accept big changes
				result<-addResult("jackknife failed after deleting a row.")
				table<-makeTable(rr,or=or,colNames=colNames)
				retVal<-c(result,list(glmLogit=glmOut,table=table))
			} else {
				rrEstMat<-matrix(rrEst,n,m,byrow=TRUE)
				thSD<-sqrt((colSums((rrI-rrEstMat)^2)/(n-1))/n)
				thSD<-thSD 
				ci<-c(rrEst+thSD*qt(cProb[1],n-1),rrEst+thSD*qt(cProb[2],n-1))
				result<-addResult("Logit transformed")
				table<-makeTable(rrEst,ci,or,colNames,cProb,LL=TRUE)
				retVal<-c(result,list(glmLogit=glmOut,table=table))
			}
		}
	}
	else {
		rr<-rr*rng
		result<-addResult("Logit transformed")
		table<-makeTable(rr,or=or,colNames=colNames)
		retVal<-c(result,list(glmLogit=glmOut,table=table))
	}

#EXIT
	retVal
}
