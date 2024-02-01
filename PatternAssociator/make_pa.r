function(inputs, targets, init=0){
#This function creates a pattern association matrix given
#a set of input patterns and target patterns.
#inputs: input patterns
#targets: target patterns
#########################
	
	#Make sure same number of patterns:
	if(dim(inputs)[1] != dim(targets)[1]) stop("Different number of patterns in inputs and targets")
	
	innum <- dim(inputs)[2] #Number of input units
	outnum <- dim(targets)[2] #Number of output units
	
	if(init==0){
		#Initialize to all zeros if init is set to 0:
		m <- matrix(0, innum, outnum)} else{ 
		#Otherwise initialize with uniform random between -init and +init:
		m <- matrix((runif(innum*outnum) - .5) * 2 * init, innum, outnum)
		}
	row.names(m) <- paste0("In", c(1:innum)) #Name rows
	colnames(m) <- paste0("Out", c(1:outnum)) #Name columns
	m #Return initialized model 
}