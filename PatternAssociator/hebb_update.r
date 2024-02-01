function(m, inp, outp, lr = 1.0){
#This function updates weights in a pattern associator with the Hebb rule
#given a current weight matrix m and matrices of input and output patterns
#m: current weight matrix
#inp: matrix of input patterns
#outo: matrix with corresponding output patterns
#lr: learning rate
###################
	
	if(is.vector(inp)) inp <- matrix(inp, 1, length(inp)) #If input is a vector make it a matrix
	if(is.vector(outp)) outp <- matrix(outp, 1, length(outp)) #If output is a vector make it a matrix

	#innum <- dim(inp)[2] #Number of input units
	#outnum <- dim(outp)[2] #Number of output units
	
	delta <- t(t(outp) %*% inp) * lr
	
	delta + m
}

