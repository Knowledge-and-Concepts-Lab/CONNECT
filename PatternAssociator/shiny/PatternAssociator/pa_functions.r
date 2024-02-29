hebb.update <- function(m, inp, outp, lr = 1.0){
#This function updates weights in a pattern associator with the Hebb rule
#given a current weight matrix m and matrices of input and output patterns
#m: current weight matrix
#inp: matrix of input patterns
#outo: matrix with corresponding output patterns
#lr: learning rate
###################
	
	if(is.vector(inp)) inp <- matrix(inp, 1, length(inp)) #If input is a vector make it a matrix
	if(is.vector(outp)) outp <- matrix(outp, 1, length(outp)) #If output is a vector make it a matrix

	#Compute change in weights as matrix multiplication:
	delta <- t(t(outp) %*% inp) * lr
	
	delta + m
}

delta.update <- function(m, inp, outp, lr = 1.0){
#This function updates weights in a pattern associator with the Delta rule
#given a current weight matrix m and matrices of input and output patterns
#m: current weight matrix
#inp: matrix of input patterns
#outo: matrix with corresponding output patterns
#lr: learning rate
###################
	
	if(is.vector(inp)) inp <- matrix(inp, 1, length(inp)) #If input is a vector make it a matrix
	if(is.vector(outp)) outp <- matrix(outp, 1, length(outp)) #If output is a vector make it a matrix

	predout <- inp %*% m
	err <- outp - predout
	
	print(mean(err^2))
	
	delta <- t(t(err) %*% inp) * lr
	
	delta + m
}

get.err <- function(m, inp, outp){
#This function computes mean squared error for linear outputs
#given a current weight matrix m and matrices of input and output patterns
#m: current weight matrix
#inp: matrix of input patterns
#outo: matrix with corresponding output patterns
###################
	
	if(is.vector(inp)) inp <- matrix(inp, 1, length(inp)) #If input is a vector make it a matrix
	if(is.vector(outp)) outp <- matrix(outp, 1, length(outp)) #If output is a vector make it a matrix

	predout <- inp %*% m
	err <- outp - predout

	sum(err^2)
}

get.terr <- function(m, inp, outp, amin, amax){
#This function computes mean squared error for thresholded outputs
#given a current weight matrix m and matrices of input and output patterns
#m: current weight matrix
#inp: matrix of input patterns
#outp: matrix with corresponding output patterns
#amin: Minimum activation
#amax: Maximum activation
###################

	#Compure threshold:
    athresh <- amin + (amax - amin)/2

	if(is.vector(inp)) inp <- matrix(inp, 1, length(inp)) #If input is a vector make it a matrix
	if(is.vector(outp)) outp <- matrix(outp, 1, length(outp)) #If output is a vector make it a matrix

	predout <- inp %*% m
	threshout <- matrix(amin, dim(predout)[1], dim(predout)[2])
	threshout[predout > athresh] <- amax
	
	err <- 1 - mean(outp == threshout)

	err
}


make.pa <- function(inputs, targets, init=0){
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
	#Set weight matrix names:
	if(is.null(colnames(inputs))){
		row.names(m) <- paste0("In", c(1:innum))
	} else{
		row.names(m) <- gsub("in_", "", colnames(inputs))
	}
	if(is.null(colnames(targets))){
		colnames(m) <- paste0("In", c(1:innum))
	} else{
		colnames(m) <- gsub("out_","",colnames(targets))
	}
	m #Return initialized model 
}

plot.pa <- function(m, inpat, outpat, rd = 0.15){
#This function generates a plot of the current state of a pattern associator
#m: Matrix of model weights
#inpat: Input pattern to show
#outpat: Output pattern to show
#############
	par(oma = c (0,0,0,0), mar = c(2,2,1,1))
	innum <- dim(m)[1]  #Number of input units
	outnum <- dim(m)[2] #Number of output units
	
	#Could add check here to see if length of input and output patterns match
	
	#Get predicted outpat pattern
	predpat <- inpat %*% m
	
	plot(0,0,xlim = c(-1, outnum + .5), ylim = c(-2, innum + .5), type = "n",
		xaxt = "n", yaxt = "n", xlab = "", ylab="", bty="n")
		
	#mtext(side = 2, text = "Input pattern")
		
	for(i in c(1:outnum)) lines(c(i,i), c(-1, innum))
	for(i in c(1:innum)) lines(c(-1, outnum), c(i,i))
	
	maxwt <- max(abs(m)) #Largest weight for color scaling
	
	for(i in c(1:outnum)) for(j in c(1:innum)){
		if(m[j,i] < 0) hue <- 0.66 else hue <- 0.0 #Set hue to blue or red
		#Set saturation and value
		if(maxwt != 0){ 
			sat <- abs(m[j,i])/maxwt 
			val <- 1
			} else{ 
				sat <- 0 
				val = .8
			}
		
		draw.circle(i, innum - j + 1, radius = rd, col = hsv(hue, sat, val), border = NA)
		text(i,innum - j + 1,round(m[j,i],1), cex = .5)
		}
	
	
	# Add inputs
	incol <- rep("gray", times = innum)
	incol[inpat < 0] <- "blue"
	incol[inpat > 0] <- "red"
	for(i in c(1:innum)) rect(-1, innum - i-.4 + 1, 0, innum - i + 1.4, col = incol[i])
	text(rep(-.5, times = innum), c(innum:1), labels = round(inpat,3), col = "white", adj = 0.5)

	#Print input unit names if they are specified:
	if(!is.null(row.names(m))) {
	mtext(side = 2, at = c(innum:1), text = row.names(m), cex = 1.0, adj = 1, las = 2, line = 0)
		}


	# Add predicted
	predcol <- rep("gray", times = outnum)
	predcol[predpat < 0] <- "blue"
	predcol[predpat > 0] <- "red"
	for(i in c(1:outnum)) rect(i-.4, -1, i + .4, 0, col = predcol[i])
	text(c(1:outnum), rep(-.5, times = outnum), labels = round(predpat, 3), col = "white")

	### Add targets
	#Make color vector:
	outcol <- rep("gray", times = outnum)
	outcol[outpat < 0] <- "blue"
	outcol[outpat > 0] <- "red"
	
	for(i in c(1:outnum)) rect(i-.4, -2, i + .4, -1, col = outcol[i]) #Draw boxes
	text(c(1:outnum), rep(-1.5, times = outnum), labels = round(outpat, 3), col = "white") #Target values
	
	#Print ouput unit names if they are specified:
	if(!is.null(colnames(m))) {
		text(c(1:outnum), rep(-2.3, times = outnum), labels = colnames(m), cex = 1.0)
		}
	
	
	#Add labels for bottom boxes
	text(.4, -.5, "output", adj = 1)
	text(.4, -1.5, "target", adj = 1)
	
	#inputs <- read.csv("./data/inputs.csv", header = T, row.names = 1)
	#outputs <- read.csv("./data/outputs.csv", header = T, row.names = 1)
	
}