function(m, inpat, outpat, rd = 0.15){
#This function generates a plot of the current state of a pattern associator
#m: Matrix of model weights
#inpat: Input pattern to show
#outpat: Output pattern to show
#############
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
	text(rep(-.5, times = innum), c(innum:1), labels = round(inpat,1), col = "white", adj = 0.5)

	#Print input unit names if they are specified:
	if(!is.null(row.names(m))) {
	mtext(side = 2, at = c(innum:1), text = row.names(m), cex = .8, adj = 1, las = 2, line = 0)
		}


	# Add predicted
	predcol <- rep("gray", times = outnum)
	predcol[predpat < 0] <- "blue"
	predcol[predpat > 0] <- "red"
	for(i in c(1:outnum)) rect(i-.4, -1, i + .4, 0, col = predcol[i])
	text(c(1:outnum), rep(-.5, times = outnum), labels = round(predpat, 1), col = "white")

	### Add targets
	#Make color vector:
	outcol <- rep("gray", times = outnum)
	outcol[outpat < 0] <- "blue"
	outcol[outpat > 0] <- "red"
	
	for(i in c(1:outnum)) rect(i-.4, -2, i + .4, -1, col = outcol[i]) #Draw boxes
	text(c(1:outnum), rep(-1.5, times = outnum), labels = round(outpat, 1), col = "white") #Target values
	
	#Print ouput unit names if they are specified:
	if(!is.null(colnames(m))) {
		text(c(1:outnum), rep(-2.3, times = outnum), labels = colnames(m), cex = .8)
		}
	
	
	#Add labels for bottom boxes
	text(.4, -.5, "output", adj = 1)
	text(.4, -1.5, "target", adj = 1)
	
	
}

	
	