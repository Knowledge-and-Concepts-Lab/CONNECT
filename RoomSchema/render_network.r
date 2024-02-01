function(inputs, amat){
#This function plots activations in the room schema network
#inputs: current inputs, vector range 0 - 1.
#amat: matrix containing activation of each unit across all time points
####

	require(plotrix)

	nunits <- length(inputs)
	maxtime <- dim(amat)[2]
	
	par(oma = c(1,1,1,1), mar = c(0,0,0,3))

	plot(0,0,type = "n", xlim = c(0, maxtime+.5), ylim=c(0,nunits), 
		xaxt = "n", yaxt = "n", xlab = "", ylab="", bty = "n")
	
	#Below adds a column to show inputs
	#rect(rep(0.05, times = nunits), c(0:(nunits-1)) + .05, rep(.75, times = nunits), 
	#	c(1:nunits) - 0.05, col = hsv(0,inputs,1))
		
	for(i in c(1:maxtime)){
	rect(rep(i + 0.05, times = nunits), nunits - c(0:(nunits-1)) + .05, rep((i+1) - .05, times = nunits), 
		nunits - c(1:nunits) - 0.05, col = hsv(0,amat[,i],1))
	}
	
	mtext(side = 4, at = (nunits) - c(1:nunits) + .5 , 
		line = 0, las = 2, adj = 0, text = row.names(amat), 
		col = inputs+1, cex = .75)
}
