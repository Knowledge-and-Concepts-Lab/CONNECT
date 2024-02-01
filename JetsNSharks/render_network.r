function(inputs, coord = jscoords){
#This function plots activations in the jets-n-sharks network
#inputs: current inputs, vector range 0 - 1.
#amat: matrix containing activation of each unit across all time points
####

	#Below is for base R:
	par(oma = c(1,1,1,1), mar = c(0,0,0,0))
	
	plot(1,1,type = "n", xlim = c(0, 11), ylim=c(0,15), 
		xaxt = "n", yaxt = "n", xlab = "", ylab="", bty = "n")
		
	rect(coord$X, coord$Y, coord$X+1, coord$Y+1, col = hsv(0,inputs,1))
	text(coord$X + .5, coord$Y + .5, adj = .5, labels=row.names(coord))
	
}
