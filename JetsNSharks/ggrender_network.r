function(inputs, coord = jscoords){
#This function plots activations in the jets-n-sharks network
#inputs: current inputs, vector range 0 - 1.
#amat: matrix containing activation of each unit across all time points
####

	#Below is for base R:
	#par(oma = c(1,1,1,1), mar = c(0,0,0,0))
	#
	#plot(1,1,type = "n", xlim = c(0, 11), ylim=c(0,15), 
	#	xaxt = "n", yaxt = "n", xlab = "", ylab="", bty = "n")
	#	
	#rect(coord$X, coord$Y, coord$X+1, coord$Y+1, col = hsv(0,inputs,1))
	#text(coord$X + .5, coord$Y + .5, adj = .5, labels=row.names(coord))
	
	coord$act <- round(inputs,3)
	
	p <- ggplot(data = coord, label=row.names(coord)) + xlim(0,11) + ylim(0,15) +
			geom_rect(data = NULL, aes(xmin=X, xmax=X+1, ymin=Y, ymax=Y+1, fill = act), colour="black") +
			scale_fill_gradient(low = "white", high = "red") +
			geom_text(data = NULL, aes(X + .5, Y+.5, label = row.names(coord), text=paste("act:",act))) + 
	
			theme(panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.title.x=element_blank(),
				axis.text.x=element_blank(),
				axis.ticks.x=element_blank(),
				axis.title.y=element_blank(),
				axis.text.y=element_blank(),
				axis.ticks.y=element_blank()
				)

	p
}
