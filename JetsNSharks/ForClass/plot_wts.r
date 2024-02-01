function(wts, collim = c(-1,1)){
#This function plots weight values as a red-blue heat plot.
#wts: Weight matrix to be plotted
#collim: Limits of weight size in color range
#returns: Nothing, just generates plot

	nitems <- dim(wts)[1] #Number of items
	
	#Create a blue-white-red color plotting gradient:
	blue2red <- c(hsv(.66, c(10:0)/10, 1), hsv(0, c(1:10)/10, 1))
	
	#Rectify to lower limit:
	wts[wts < collim[1]] <- collim[1]
	
	#Rectify to upper limit:
	wts[wts > collim[2]] <- collim[2]
	
	#Generate plot
	par(mar = c(1,6,6,1)) #Set margin widths
	image(wts[,nitems:1], xaxt = "n", yaxt = "n", col = blue2red, zlim = collim) #Plot weights
	box() #draw box
	#left-side labels:
	mtext(side = 2, line = .1, at = c((nitems-1):0)/(nitems-1), adj = 1, text = colnames(wts), cex = .8, las = 2)
	#top labels:
	mtext(side = 3, line = .1, at = c(0:(nitems-1))/(nitems-1), adj = 0, text = row.names(wts), cex = .8, las = 2)
}

