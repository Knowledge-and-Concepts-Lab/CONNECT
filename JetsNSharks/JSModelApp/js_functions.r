#Name this render.network for ggplot/plotly:
tmp.func <- function(inputs, coord = jscoords){
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

	ggplotly(p)
}

#Rename this to render.network for base R plotting:
render.network <- function(inputs, coord = jscoords){
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

iac.activation <- function(act, net, decay = TRUE, rest = 0.0, maxout = 1.0, minout = -0.2, dt = 0.1){
#This function implements the original IAC activation function
#but using the same tweaks adopted by Doug Rhode in LENS as
#explained here: https://ni.cmu.edu/~plaut/Lens/Manual/Example/jets-n-sharks.html
#act: current activation
#net: current net inputs
#rest: resting activation level
#maxout: maximum activation level
#minout: minimum activation level
#dt: rate of activation change
#outputs: vector of new activations
################

	delta <- rep(NA, times = length(act))
	
	#Update for other units
	if(decay){
		#net positive:
		delta[net > 0] <- dt * ((maxout - act[net > 0]) * net[net > 0] - (act[net > 0] - rest))
		#net negative or zero:
		delta[net <= 0] <- dt * ((act[net <= 0] - minout) * net[net <= 0]  - (act[net <= 0] - rest))
		} else{
		#Without activation decay:
		delta[net > 0] <- dt * ((maxout - act[net > 0]) * net[net > 0])
		delta[net <= 0] <- dt * ((act[net <= 0] - minout) * net[net <= 0])
		}

	newact <- act + delta
	newact[newact > maxout] <- maxout #rectify to max
	newact[newact < rest] <- rest     #rectify to rest state

	newact
}

update.acts <- function(inputs, wts = jswts, dt = 0.2, timesteps =20, clamp = TRUE, decay = TRUE){
#This function takes an input vector and computes
#unit activations over the specified number of 
#timesteps for the room schema network
#inputs: A binary (0-1) vector of external inputs to units
#timesteps: Number of timesteps to computer
#rwts: Weights for the room schema model
#returns: a matrix where rows are units and columns are timesteps
#########

	#Initialize output matrix:
	o <- matrix(NA, length(inputs), timesteps)

	for(i in c(1:timesteps)){
		if(i==1) curract <- inputs else curract <- o[,i-1]
		net <- wts %*% curract #compute net inputs
		o[,i] <- iac.activation(curract, net, dt = dt, decay = decay)
		if(clamp) o[inputs==1.0,i] <- 1.0 #If clamping, set externally activated units to 1.0
		}
	row.names(o) <- row.names(wts)
	o
}

plot.wts <- function(wts, collim = c(-1,1)){
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
	par(mar = c(1,10,6,10)) #Set margin widths
	image(wts[,nitems:1], xaxt = "n", yaxt = "n", col = blue2red, zlim = collim) #Plot weights
	box() #draw box
	#left-side labels:
	mtext(side = 2, line = .1, at = c((nitems-1):0)/(nitems-1), adj = 1, text = colnames(wts), cex = .8, las = 2)
	#top labels:
	mtext(side = 3, line = .1, at = c(0:(nitems-1))/(nitems-1), adj = 0, text = row.names(wts), cex = .8, las = 2)
}


		
	