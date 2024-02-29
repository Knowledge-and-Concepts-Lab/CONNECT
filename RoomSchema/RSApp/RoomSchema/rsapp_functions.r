render.network <- function(inputs, amat){
#This function plots activations in the room schema network
#inputs: current inputs, vector range 0 - 1.
#amat: matrix containing activation of each unit across all time points
####

	require(plotrix)

	nunits <- length(inputs)
	maxtime <- dim(amat)[2]
	
	par(oma = c(0,0,0,0), mar = c(1,0,0,5))

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
		col = inputs+1, cex = 1.0)
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

tmp.update.acts <- function(inputs, rwts = rwts, dt = 0.2, timesteps =20, clamp = TRUE, decay = FALSE){
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
		currinput <- as.matrix(append(c(1), curract)) #append bias unit
		net <- rwts %*% currinput #compute net inputs
		o[,i] <- iac.activation(curract, net, dt = dt, decay = decay)
		if(clamp) o[inputs==1.0,i] <- 1.0 #If clamping, set externally activated units to 1.0
		}
	row.names(o) <- row.names(rwts)
	o
}
	
update.acts <- function(inputs, rwts = rwts, dt = 0.2, timesteps =20, clamp = TRUE, decay = FALSE){
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
		currinput <- append(c(1), curract) #append bias unit
		net <- currinput %*% rwts #compute net inputs
		o[,i] <- iac.activation(curract, net, dt = dt, decay = decay)
		if(clamp) o[inputs==1.0,i] <- 1.0 #If clamping, set externally activated units to 1.0
		}
	row.names(o) <- row.names(rwts)
	o
}

goodness <- function(inputs, states, wts){
#This function computes the goodness of an activation pattern
#over network units given the current inputs, activation pattern
#and weight matrix.
#inputs: external inputs to model units
#states: activations of all model units
#wts: matrix of weights between units
#returns: goodness-of-fit value for activation pattern
#######
	inputg <- sum(inputs * states) #Compute match bewteen input and activation states
	states <- append(c(1), states) #Add bias unit
	act.product <- as.matrix(states) %*% t(as.matrix(states)) 
	act.product <- act.product[2:length(states),] #Remove bias row
	g <- sum(act.product * wts) + inputg
	g
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
	par(mar = c(6,6,6,1)) #Set margin widths
	image(wts[,(nitems+1):1], xaxt = "n", yaxt = "n", col = blue2red, zlim = collim) #Plot weights
	box() #draw box
	#left-side labels:
	mtext(side = 2, line = .1, at = c(nitems:0)/nitems, adj = 1, text = colnames(wts), las = 2)
	#top labels:
	mtext(side = 3, line = .1, at = c(0:(nitems-1))/(nitems-1), adj = 0, text = row.names(wts), las = 2)
}




	