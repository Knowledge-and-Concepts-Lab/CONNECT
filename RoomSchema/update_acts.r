function(inputs, rwts = rwts, dt = 0.2, timesteps =20, clamp = TRUE, decay = TRUE){
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
		
	
	