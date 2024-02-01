function(inputs, states, wts){
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
