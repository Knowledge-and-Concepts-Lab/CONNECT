function(act, net, decay = TRUE, rest = 0.0, maxout = 1.0, minout = -0.2, dt = 0.1){
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

