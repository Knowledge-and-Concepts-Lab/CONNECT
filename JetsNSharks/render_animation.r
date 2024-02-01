function(activations, coord = jscoords){
#This function generates an animation showing propagation of activation
#in the network over time
#activations: matrix with columns containing activation of units at each time step
#coord: Coordinates of each unit on plot
####
	require(ggplot2)
	require(plotly)
	require(reshape2)
	
	pmat <- cbind(row.names(coord), coord, activations)
	names(pmat)[1] <- "unit"
	pmat <- melt(pmat, 1:3, variable.name="timestep", value.name="activation")
	pmat$activation <- round(pmat$activation, 3)
	
	p <- ggplot(data = pmat) + xlim(0,11) + ylim(0,15) +
			geom_rect(data = NULL, aes(xmin=X, xmax=X+1, ymin=Y, ymax=Y+1, fill = activation, ids=unit, frame=timestep), colour = "black") +
			scale_fill_gradient(low = "white", high = "red") +
			geom_text(data = NULL, aes(X + .5, Y+.5, label = unit), size = 3) + 
	
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

	fig <- ggplotly(p)
	fig %>% animation_opts(frame = 25, transition=0)
}
