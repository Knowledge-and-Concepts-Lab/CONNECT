function (vmat) 
{
    nunits <- dim(vmat)[1]
    o <- matrix(0, nunits, nunits)
    for (i in c(1:nunits)) for (j in c(1:nunits)) {
        if (i != j) {
            psame <- mean(vmat[i,] == vmat[j,])
			psame <- .98 * psame + .01 * psame
            o[i, j] <- -log((1 - psame)/psame)
        }
    }
    baseact <- rowMeans(vmat==1)
	baseact <- .98 * baseact + .01 * baseact
    bias <- -log((1 - baseact)/baseact)
    o <- cbind(bias, o)
    colnames(o) <- c("bias", row.names(o))
    o
}
