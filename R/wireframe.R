wireframe <-
function (mlegpfit, numpredictvals, xandy) 
{
    library(lattice)
    library(emulator)
    mins <- mlegpfit[[2]][[2]]
    maxs <- mlegpfit[[2]][[3]]
    testmat <- apply(cbind(mins, maxs, numpredictvals), 1, function(x) seq(from = x[1], 
        to = x[2], length.out = x[3]))
    if (is.list(testmat)) {
        gridmat <- expand.grid(testmat)
    }
    else if (is.matrix(testmat)) {
        gridmat <- do.call("expand.grid", as.data.frame(testmat))
    }
    yhat <- predict(mlegpfit[[1]], newData = gridmat)
    gridmat <- as.data.frame(gridmat)
    for (j in (1:ncol(gridmat))[-xandy]) {
        assign(paste("x", j, sep = ""), factor(gridmat[, j], 
            labels = round(unique(gridmat[, j]), 2)))
    }
    for (j in xandy) {
        assign(paste("x", j, sep = ""), gridmat[, j])
    }
    xandystring <- paste(rep("x", 2), xandy, sep = "", collapse = "*")
    conditionstring <- paste(rep("x", ncol(gridmat) - 2), (1:(ncol(gridmat)))[-(xandy)], 
        sep = "", collapse = "*")
    if (length(numpredictvals) == 2) {
        conditionstring <- ""
    }
    else {
        conditionstring <- paste("|", conditionstring, sep = "")
    }
    names(gridmat) <- colnames(mlegpfit[[2]][[1]])
    cmd <- paste("lattice::wireframe(yhat ~", xandystring, sep = "")
    cmd <- paste(cmd, conditionstring, sep = "")
    cmd <- paste(cmd, ",\ndrape=TRUE,colorkey=TRUE,\nstrip=strip.custom(strip.levels=TRUE,\nstrip.names=TRUE))", 
        sep = "")
    eval(parse(text = cmd))
}

