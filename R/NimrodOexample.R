NimrodOexample <-
function (xlist) 
{
    xxmat <- xlist[[1]]
    nimrodogen <- function(xxmat) {
        dims <- ncol(xxmat)
        X1 <- apply(xxmat, 1, "sum") - xxmat[, 1]
        X2 <- apply(xxmat, 1, "sum") - xxmat[, 2]
        cos(pi * X2/(6 * (dims - 1))) - sin(pi * X1/(12 * (dims - 
            1))) + pi * X2/(12 * (dims - 1)) + pi * X1/(24 * 
            (dims - 1))
    }
    yval <- apply(xlist[[1]], 1, function(x) nimrodogen(matrix(x, 
        nrow = 1)))
    yval
}
