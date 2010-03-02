LatinHypercube <-
function (N, d, mins = rep(0, d), maxs = rep(1, d), varnames = paste("x", 
    seq(1, d), sep = "")) 
{
    library(emulator)
    nimrod.xmat <- mins + (maxs - mins) * latin.hypercube(N, 
        d)
    colnames(nimrod.xmat) <- varnames
    list(nimrod.xmat, mins, maxs)
}
