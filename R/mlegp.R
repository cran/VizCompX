mlegp <-
function (xlist, yval) 
{
    library(mlegp)
    Nimrodfit <- mlegp::mlegp(xlist[[1]], yval)
    list(Nimrodfit, xlist)
}

