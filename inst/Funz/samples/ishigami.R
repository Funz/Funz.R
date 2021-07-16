ishigami <- function (x)
{
    A <- 7
    B <- 0.1
    sin(x[, 1]) + A * sin(x[, 2])^2 + B * x[, 3]^4 * sin(x[,1])
}

cat('i=',ishigami(cbind(?a , ?b , ?c )),"\n")
