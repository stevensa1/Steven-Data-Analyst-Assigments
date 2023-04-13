# Bangkitkan Variabel Random X dengan IID U[a,b] sebanyak n

gx <- function(x) {
    return(sin(x))
}

gx1 <- function(x) {
    return(x^(sin(x))+5)
}

sinmontecarlo <- function(n, lb, ub) {
    vr <- runif(n, lb, ub)
    i <- (ub - lb) * sum(gx(vr))/n
    return(i)
}

sinmontecarlo(1000, 1, 20)

acceptanceMethod <- function(n, lb, ub) {
    fmax <- max(gx(seq(lb, ub, by = 0.01)))
    xr <- runif(n, lb, ub)
    yr <- runif(n, 0, 1) * fmax
    count_in <- 0
    for(i in 1:n) {
        if(yr[i] <= gx(xr[i])) {
            count_in <- count_in + 1
        }
    }
    result <- ((count_in) / n) * (ub-lb) * fmax
    return(result)
}
acceptanceMethod(100000, 0, pi)

