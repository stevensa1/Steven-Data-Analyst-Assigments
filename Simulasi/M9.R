# Bangkitkan Variabel Random X dengan IID U[a,b] sebanyak n
# Nama: Steven Soewignjo
# NIM: 082011833060
# Tugas Minggu ke-9

gx <- function(x) {
    return(1/((x+5)^2))
}

montecarlo <- function(n, lb, ub) {
    vr <- runif(n, lb, ub)
    i <- (ub - lb) * sum(gx(vr))/n
    return(i)
}

acceptanceMethod <- function(n, lb, ub) {
    fmax <- max(gx(seq(lb, ub, by = 0.01)))
    xr <- runif(n, lb, ub)
    yr <- runif(n, 0, 1) * fmax
    count_in <- 0

    gx_res <- gx(xr)
    condition <- rep(0, n)

    for(i in 1:n) {
        if(yr[i] <= gx(xr[i])) {
            count_in <- count_in + 1
            condition[i] <- 1
        }
    }

    df <- data.frame(xr, yr, gx_res, condition)

    acc <- subset(df, condition == 1)
    rej <- subset(df, condition == 0)

    plotdata <- data.frame(xr, gx_res)
    plotdata <- plotdata[order(plotdata$xr),]

    win.graph()
    plot(rej[,1], rej[,2], pch=4, col = "black", main = "Acceptance-Rejection Method\nby Steven Soewignjo (082011833060)", xlab = "x", ylab = "y")
    lines(plotdata$xr, plotdata$gx_res, col="red", lwd=5)
    points(acc[,1], acc[,2], col="blue", lwd=1, pch = 16)
    legend("topright", legend=c("reject","accept" ,"g(x)"), col=c("black", "blue","red"), pch = c(16, 16, 16), cex=0.8,bg="white")

    result <- ((count_in) / n) * (ub-lb) * fmax
    return(result)
}

# Monte Carlo Integration
montecarlo(1000, 0, 10)

# Monte Carlo Acceptance-Rejection Method
acceptanceMethod(1000, 0, 10)
