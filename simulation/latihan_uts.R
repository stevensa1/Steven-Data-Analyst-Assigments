congruential_gen <- function(x0, a, m, n) {
    x <- x0
    xi <- rep(0, n)
    ui <- rep(0, n)
    for (i in 1:n) {
        x <- (a * x) %% m
        xi[i] <- x
        ui[i] <- x / m
    }
    return(data.frame(xi, ui))
}

congruential_gen(182, 11, 119, 11)

congruential_mixed_gen <- function(x0, a, m, c, n) {
    x <- x0
    xi <- rep(0, n)
    ui <- rep(0, n)
    for (i in 1:n) {
        x <- (a * x + c) %% m
        xi[i] <- x
        ui[i] <- x / m
    }
    return(data.frame(xi, ui))
}

congruential_mixed_gen(2, 1, 7, 5, 10)

no3 <- function(z0, a, m, c, n) {
    x <- z0
    xi <- rep(0, n)
    ui <- rep(0, n)
    for (i in 1:n) {
        x <- (a * x + c) %% m
        xi[i] <- x
        ui[i] <- x / m
    }
    xbinom <- seq(0, 10)
    binom <- pbinom(0:10, 10, 0.65) # CDF Binomial
    random_i <- rep(0, n)
    for(i in 1:n) {
        for(j in 1:11) {
            if(ui[i] <= binom[j]) {
                cat(ui[i], binom[j], j, "\n")
                random_i[i] <- j - 1
                break
            }
        }
    }

    return(data.frame(xi, ui, random_i))
}
no3(1237, 29, 512, 71, 10)

no3lat <- function(n) {
    u1 <- runif(1)
    u2 <- runif(1)
    random_i <- rep(0, n)

    for(i in 1:n) {
        while(u2 > 4*u1*(1-u1)) {
            u1 <- runif(1)
            u2 <- runif(1)
        }
        if(u2 <= 4*u1*(1-u1)) {
            random_i[i] <- u1
        }
        u1 <- runif(1)
        u2 <- runif(1)
    }
    return(data.frame(random_i))
}

no3lat(5)

no5lat <- function (n, mu, sigma) {
    u <- runif(n)
    z <- sigma * sqrt(-2 * log(u)) + mu
    return(z^2)
}

rnorm

no5lat(5, 2, 1)

nc1 <- function(n) {
    # Normal Baku ke Chi-Square(1)
    z <- rnorm(n)
    return(z^2)
}

nc1(5)

cfmn <- function(m, n, size) {
    # U distribusi ChiSquare(m)
    # V distribusi ChiSquare(n)
    # F distribusi F(m, n)
    u <- rchisq(size, m)
    v <- rchisq(size, n)
    f <- ((v/m)/(u/n))
    return(f)
}

nctn <- function(n, size) {
    z <- rnorm(size)
    v <- rchisq(size, n)
    t <- z/sqrt(v/n)
    return(t)
}

unifnorm <- function(n) {
    # U,V ~ Unif(0, 1)
    # Z1 ~ N(0, 1)
    # Z2 ~ N(0, 2)
    u <- runif(n)
    v <- runif(n)
    z1 <- sqrt(-2 * log(u)) * cos(2 * pi * v)
    z2 <- sqrt(-2 * log(v)) * sin(2 * pi * u)
    return(data.frame(z1, z2))
}
unifnorm(10)

twogammabeta <- function(r, s, lambda, size) {
    # u ~ Gamma(r, lambda)
    # v ~ Gamma(s, lambda)
    # x ~ Beta(r, s)
    u <- rgamma(size, shape = r, rate = lambda)
    v <- rgamma(size, shape = s, rate = lambda)
    x <- u / (u + v)
    return(x)
}

uniflog <- function(n, theta) {
    # U ~ Unif(0, 1)
    # V ~ Unif(0, 1)
    # X ~ Logaritmic(theta)
    u <- runif(n)
    v <- runif(n)
    x <- floor(1 + log(v)/log(1-(1-theta)^u))
}

no1 <- function(z0, a, m, n, theta) {
    # RNG and Inverse Transform
    x <- z0
    xi <- rep(0, n)
    ui <- rep(0, n)
    zi <- rep(0, n)
    for (i in 1:n) {
        x <- (a * x) %% m
        xi[i] <- x
        ui[i] <- x / m
        zi[i] <- theta / (sqrt(1 - ui[i]))
    }
    return(data.frame(xi, ui, zi))
}

no1(1357, 67, 512, 8, 4)

no2 <- function(z0, a, m, c, n, k, lambda) {
    x <- z0
    xi <- rep(0, k)
    ui <- rep(0, k)
    zi <- rep(0, k)
    random_i <- rep(0, n)
    for (i in 1:(n*k)) {
        x <- (a * x + c) %% m
        xi[i] <- x
        ui[i] <- x / m
        zi[i] <- log(1 - ui[i]) / (-lambda)
        if(i %% k == 0) {
            random_i[i/k] <- sum(zi[(i-k+1):i])
        }
    }
    return(data.frame(random_i))
}
no2(12357, 29, 2048, 0, 1, 9, 12)
no2(137, 31, 512, 91, 10, 7, 3)
