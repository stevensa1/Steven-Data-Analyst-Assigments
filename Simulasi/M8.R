# Monte Carlo

monte_carlo <- function(data, freq, n) {
    # data = data yang akan dihitung probabilitasnya
    # freq = frekuensi masing-masing data
    # return = data frame dengan kolom data, freq, prob, dan cum_prob
    prob <- freq / sum(freq)
    cum_prob <- cumsum(prob)

    tag_u <- rep(0, length(cum_prob))
    tag_l <- rep(0, length(cum_prob))

    # Membuat batas
    for (i in 1:length(cum_prob)) {
      if (i == 1) {
        tag_l[i] <- 0
        tag_u[i] <- cum_prob[i]
      } else {
        tag_l[i] <- cum_prob[i - 1] + 0.01
        tag_u[i] <- cum_prob[i]
      }
    }
    
    # Prediksi
    random_dat <- runif(n)
    random_result <- rep(0, n)
    
    for(i in 1:n){
      for(j in 1:length(cum_prob)){
        if(random_dat[i] <= tag_u[j]){
          random_result[i] <- data[j]
          break
        }
      }
    }
    cat("Data dan Interval\n")
    print(data.frame(data, freq, prob, tag_l, tag_u))
    cat("Hasil Random\n")
    print(data.frame(random_dat, random_result))
}

dDay <- c(4, 5, 6, 7, 8, 9)
dFreq <- c(5, 10, 15, 30, 25, 15)

monte_carlo(dDay, dFreq, 10)


monte_carlo2 <- function(data, prob, n) {
    # data = data yang akan dihitung probabilitasnya
    # return = data frame dengan kolom data, freq, prob, dan cum_prob
    cum_prob <- cumsum(prob)

    tag_u <- rep(0, length(cum_prob))
    tag_l <- rep(0, length(cum_prob))

    # Membuat batas
    for (i in 1:length(cum_prob)) {
      if (i == 1) {
        tag_l[i] <- 0
        tag_u[i] <- cum_prob[i]
      } else {
        tag_l[i] <- cum_prob[i - 1] + 0.01
        tag_u[i] <- cum_prob[i]
      }
    }
    
    # Prediksi
    random_dat <- runif(n)
    random_result <- rep(0, n)
    
    for(i in 1:n){
      for(j in 1:length(cum_prob)){
        if(random_dat[i] <= tag_u[j]){
          random_result[i] <- data[j]
          break
        }
      }
    }
    return(random_result)
}

data_A <- seq(10, 13)
prob_A <- c(0.25, 0.25, 0.25, 0.25)
data_B <- seq(17, 22)
prob_B <- c(0.07, 0.14, 0.23, 0.38, 0.12, 0.06)

no2 <- function(a, ap, b, bp, n) {
    res_a <- monte_carlo2(a, ap, n)
    res_b <- monte_carlo2(b, bp, n)
    res_c <- res_a + res_b
    avg_c <- mean(res_c)
    var_c <- var(res_c)
    sd_c <- sd(res_c)
    print(data.frame(res_a, res_b, res_c))
    print(data.frame(avg_c, var_c, sd_c))
    # return(data.frame(res_a, res_b, res_c))
}

no2(data_A, prob_A, data_B, prob_B, 10)
