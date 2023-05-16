congruential_mixed <- function(z0, a, m, c, n) {
  z <- rep(0, n)
  u <- rep(0, n)
  for(i in 1:n) {
    if(i == 1) {
      z[i] <- (a * z0 + c) %% m
    } else {
      z[i] <- (a * z[i-1] + c) %% m
    }
    u[i] <- z[i] / m
  }
  return(u)
}

congruential_mixed(16, 26, 99, 20, 12)

simulasi <- function(data, test, random_method, x0, a, m, c) {
    # data = data yang akan dihitung probabilitasnya
    # return = data frame dengan kolom data, freq, prob, dan cum_prob
    mape <- function(act, pred) {
      return(abs((act - pred) / act) * 100)
    }

    # 0 = RUNIF
    # 1 = MULTIPLICATIVE CONGRUENTIAL

    if(length(data) != length(test)) {
        cat("Data tidak sama panjang!\n")
        return()
    }

    prob <- data / sum(data)
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
    random_dat <- rep(0, length(data))
    random_result <- rep(0, length(data))

    # Randomisasi
    if(random_method == 1) {
      # Randomisasi runif
      random_dat <- runif(length(data))
    } else if(random_method == 2) {
      # Congruential method
      random_dat <- congruential_mixed(x0, a, m, c, length(data))
    }
    
    for(i in 1:length(data)){
      for(j in 1:length(cum_prob)){
        if(random_dat[i] <= tag_u[j]){
          random_result[i] <- data[j]
          break
        }
      }
    }
    mape <- mape(random_result, test)
    akurasi <- 100 - mape

    result <- data.frame(data, prob, tag_l, tag_u, random_dat, random_result, test, mape, akurasi)
    names(result) <- c("Data Tahun", "Probabilitas", "Batas Bawah", "Batas Atas", "Random", "Hasil Prediksi Tahun Depan", "Data Tahun Depan", "MAPE", "Akurasi")
    
    # print(result)
    View(result)
    
    akurasi_avg <- mean(100 - mape)
    
    cat("\nRata-rata Akurasi Prediksi:", akurasi_avg, "%\n")
    if(random_method == 1) {
      cat("Metode Randomisasi: Runif\n")
    } else if(random_method == 2) {
      if(c == 0) {
        cat("Metode Randomisasi: Multiplicative Congruential Random Number Generator\n")
      } else {
        cat("Metode Randomisasi: Mixed Congruential Random Number Generator\n")
      }
    }
}

input_data <- function() {
  cat("Simulasi Monte Carlo\n")
  cat(">> Masukkan data tahun ini\n")
  x <- scan(what = double(), nmax = 12)
  cat(">> Masukkan data tahun depan\n")
  y <- scan(what = double(), nmax = length(x))
  choice_opt <- c(1, 2)
  choice <- 0
  while(choice %in% choice_opt == FALSE) {
    cat(">> Pilih metode randomisasi\n")
    cat("1. Random Uniform(0, 1)\n")
    cat("2. Multiplicative Congruential Random Numbers Generator\n")
    choice <- readline(prompt = "Pilihan: ")
  }
  if(choice == 1) {
    simulasi(x, y, choice, 0, 0, 0, 0)
  } else if(choice == 2) {
    x0 <- as.integer(readline(">> Masukkan nilai z0: "))
    a <- as.integer(readline(">> Masukkan nilai a: "))
    m <- as.integer(readline(">> Masukkan nilai m: "))
    c <- as.integer(readline(">> Masukkan nilai c: "))
    simulasi(x, y, choice, x0, a, m, c)
  }
}
input_data()
