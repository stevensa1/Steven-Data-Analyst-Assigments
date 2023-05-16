simulasi <- function(data) {
    # data = data yang akan dihitung probabilitasnya
    # return = data frame dengan kolom data, freq, prob, dan cum_prob
    mape <- function(act, pred) {
      return(abs((act - pred) / act) * 100)
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
    random_dat <- runif(length(data))
    random_result <- rep(0, length(data))
    
    for(i in 1:length(data)){
      for(j in 1:length(cum_prob)){
        if(random_dat[i] <= tag_u[j]){
          random_result[i] <- data[j]
          break
        }
      }
    }
    cat("Hasil\n")
    mape <- mape(data, random_result)
    akurasi <- 100 - mape

    result <- data.frame(data, prob, tag_l, tag_u, random_dat, random_result, mape, akurasi)
    names(result) <- c("Data Tahun", "Probabilitas", "Batas Bawah", "Batas Atas", "Random", "Hasil Prediksi Tahun Depan", "MAPE", "Akurasi")
    
    # print(result)
    View(result)
    
    akurasi_avg <- mean(100 - mape)
    
    cat("\nRata-rata Akurasi Prediksi:", akurasi_avg, "%\n")
}

real_thn <- c(6830, 8163, 8727, 8825, 8742, 7067, 8693, 9375, 10351, 8547, 8543, 7977)

simulasi(real_thn)

input_data <- function() {
  cat("Simulasi Monte Carlo\n")
  cat("Masukkan data tahunan\n")
  x = scan(what = double(), nmax = 12)
  simulasi(x)
}
input_data()
