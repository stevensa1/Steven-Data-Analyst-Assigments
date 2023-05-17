# Memuat paket
library(fitdistrplus)
library(MASS)

# Fungsi untuk mencoba menyesuaikan beberapa distribusi
fit_distributions <- function(data) {
  # Mendefinisikan daftar distribusi untuk dicoba
  dist_names <- c("norm")
  
  # Inisialisasi daftar untuk menyimpan hasil
  fit_results <- list()
  
  # Loop melalui setiap distribusi dan mencoba menyesuaikannya dengan data
  for (dist_name in dist_names) {
    fit_result <- suppressWarnings(fitdist(data, dist_name, method = "mle"))
    fit_results[[dist_name]] <- fit_result
  }
  
  # Mengembalikan hasil
  return(fit_results)
}

# Fungsi untuk memilih distribusi terbaik berdasarkan uji kebaikan sesuaian chi-square
select_best_distribution <- function(fit_results) {
  # Inisialisasi nilai chi-square terkecil dengan nilai maksimum
  smallest_chisq <- Inf
  
  # Loop melalui setiap hasil dan mencari yang memiliki nilai chi-square terkecil
  for (fit_result in fit_results) {
    chisq <- gofstat(fit_result)$chisq
    if (chisq < smallest_chisq) {
      smallest_chisq <- chisq
      best_fit <- fit_result
    }
  }
  
  # Mengembalikan distribusi terbaik
  return(best_fit)
}

# Uji fungsi dengan data Anda
data <- rnorm(100) # ganti dengan data Anda
fit_results <- fit_distributions(data)
best_fit <- select_best_distribution(fit_results)

print(fit_results)
print(best_fit)

mydata <- scan(what = double())

fit_results <- fit_distributions(mydata)
best_fit <- select_best_distribution(fit_results)

print(fit_results)
print(best_fit)
