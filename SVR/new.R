# Load Packages ####
require(pacman)
pacman::p_load(rio, tseries, forecast, e1071, dplyr)

# Import Data ####
banjir <- import("C:/Users/Steven/Documents/RStudio/Support Vector Regression/Data Banjir 2019-2023.xlsx")
#longsor <- import("Data Tanah Longsor 2019-2023.xlsx")

# Cek tipe data ####
str(banjir)
typeof(banjir)
class(banjir)

# Definisi Data ####
flood.occurence <- banjir[1:50, 3]
ts.plot(flood.occurence, main = "Kejadian Banjir 2019 - 2023", ylab = "Jumlah Banjir", xlab = "Waktu")
plot(flood.occurence,main="Kejadian Banjir 2019 - 2023", ylab="Jumlah Kejadian",xlab="Waktu", pcy = 3)
lines(flood.occurence[1:50],col="red", lwd = 3)
lines(flood.occurence[1:40],col="blue", lwd = 3)
legend("topright",col=c("blue","red"),lty=1, lwd = 3, legend=c("Train","Test"))

# Cek non-linearitas ####
terasvirta.test(seq(1:50), flood.occurence) # H0 : Model regresi linear memadai (kesalahan residual homoskedastik dan tidak ada nonlinieritas pada model)

# Penentuan Lag ####
pacf(train) # Lag banjir
# Lag signifikan ada pada lag 1 dan 5

# Membuat Data Frame dengan lag
df <- data.frame(flood.occurence, dplyr::lag(flood.occurence, n = 1), dplyr::lag(flood.occurence, n = 5))
colnames(df) <- c("yt", "yt-1", "yt-5")
df.train <- df[6:40,]
df.test <- df[41:50,]
View(df)

# Pemodelan SVM Linear ####

# Pembuatan model SVM ####
svm.linear <- svm(yt ~ ., data = df.train, kernel = "linear")
svm.radial <- svm(yt ~ ., data= df.train, kernel = "radial")

summary(svm.linear)
summary(svm.radial)


# Membangkitkan hasil prediksi berdasarkan prediktor ####
fit.svm.linear.train <- predict(svm.linear, df.train)
fit.svm.radial.train <- predict(svm.radial, df.train)


# Root Mean Square Error & MAPE ####
rmse <- function(residual) {
  rootmse <- sqrt(mean(residual^2))
  return(rootmse)
}

mape <- function(actual_values, predicted_values) {
  n <- length(actual_values)
  mape <- sum(abs((actual_values - predicted_values) / actual_values)) / n * 100
  return(mape)
}

residual.train.svm.linear <- df.train$yt - fit.svm.linear.train
residual.train.svm.radial <- df.train$yt - fit.svm.radial.train

(rmse.train.svm.linear <- rmse(residual.train.svm.linear))
(rmse.train.svm.radial <- rmse(residual.train.svm.radial))

(mape.train.svm.linear <- mape(df.train$yt, fit.svm.linear.train))
(mape.train.svm.radial <- mape(df.train$yt, fit.svm.radial.train))

# Plot Fit vs. Train ####
win.graph()
par(mfrow=c(2,1))
plot(df.train$yt, type = 'l', main = "Train Vs. Fit SVR Linear", lwd = 3)
lines(fit.svm.linear.train, col='red', lty="dashed", lwd = 3)
legend("topright", legend = c("Train","SVR Linear"), col = c("black","red"), lty = c(1,2))

plot(df.train$yt, type = 'l', main = "Train Vs. Fit SVR Radial", lwd = 3)
lines(fit.svm.radial.train, col='red', lty="dashed", lwd = 3)
legend("topright", legend = c("Train","SVR Radial"), col = c("black","red"), lty = c(1,2))

# Testing Out-Sample ####
fit.svm.linear.test <- predict(svm.linear, df.test)
fit.svm.radial.test <- predict(svm.radial, df.test)

residual.test.svm.linear <- df.test$yt - fit.svm.linear.test
residual.test.svm.radial <- df.test$yt - fit.svm.radial.test

(rmse.test.svm.linear <- rmse(residual.test.svm.linear))
(rmse.test.svm.radial <- rmse(residual.test.svm.radial))

(mape.test.svm.linear <- mape(df.test$yt, fit.svm.linear.test))
(mape.test.svm.radial <- mape(df.test$yt, fit.svm.radial.test))

# Plot Prediksi
win.graph()
par(mfrow=c(2,1))
plot(df.test$yt, type = 'l', main = "Test Vs. Fit SVR Linear")
lines(fit.svm.linear.test, col='red', lty="dashed")
legend("topright", legend = c("Test","SVR Linear"), col = c("black","red"), lty = c(1,1))

plot(df.test$yt, type = 'l', main = "Test Vs. Fit SVR Radial")
lines(fit.svm.radial.test, col='red', lty="dashed")
legend("topright", legend = c("Test","SVR Radial"), col = c("black","red"), lty = c(1,1))


# Tuning ####
# Loose Grid
tuneResult <- tune(svm, yt ~ .,  data = df.train, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
print(tuneResult)
plot(tuneResult)

# Finer Grid
tuneResult2 <- tune(svm, yt ~ ., data = df.train,  ranges = list(epsilon = 0.8, cost = 2^seq(2, 8, 1)))
print(tuneResult2)
plot(tuneResult2)

# Tuned Model Result ####
tunedModel <- tuneResult2$best.model
summary(tunedModel)

# Fitting Tuned Model to Train ####
fit.tuned.svm.radial.train <- predict(tunedModel, df.train)
residual.train.tuned.svm.radial <- df.train$yt - fit.tuned.svm.radial.train
(rmse.train.tuned.svm.radial <- rmse(residual.train.tuned.svm.radial))
(mape.train.fit.svm.radial <- mape(df.train$yt, fit.tuned.svm.radial.train))

# Plot train
win.graph()
plot(df.train$yt, type = 'l', main = "Test Vs. Fit Tuned SVR Radial")
lines(fit.tuned.svm.radial.train, col='red', lty="dashed")
legend("topright", legend = c("Test","Tuned SVR Radial"), col = c("black","red"), lty = c(1,1))

# Fitting Tuned Model to Test ####
fit.tuned.svm.radial.test <- predict(tunedModel, df.test)
residual.test.tuned.svm.radial <- df.test$yt - fit.tuned.svm.radial.test
(rmse.test.tuned.svm.radial <- rmse(residual.test.tuned.svm.radial))
(mape.test.fit.svm.radial <- mape(df.test$yt, fit.tuned.svm.radial.test))

# Plot test
plot(df.test$yt, type = 'l', main = "Test Vs. Fit Tuned SVR Radial")
lines(fit.tuned.svm.radial.test, col='red', lty="dashed")
legend("topright", legend = c("Test","Tuned SVR Radial"), col = c("black","red"), lty = c(1,1))

# Polynomial Kernel ####
svm.polynomial <- svm(yt ~ ., data = df.train, kernel = "polynomial", degree = 3)
polynomial.tune <- tune(svm, yt ~ ., data = df.train, kernel = "polynomial", ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
best.polynomial <- polynomial.tune$best.model
summary(svm.polynomial)
fit.svm.polynomial.train <- predict(best.polynomial, df.train)
fit.svm.polynomial.test <- predict(best.polynomial, df.test)
mape(df.train$yt, fit.svm.polynomial.train)
mape(df.test$yt, fit.svm.polynomial.test)

# Uji Coba
modelUJi <- svm(yt ~ df.train$`yt-1`, data = df.train)
summary(modelUJi)
tesUji <- predict(modelUJi, df.train)

mape(df.train$yt, tesUji)

plot(df.train$yt, type = 'l', main = "Train Vs. Fit SVR Radial")
lines(tesUji, col='red', lty="dashed", lwd = 3)
legend("topright", legend = c("Train","Tuned SVR Radial"), col = c("black","red"), lty = c(1,2), lwd = 3)


tesUjiTest <- predict(modelUJi, df.test)
mape(df.test$yt, tesUjiTest[1:10])
