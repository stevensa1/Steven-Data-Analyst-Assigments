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
win.graph()
pacf(flood.occurence)
# Lag banjir
# Lag signifikan ada pada lag 1 dan 5

# Membuat Data Frame dengan lag
df <- data.frame(flood.occurence, dplyr::lag(flood.occurence, n = 1), dplyr::lag(flood.occurence, n = 5))
colnames(df) <- c("yt", "yt-1", "yt-5")
df.train <- df[6:40,]

df.test <- df[-(6:40),]
df.test <- df[41:50,]

# Pembuatan model SVM ####
svm.linear <- svm(yt ~ ., data = df.train, kernel = "linear")
svm.radial <- svm(yt ~ ., data= df.train, kernel = "radial")
summary(svm.linear)
summary(svm.radial)


# Membangkitkan hasil prediksi berdasarkan nilai x dari model
fit.svm.linear.train <- predict(svm.linear, df.train)
fit.svm.radial.train <- predict(svm.radial, df.train)

# Root Mean Square Error & MAPE ####
rmse <- function(err) {
  rootmse <- sqrt(mean(err^2))
  return(rootmse)
}

mape <- function(actual_values, predicted_values) {
  n <- length(actual_values)
  mape <- sum(abs((actual_values - predicted_values) / actual_values)) / n * 100
  return(mape)
}

residual.train.svm.linear <- df.train$yt - fit.svm.linear.train
residual.train.svm.radial <- df.train$yt - fit.svm.radial.train

rmse.train.svm.linear <- rmse(residual.train.svm.linear)
rmse.train.svm.radial <- rmse(residual.train.svm.radial)

(mape.train.svm.linear <- mape(df.train$yt, fit.svm.linear.train))
(mape.train.svm.radial <- mape(df.train$yt, fit.svm.radial.train))

# Plot Fit vs. Train ####
win.graph()
par(mfrow=c(2,1))
plot(df.train$yt, type = 'l', main = "Train Vs. Fit SVR Linear", lwd = 3)
lines(fit.svm.linear.train, col='red', lty="dashed", lwd = 3)
legend("topright", legend = c("Train","SVR Linear"), col = c("black","red"), lty = c(1,2), lwd = 3)

plot(df.train$yt, type = 'l', main = "Train Vs. Fit SVR Radial", lwd = 3)
lines(fit.svm.radial.train, col='red', lty="dashed", lwd = 3)
legend("topright", legend = c("Train","SVR Radial"), col = c("black","red"), lty = c(1,2), lwd = 3)

# Testing Out-Sample ####
fit.svm.linear.test <- predict(svm.linear, df.test)
fit.svm.radial.test <- predict(svm.radial, df.test)

residual.test.svm.linear <- df.test$yt - fit.svm.linear.test
residual.test.svm.radial <- df.test$yt - fit.svm.radial.test

rmse.test.svm.linear <- rmse(residual.test.svm.linear)
rmse.test.svm.radial <- rmse(residual.test.svm.radial)

mape.test.svm.linear <- mape(df.test$yt, fit.svm.linear.test)
mape.test.svm.radial <- mape(df.test$yt, fit.svm.radial.test)

# Plot Prediksi
win.graph()
par(mfrow=c(2,1))
plot(df.test$yt, ylim = c(0, 200), type = 'l', main = "Test Vs. Fit SVR Linear", lwd = 3)
lines(fit.svm.linear.test, col='red', lty="dashed", lwd = 3)
legend("topright", legend = c("Test","SVR Linear"), col = c("black","red"), lty = c(1,2), lwd = 3)

plot(df.test$yt, ylim = c(0, 200), type = 'l', main = "Test Vs. Fit SVR Radial", lwd = 3)
lines(fit.svm.radial.test, col='red', lty="dashed", lwd = 3)
legend("topright", legend = c("Test","SVR Radial"), col = c("black","red"), lty = c(1,2), lwd = 3)


# Tuning ####
# Loose Grid
svm.tune.linear <- tune(svm, yt ~ .,  data = df.train, kernel = "linear", ranges = list(epsilon = seq(0, 1, 0.1), cost = 2^(1:9)))
svm.tune.radial <- tune(svm, yt ~ .,  data = df.train, kernel = "radial", ranges = list(epsilon = seq(0, 1, 0.1), cost = 2^(1:9)))

print(svm.tune.linear)
print(svm.tune.radial)

win.graph()
plot(svm.tune.linear, main = "Tuning Result SVR Linear")

win.graph()
plot(svm.tune.radial, main = "Tuning Result SVR Radial")

# Finer Grid
svm.tune.linear.2 <- tune(svm, yt ~ .,  data = df.train, kernel = "linear", ranges = list(epsilon = seq(0, 0.2, 0.01), cost = 2^(5:9)))
svm.tune.radial.2 <- tune(svm, yt ~ .,  data = df.train, kernel = "radial", ranges = list(epsilon = seq(0.8, 1, 0.01), cost = 2^(5:9)))

print(svm.tune.linear.2)
print(svm.tune.radial.2)

win.graph()
plot(svm.tune.linear.2, main = "Tuning Result SVR Linear 2")

win.graph()
plot(svm.tune.radial.2, main = "Tuning Result SVR Radial 2")

# Final Model
svm.linear.tuned <- svm.tune.linear.2$best.model
svm.radial.tuned <- svm.tune.radial.2$best.model

# Train Data for Tuned SVR
fit.svm.tuned.linear.train <- predict(svm.linear.tuned, df.train)
fit.svm.tuned.radial.train <- predict(svm.radial.tuned, df.train)
residual.train.svm.tuned.linear <- df.train$yt - fit.svm.tuned.linear.train
residual.train.svm.tuned.radial <- df.train$yt - fit.svm.tuned.radial.train

(rmse.train.svm.tuned.linear <- rmse(residual.train.svm.tuned.linear))
(rmse.train.svm.tuned.radial <- rmse(residual.train.svm.tuned.radial))

(mape.train.svm.tuned.linear <- mape(df.train$yt, fit.svm.tuned.linear.train))
(mape.train.svm.tuned.radial <- mape(df.train$yt, fit.svm.tuned.radial.train))

win.graph()
par(mfrow=c(2,1))
plot(df.train$yt, type = 'l', main = "Train Vs. Fit SVR Tuned Linear", lwd = 3)
lines(fit.svm.tuned.linear.train, col='red', lty="dashed", lwd = 3)
legend("topright", legend = c("Train","SVR Linear"), col = c("black","red"), lty = c(1,2), lwd = 3)

plot(df.train$yt, type = 'l', main = "Train Vs. Fit SVR Tuned Radial", lwd = 3)
lines(fit.svm.tuned.radial.train, col='red', lty="dashed", lwd = 3)
legend("topright", legend = c("Train","SVR Radial"), col = c("black","red"), lty = c(1,2), lwd = 3)

# Test Data for Tuned SVR
fit.svm.tuned.linear.test <- predict(svm.linear.tuned, df.test)
fit.svm.tuned.radial.test <- predict(svm.radial.tuned, df.test)
residual.test.svm.tuned.linear <- df.test$yt - fit.svm.tuned.linear.test
residual.test.svm.tuned.radial <- df.test$yt - fit.svm.tuned.radial.test

(rmse.test.svm.tuned.linear <- rmse(residual.test.svm.tuned.linear))
(rmse.test.svm.tuned.radial <- rmse(residual.test.svm.tuned.radial))

(mape.test.svm.tuned.linear <- mape(df.test$yt, fit.svm.tuned.linear.test))
(mape.test.svm.tuned.radial <- mape(df.test$yt, fit.svm.tuned.radial.test))

win.graph()
par(mfrow=c(2,1))
plot(df.test$yt, ylim = c(0, 200), type = 'l', main = "test Vs. Fit SVR Tuned Linear", lwd = 3)
lines(fit.svm.tuned.linear.test, col='red', lty="dashed", lwd = 3)
legend("topright", legend = c("test","SVR Linear"), col = c("black","red"), lty = c(1,2), lwd = 3)

plot(df.test$yt, ylim = c(0, 200), type = 'l', main = "test Vs. Fit SVR Tuned Radial", lwd = 3)
lines(fit.svm.tuned.radial.test, col='red', lty="dashed", lwd = 3)
legend("topright", legend = c("test","SVR Radial"), col = c("black","red"), lty = c(1,2), lwd = 3)