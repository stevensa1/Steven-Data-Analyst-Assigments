# Load Package
require(pacman)
pacman::p_load(rio, tseries, forecast, e1071)

#DATA
data <- import("assets/data/Data Temperatur Udara 2022 - 2023 Lag.xlsx")
y=data[1:100,2]
x=data[1:100,1]
dataawal=cbind(x,y)
colnames(dataawal) = c("X","Y")

#PLOT DATA
win.graph()
ts.plot(data[,2])
plot(data[,2],main="Temperatur Udara Kota Bandung",ylab="Suhu Udara",xlab="Time", lwd = 3)
lines(data[1:100, 2],col="red", lwd = 3)
lines(data[1:79, 2],col="blue", lwd = 3)
legend("bottomright",col=c("blue","red"),lty=1,legend=c("Training","Testing"))

#HETEROSKEDASTISITAS DAN LINEARITAS DATA
white.test(dataawal[,2],dataawal[,1]) #uji heteroskedastisitas
terasvirta.test(dataawal[,2],dataawal[,1]) #uji nonlinearitas

win.graph()
plot(dataawal, type = "l",col="black")
plot(dataawal, type = "p",col="black")
abline(lm(y~x))

# PENENTUAN LAG
win.graph()
pacf(data$y)
# DIPEROLEH LAG 1

# INPUT DATA LAG
datalag <- import("assets/data/Data Temperatur Udara 2022 - 2023 Lag.xlsx")
head(datalag)
View(datalag)


# PEMBAGIAN DATA TESTING DAN DATA TRAINING
datasvr <- datalag[2:100, 2:3]
training <- datasvr[1:79,]
testing <- datasvr[-1:-79,]
tail(training)
head(testing)

# PEMODELAN AWAL
modeltrain <- svm(formula = y ~ ., data = training, kernel = "radial")
summary(modeltrain)

prediksisvrtraining = predict(modeltrain, training)
prediksisvrtraining
View(prediksisvrtraining)

win.graph()
plot(training$y, type = "l",col="black")
lines(prediksisvrtraining, col = "red",pch=4)
legend("topleft", legend = c("Data Training","SVR"), col = c("black","red"), lty = c(1,1))

# NILAI MAPE DAN RMSE MODEL AWAL
rmse <- function(error) {
  sqrt(mean(error^2))
}
#
resTrain<-training$y-prediksisvrtraining
RMSEtrain<-rmse(resTrain)


cat("\nNilai RMSE adalah = ",RMSEtrain,"\n")
mapetraining=mean(abs((training$y-prediksisvrtraining)/training$y)) * 100
cat("\nNilai Mape adalah = ",round(mapetraining,2),"%\n")

# PLOT FIT VS TRAIN
win.graph()
par(mfrow=c(2,1))
ts.plot(training$y)
ts.plot(prediksisvrtraining)

# TUNING PARAMETER
# TUNING PARAMETER LOOSE GRID
trainingframe = training
tuneResult <- tune(svm, y~.,  data = trainingframe,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult)
# menggambarkan hasil tuning
win.graph()
plot(tuneResult)

# TUNING PARAMETER FINER GRID
tuneResult2 <- tune(svm, y~.,  data = training,
                    ranges = list(epsilon = 0.2, cost = 2^seq(2,4,1))
)
print(tuneResult2)
tunedModel <- tuneResult2$best.model
tunedModel

# PEMODELAN DATA TRAIN DENGAN PARAMATER TERBAIK
tunedModelTrain <- predict(tunedModel, training)
tunedModelTrain
errorTrain <- training$y-tunedModelTrain
tunedModelRMSETrain <- rmse(errorTrain);tunedModelRMSETrain 
prediksitrainsvr = matrix(tunedModelTrain,nrow=length(tunedModelTrain), ncol=1)
prediksitrainsvr 

# GRAFIK TUNING
win.graph()
plot(training$y, type = "l",col="black")
lines(tunedModelTrain, col = "red",pch=4)
legend("topleft",
       legend = c("Data Training","SVR"),
       col = c("black","red"),
       lty = c(1,1))

# NILAI RMSE DAN MAPE TUNING
resTrain<-training$y-tunedModelTrain
RMSEtrain<-rmse(resTrain)

cat("\nNilai RMSE adalah = ",RMSEtrain,"\n")
mapetraining=mean(abs((training$y-tunedModelTrain)/training$y)) * 100
cat("\nNilai Mape adalah = ",round(mapetraining,2),"%\n")

# PLOT FIT VS TRAIN
win.graph()
par(mfrow=c(2,1))
ts.plot(training$y)
ts.plot(tunedModelTrain)

# PEMODELAN DATA TESTING DENGAN PARAMETER TERBAIK
tunedModelTest <- predict(tunedModel, testing)
tunedModelTest
errorTest <- testing$y-tunedModelTest
tunedModelRMSETest <- rmse(errorTest);tunedModelRMSETest
prediksitestsvr = matrix(tunedModelTest,nrow=length(tunedModelTest), ncol=1)
prediksitestsvr 

# GRAFIK TUNING
win.graph()
plot(testing$y, type = "l",col="black")
lines(tunedModelTest, col = "red",pch=4)
legend("topleft",
       legend = c("Data Testing","SVR"),
       col = c("black","red"),
       lty = c(1,1))

# NILAI RMSE DAN MAPE TUNING
resTest<-testing$y-tunedModelTest
RMSEtest<-rmse(resTest)

cat("\nNilai RMSE adalah = ",RMSEtest,"\n")
mapetest=mean(abs((testing$y-tunedModelTest)/testing$y)) * 100
hasil=cbind(testing$y,tunedModelTest)
colnames(hasil) = c("Data Testing","Data Prediksi")
print(hasil)
cat("\nNilai Mape adalah = ",round(mapetest,2),"%\n")

# PLOT FIT VS TEST
win.graph()
par(mfrow=c(2,1))
ts.plot(testing$y)
ts.plot(tunedModelTest)

# TUNING PARAMETER FINER GRID
tuneResult2 <- tune(svm, y~.,  data = training,
                    ranges = list(epsilon = seq(0.5,1,0.1), cost = (2^1:4))
)
tuneResult2