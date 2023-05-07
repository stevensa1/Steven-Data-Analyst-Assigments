# DATA IN SAMPEL
pacman::p_load(rio)

data<-import(file.choose())

#View(data)
datain = data[1:80,]
#View(datain)
dataout=data[81:100,]
#View(dataout)

GCV <- function(data)
{
k <- as.character(readline(" Kernel (G/C/T/Q/E) : "))
if(k == 'G') kernel <- function(u) { exp(-0.5*(u^2))/sqrt(2*pi) }
if(k == 'C') kernel <- function(u) { (pi/4)*cos((pi/2)*u) }
if(k == 'T') kernel <- function(u) { (35/32)*(1-u^2)^3 }
if(k == 'Q') kernel <- function(u) { (15/16)*(1-u^2)^2 }
if(k == 'E') kernel <- function(u) { (3/4)*(1-u^2)^2 }
y <- as.vector(data[,2]) 
x <- as.vector(data[,1])
bb <- as.numeric(readline(" Batas Bawah Bandwith = "))
ba <- as.numeric(readline(" Batas Atas Bandwith = "))
inc <- as.numeric(readline(" Increment = "))
vh <- seq(bb,ba,inc)
nvh <- length(vh)
n <- length(y)
GCV <- rep(0,nvh)
for (k in 1:nvh)
{
u <- matrix(0,ncol=n,nrow=n)
for(i in 1:n) for(j in 1:n) 
{
u[i,j] <- (x[i] - x[j])/vh[k]
jumlah <- matrix(0,ncol=1,nrow=n)
}
for(i in 1:n)
{
  for(j in 1:n)
  {
    jumlah[i,1] <- jumlah[i,1] + kernel(u[i,j])
  }
}
H<-matrix(0,ncol=n,nrow=n)
for(i in 1:n)
{
  for(j in 1:n) 
  {
    H[i,j]<-kernel(u[i,j])/jumlah[i,1]
  }
}
mhlamda <- H%*%y
atas<-t(y-mhlamda)%*%(y-mhlamda)/n; bawah<-(1-(sum(diag(H)))/n)^2
GCV[k]<-atas/bawah
}
s<-matrix(c(vh,GCV),nvh,2)
out <- data.frame(Bandwidth=vh,GCV=GCV)
op <- which.min(out[,2])
print(out)
cat(" BERDASARKAN TABEL DIATAS DIKETAHUI:\n")
cat(" Bandwidth Optimal =",out[op,1],"Nilai GCV Minimum =",out[op,2],"\n")
plot(vh,GCV,type="l",col="red",lwd=2)
return(out[op,1])
}
GCV(datain)

est <- function(data)
{
k <- as.character(readline(" Kernel (G/C/T/Q/E) : "))
if(k == 'G') kernel <- function(u) { exp(-0.5*(u^2))/sqrt(2*pi) }
if(k == 'C') kernel <- function(u) { (pi/4)*cos((pi/2)*u) }
if(k == 'T') kernel <- function(u) { (35/32)*(1-u^2)^3 }
if(k == 'Q') kernel <- function(u) { (15/16)*(1-u^2)^2 }
if(k == 'E') kernel <- function(u) { (3/4)*(1-u^2)^2 }
dataurut <- data[order(data[,1]),1:2]
h <- as.numeric(readline("Bandiwth Optimal = "))
y <- dataurut[,2]
x <- dataurut[,1]
n <- length(y)
u <- matrix(0, ncol=n, nrow=n)
for(i in 1:n) 
{
  for (j in 1:n)
  {
    u[i,j]<-(x[i]-x[j])/h
  }
}
w <- matrix(0, ncol=n, nrow=n)
for(i in 1:n)
{
  for(j in 1:n) 
  {
    w[i,j] <- kernel(u[i,j])
  }
}
H <- matrix(0, ncol=n, nrow=n)
for(i in 1:n)
{
  for(j in 1:n) 
  {
    H[i,j] <- w[i,j]/sum(w[i,])
  }
} 
mhlamda <- H%*%y
error <- y - mhlamda
MSE <- round(mean(error^2),4)
Atas <- sum((y-mhlamda)^2)
Bawah <- sum((y-mean(y))^2)
R2 <- 100-round(Atas/Bawah*100,4)
out <- round(data.frame(Y=y,Ytopi=mhlamda,Error=error),3)
print(out)

cat(" BERDASARKAN TABEL DIATAS DIKETAHUI:\n")
cat(" MSE =",MSE,"R-square =",R2,"\n")
win.graph()
plot(x,y,type="p",ylab="Y",xlab="X")
lines(x,mhlamda,type="l",main="Plot Estimasi vs Observasi",
col="red",lwd=2,ylab="Y",xlab="X")
}
est(datain)