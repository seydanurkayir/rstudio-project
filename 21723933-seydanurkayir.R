data <- read_table()  

names(data)<-c("y","x1","x2","x3")
attach(data)
qqnorm(y)
qqline(y)
shapiro.test(y)

lny<-log(y)
qqnorm(lny)
qqline(lny)
shapiro.test(lny)

boxplot(y)$out
which(y %in%  boxplot(y)$out)
data1<-data[-c(21,60),]
qqnorm(data1$y)
qqline(data1$y)
shapiro.test(data1$y)
datay<-cbind(y,x1,x2,x3)
pairs(datay)

sonuc<-lm(y~x1+x2+x3)
summary(sonuc)



influence.measures(sonuc)
inf<-ls.diag(sonuc)
inf
par(mfrow= c(2,2))
plot(predict(sonuc),abs(inf$stud.res),ylab= "Studential Residuals", xlab= "Predicated Value")

library(zoo)
n<-98
k<-4

cooksd<-cooks.distance(sonuc)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (length(data$y)>50) 4/length(data$y) else 4/(length(data$y)-(length(data)-1)-1) , col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (length(data$y)>50) 4/length(data$y) else 4/(length(data$y)-(length(data)-1)-1),names(cooksd),""), col="red")
boxplot(cooksd)$out


cooksdd<- cooksd[-c(16,21,24,60,64,68,69,76,86,99 )]
shapiro.test(cooksdd)
plot(cooksdd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = if (n>50) 4/n else 4/(n-k-1) , col="red")
text(x=1:length(cooksdd)+1, y=cooksdd, labels=ifelse(cooksdd>if (n>50) 4/n else 4/(n-k- 1),names(cooksdd),""), col="red")
boxplot(cooksdd)$out


lncooksdd<-log(cooksdd)
shapiro.test(lncooksdd)
boxplot(cooksdd)$out

cooksdd2<-cooksdd[-c(12,18)] 
shapiro.test(cooksdd2) 


library(zoo)
hat<- inf$hat
plot(hat, pch="*", cex=2, main="Leverage Value by Hat value") 
abline(h = 2*(k+1)/n , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/n,index(hat),""), col="red")
boxplot(hat)$out
which(hat%in%boxplot(hat)$out)
hatt<- hat[-c(37,96)]

plot(hatt, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/n , col="red")
text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/n,index(hat),""), col="red")

shapiro.test(hatt)
lnhatt1<-log(hatt)
shapiro.test(lnhatt1)
plot(lnhatt1, pch="*", cex=2, main="Leverage Value by Hat value")
abline(h = 2*(k+1)/n , col="red")
text(x=1:length(lnhatt1)+1, y=lnhatt1, labels=ifelse(lnhatt1>2*(k+1)/n,index(lnhatt1),""), col="red")



std<-inf$std.res
plot(std, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(std)+1, y=std, labels=ifelse(std<-2 & std>2,index(std),""), col="red")

boxplot(std)$out
which(std%in%boxplot(std)$out)
stdd<- std[-c(60)]
shapiro.test(stdd)

plot(stdd, pch="*", cex=2, main="Outlier by Standardized residuals",ylab="Standardized Residuals", xlab="Index")
abline(h = c(-2,2) ,col="red")
text(x=1:length(stdd)+1, y=stdd, labels=ifelse(stdd<-2 & stdd>2,index(std),""), col="red")


stud<- inf$stud.res
plot(stud, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(stud)+1, y=stud, labels=ifelse(stud<-3 & stud>3,index(stud),""), col="red")

boxplot(stud)$out
which(stud%in%boxplot(std)$out)
studd<- stud[-c(60)]
shapiro.test(studd)

plot(studd, pch="*", cex=2, main="Outlier by Studentized residuals",ylab="Studentized Residuals", xlab="Index")
abline(h = c(-3,3) ,col="red")
text(x=1:length(studd)+1, y=studd, labels=ifelse(studd<-3 & studd>3,index(studd),""), col="red")

confint(sonuc,level = .99)

library(lmtest)
bptest(sonuc)

summary(lm(abs(residuals(sonuc)) ~ fitted(sonuc)))

dwtest(sonuc)

library(car)
vif(sonuc)

ort1<-mean(x1)
kt1<-sum((x1-ort1)^2)
skx1<-(x1-ort1)/(kt1^0.5)
ort2<-mean(x2)
kt2<-sum((x2-ort2)^2)
skx2<-(x2-ort2)/(kt2^0.5)
ort3<-mean(x3)
kt3<-sum((x3-ort3)^2)
skx3<-(x3-ort3)/(kt3^0.5)

x<-cbind(skx1,skx2,skx3)
sm<-eigen(t(x) %*%x)
signif(sm$values,3)
signif(sm$vectors,3)
t(V)%*%V
t(V)%*%V


library(stats)
lm.null<-lm(y~1)
forward<- step(lm.null,y~x1+x2+x3,  direction = "forward")
forward
summary(forward)

backward<-step(sonuc,direction="backward")
summary(backward)

library(MASS)
step.model <- stepAIC(sonuc, direction = "both", trace = FALSE)
summary(step.model)

predict(sonuc, data.frame(x1=8.536118, x2=-0.52000588, x3=2.691658 ), interval = 'prediction')
predict(sonuc, data.frame(x1=8.536118, x2=-0.52000588, x3=2.691658 ), interval = 'confidence',level = .95)

predict(sonuc, data.frame(x1=1.912, x2=5.684, x3=9.325 ), interval = 'prediction')
predict(sonuc, data.frame(x1=1.912, x2=5.684, x3=9.325 ), interval = 'confidence', level= .95)

