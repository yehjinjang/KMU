
#######################################
##### Ch4. Regression Diagnostics #####
#######################################

#-------- 4.1 partial F test --------#

## Advertisement data
setwd('C:/.../')
Adver_dat <- read.csv('Advertising.csv',header=T)
head(Adver_dat)
tail(Adver_dat)

Y <- Adver_dat[,5]
X1 <- Adver_dat[,2]
X2 <- Adver_dat[,3]
X3 <- Adver_dat[,4]

## Ex1. partial F test

## (1) Full model
Full_model <- lm(sales ~ TV + radio + newspaper, data=Adver_dat)
Full_model <- lm(Y ~ X1 + X2 + X3)
summary(Full_model)

Y_hat_F <- predict(Full_model,newdata = data.frame(X1=X1,X2=X2,X3=X3))
head(Y_hat_F)

# SST, SSR, SSE
SST <- sum((Y-mean(Y))^2);SST
SSR_F <- sum((Y_hat_F-mean(Y))^2);SSR_F
SSE_F <- sum((Y-Y_hat_F)^2);SSE_F

# MSR, MSE
MSR_F <- SSR_F/(4-1);MSR_F
MSE_F <- SSE_F/(200-4);MSE_F

# F-value
F0_F <- MSR_F/MSE_F;F0_F
pf(F0_F,df1=3,df2=196,lower.tail=F)


## (2) Reduced model
Reduced_model <- lm(sales ~ TV + radio, data=Adver_dat)
Reduced_model <- lm(Y ~ X1 + X2)
summary(Reduced_model)

Y_hat_R <- predict(Reduced_model,newdata = data.frame(X1=X1,X2=X2))
head(Y_hat_R)

# SST, SSR, SSE
SST <- sum((Y-mean(Y))^2);SST
SSR_R <- sum((Y_hat_R-mean(Y))^2);SSR_R
SSE_R <- sum((Y-Y_hat_R)^2);SSE_R

# MSR, MSE
MSR_R <- SSR_R/(3-1);MSR_R
MSE_R <- SSE_R/(200-3);MSE_R

# F-value
F0_R <- MSR_R/MSE_R;F0_R
pf(F0_R,df1=2,df2=197,lower.tail=F)


## (3) Calculation of F0
R_b2b1 <- (556.914-556.8253)
F0 <- (R_b2b1/(3-2))/(556.8253/196)
pf(F0,df1=1,df2=196,lower.tail=F)


## (4) using anova function
anova(Reduced_model,Full_model)


## Ex2. sequential sum of squares
anova(Full_model)


#-------- 4.2 partial regression plot --------#

## scatter plot using simple reg 
plot(x=X1,y=Y,pch=19,cex=2,xlab="X1(TV)",ylab="Sales")
model1 <- lm(Y~X1);model1
abline(model1,col="red",lwd=2)

plot(x=X2,y=Y,pch=19,cex=2,xlab="X2(radio)",ylab="Sales")
model2 <- lm(Y~X2);model2
abline(model2,col="red",lwd=2)

plot(x=X3,y=Y,pch=19,cex=2,xlab="X2(radio)",ylab="Sales")
model3 <- lm(Y~X3);model3
abline(model3,col="red",lwd=2)

## X2+X3 model
model23 <- lm(Y~X2+X3);model23
modelX23 <- lm(X1~X2+X3);modelX23

## Ex3. partial regression plot

# (1) estimated linear regression models
model <- lm(Y~X1+X2+X3);model
model23 <- lm(Y~X2+X3);model23
modelX23 <- lm(X1~X2+X3);modelX23

# (2) calculation of the residuals
y.x2x3 <- resid(model23)
x1.x2x3 <- resid(modelX23)

# (3) partial reg plot and corr
plot(x1.x2x3,y.x2x3,pch=19,cex=2,xlab="X1|X2,X3",ylab="Y|X2,X3")
model_parX1 <- lm(y.x2x3~x1.x2x3);model_parX1
abline(model_parX1,col="red",lwd=2)
cor.test(x1.x2x3,y.x2x3)


#-------- 4.3 lack of fit test of the model --------#

## Ex4. lack of fit test

# (1) H0 : E(y) = b0+b1*x1
fit.lm1 <- lm(Y ~ X1)
fit.pe1 <- lm(Y ~ factor(X1))
anova(fit.lm1,fit.pe1)

# (2) H0 : E(y) = b0+b1*x1+b2*x2+b3*x3
fit.lm3 <- lm(Y ~ X1+X2+X3)
fit.pe3 <- lm(Y ~ factor(X1)+factor(X2)+factor(X3))
anova(fit.lm3,fit.pe3)


#-------- 4.4 transformation of variables --------#

## 4.4.1 transformation for linear 

## Ex5. mammal
setwd('C:/.../')
mammal <- read.csv('mammal.csv',header=T)
plot(mammal$body,mammal$brain,pch=19,cex=2,xlab="body",ylab="brain")

log_brain <- log(mammal$brain)
log_body <- log(mammal$body)
plot(log_body,log_brain,pch=19,cex=2,xlab="log(body)",ylab="log(brain)")

model_EX5 <- lm(brain ~ body ,data=mammal)
summary(model_EX5)

model_log_EX5 <- lm(log_brain ~ log_body)
summary(model_log_EX5)


## Ex6. bug
bug <- read.csv('bug.csv',header=T);bug
plot(bug$time,bug$y,pch=19,cex=2,xlab="time",ylab="# of bugs")
model_EX6 <- lm(y~time,data=bug)
summary(model_EX6)

log_y <- log(bug$y)
plot(bug$time,log_y,pch=19,cex=2,xlab="time",ylab="log(# of bugs)")
model_log_EX6 <- lm(log_y~bug$time)
summary(model_log_EX6)


## 4.4.2 variance stabilizing transformation

## Ex7. restaurant
restaurant <- read.csv('restaurant.csv',header=T);restaurant
Y <- restaurant$Y
X <- restaurant$X
plot(X,Y,pch=19,cex=2,xlab="Advertising(X)",ylab="Sales(Y)")

# transformation
Y_log <- log(Y)
Y_sqrt <- sqrt(Y)
Y_inv <- 1/Y

# model1
model_EX7 <- lm(Y ~ X)
summary(model_EX7)
par(mfrow=c(2,2))
plot(model_EX7)

# model_log
model_EX7_log <- lm(Y_log ~ X)
summary(model_EX7_log)
par(mfrow=c(2,2))
plot(model_EX7_log)

# model_sqrt
model_EX7_sqrt <- lm(Y_sqrt ~ X)
summary(model_EX7_sqrt)
par(mfrow=c(2,2))
plot(model_EX7_sqrt)

# model_inv
model_EX7_inv <- lm(Y_inv ~ X)
summary(model_EX7_inv)
par(mfrow=c(2,2))
plot(model_EX7_inv)


## 4.4.3 normal transformation - power transformation
library(MASS)
model_EX7 <- lm(Y ~ X)
par(mfrow=c(1,1))
boxcox_result <- boxcox(model_EX7)
boxcox_result

# normal check
std_EX7 <- rstandard(model_EX7) 
qqnorm(std_EX7,cex=2,pch=19,ylab="standardized residual")
qqline(std_EX7,col="red",lwd=2,lty=2)

model_EX7_p0.8 <- lm(Y^0.8 ~ X)
std_EX7_p0.8 <- rstandard(model_EX7_p0.8) 
qqnorm(std_EX7_p0.8,cex=2,pch=19,ylab="standardized residual")
qqline(std_EX7_p0.8,col="red",lwd=2,lty=2)

std_EX7_sqrt <- rstandard(model_EX7_sqrt) 
qqnorm(std_EX7_sqrt,cex=2,pch=19,ylab="standardized residual")
qqline(std_EX7_sqrt,col="red",lwd=2,lty=2)


## 4.5 standardized regression coefficient

# EX 9.
houseprice <- read.csv('houseprice.csv',header=T);houseprice
model_EX9 <- lm(price ~ tax + ground + floor + year,data=houseprice)
summary(model_EX9)

Y <- houseprice$price
X1 <- houseprice$tax
X2 <- houseprice$ground
X3 <- houseprice$floor
X4 <- houseprice$year

# variable standardization
Y_std <- (Y-mean(Y))/sqrt(sum((Y-mean(Y))^2))
X1_std <- (X1-mean(X1))/sqrt(sum((X1-mean(X1))^2))
X2_std <- (X2-mean(X2))/sqrt(sum((X2-mean(X2))^2))
X3_std <- (X3-mean(X3))/sqrt(sum((X3-mean(X3))^2))
X4_std <- (X4-mean(X4))/sqrt(sum((X4-mean(X4))^2))


model_EX9_std <- lm(Y_std ~ X1_std + X2_std + X3_std + X4_std)
summary(model_EX9_std)

## 4.6 Multicollinearity
model_EX9 <- lm(price ~ tax + ground + floor + year,data=houseprice)
summary(model_EX9)

# cor
Y <- houseprice$price
X1 <- houseprice$tax
X2 <- houseprice$ground
X3 <- houseprice$floor
X4 <- houseprice$year

X <- cbind(X1,X2,X3,X4)
R <- cor(X);R
round(R,3)

# vif
diag(solve(R))

# using vif function
library(car)
vif(model_EX9)

# partial F-test
Reduced_model_EX9 <- lm(Y ~ X1+X2+X4)
Full_model_EX9 <- lm(Y ~ X1+X2+X3+X4)
anova(Reduced_model_EX9,Full_model_EX9)









