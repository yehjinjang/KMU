
#################################
##### Ch5. Data Diagnostics #####
#################################

#-------- 5.1 Residual --------#

## Ex1. 
setwd('C:/.../')
usedcars <- read.csv('usedcars.csv',header=T)
head(usedcars)
tail(usedcars)

## multiple linear regression
usedcars_model <- lm(price ~ year + mileage + cc + automatic, data=usedcars)
summary(usedcars_model)

# Hat matrix
X <- as.matrix(cbind(1,usedcars[,-1]));X
H <- X%*%solve(t(X)%*%X)%*%t(X)
round(H,2)

# residuals
Y_hat <- predict(usedcars_model)
res <- usedcars$price-Y_hat;res
resid(usedcars_model)

# standardized residuals
s <- sqrt(sum(res^2)/(dim(usedcars)[1]-5)) # Estimate of sigma
std_res <- res/(s*sqrt(1-diag(H)));std_res

# using rstandard function
rstandard(usedcars_model)

# residual plot
plot(std_res,pch=19,cex=1,ylab="std residual",xlab="Index",ylim=c(-2.5,2.5))
abline(h=-2,col="red",lty=2)
abline(h=2,col="red",lty=2)


#-------- 5.2 Leverage point --------#

## Ex2. 
H <- X%*%solve(t(X)%*%X)%*%t(X)
leverage <- diag(H)
2*(dim(X)[2])/dim(X)[1]

# using hatvalues function
hatvalues(usedcars_model)

# leverage plot
plot(leverage,pch=19,cex=1,ylab="leverage value",xlab="Index")
abline(h=0.333,col="blue",lty=2)


#-------- 5.3 Influential observations and measures --------#

## Ex3. 
infludata <- read.csv('infludata.csv',header=T)

fit <- lm(y ~ x, data=infludata) # All data
fit.A <- lm(y ~ x, data=infludata[-1,]) # remove A
fit.B <- lm(y ~ x, data=infludata[-2,]) # remove B
fit.AB <- lm(y ~ x, data=infludata[-c(1,2),]) # remove A,B
fit.C <- lm(y ~ x, data=infludata[-10,]) # remove C 
fit.D <- lm(y ~ x, data=infludata[-11,]) # remove D
fit.CD <- lm(y ~ x, data=infludata[-c(10,11),]) # remove C,D

summary(fit)
summary(fit.A)
summary(fit.B)
summary(fit.AB)
summary(fit.C)
summary(fit.D)
summary(fit.CD)

## Ex4. 
influence.measures(usedcars_model)

cooks.distance(usedcars_model) # cooks distance
dfbetas(usedcars_model) # DFBETAS
dffits(usedcars_model) # DFFITS
covratio(usedcars_model) # COVRATIO

# standard of measures
p <- 5
n <- 30
cook_standard <- 3.67/(n-p);cook_standard
DFBETAS_standard <- 2/sqrt(n);DFBETAS_standard
DFFITS_standard <- 2*sqrt(p/n);DFFITS_standard
COVRATIO_standard <- 3*p/n;COVRATIO_standard

# Plots

# 1) Cooks distance
plot(cooks.distance(usedcars_model),pch=19,ylab="cooks distance")
abline(h=cook_standard,col="red",lty=2)

# 2) DFBETAS
par(mfrow=c(2,2))
plot(dfbetas(usedcars_model)[,2],pch=19,ylab="DFBETAS_1")
abline(h=DFBETAS_standard,col="red",lty=2)
abline(h=-DFBETAS_standard,col="red",lty=2)

plot(dfbetas(usedcars_model)[,3],pch=19,ylab="DFBETAS_2")
abline(h=DFBETAS_standard,col="red",lty=2)
abline(h=-DFBETAS_standard,col="red",lty=2)

plot(dfbetas(usedcars_model)[,4],pch=19,ylab="DFBETAS_3")
abline(h=DFBETAS_standard,col="red",lty=2)
abline(h=-DFBETAS_standard,col="red",lty=2)

plot(dfbetas(usedcars_model)[,5],pch=19,ylab="DFBETAS_4")
abline(h=DFBETAS_standard,col="red",lty=2)
abline(h=-DFBETAS_standard,col="red",lty=2)

# 3) DFFITS
par(mfrow=c(1,1))
plot(dffits(usedcars_model),pch=19,ylab="DFFITS")
abline(h=DFFITS_standard,col="red",lty=2)
abline(h=-DFFITS_standard,col="red",lty=2)

# 4) COVRATIOS
plot(abs(covratio(usedcars_model)-1),pch=19,ylab="|COVRATIO-1|")
abline(h=COVRATIO_standard,col="red",lty=2)


#-------- 5.4 Durbin-Watshon test --------#
library(lmtest)
dwtest(usedcars_model)










