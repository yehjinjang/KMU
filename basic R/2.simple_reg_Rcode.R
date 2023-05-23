
#########################################
##### Ch2. Simple linear regression #####
#########################################


#-------- 3. Estimation of regression parameters --------#

## Ex1. Estimation of coefficient(beta)

# icecream data
Score_data <- data.frame(science = c(76,74,44,84,79,67,77),math=c(61,59,62,74,85,68,73))
Score_data

X <- Score_data$science
Y <- Score_data$math


# estimate of beta
beta1_hat <- sum((X-mean(X))*(Y-mean(Y)))/sum((X-mean(X))^2)
beta0_hat <- mean(Y) - beta1_hat*mean(X)
beta1_hat
beta0_hat

# using lm function
model <- lm(math ~ science,data=Score_data)
model

# scatter plot and reg line
plot(x=Ice_data$temp,y=Ice_data$Ice,pch=19,cex=2,xlab="temperature",ylab="Sales")
model <- lm(Ice_data$Ice~Ice_data$temp)
abline(model,col="red",lwd=2)


## Ex2. Estimation of variance of error

# yhat
Y_hat <- beta0_hat + beta1_hat*X
Y_hat
predict(model,newdata=data.frame(science=Score_data$science))

# residual
res <- Y-Y_hat
res
resid(model)

# Estimate of sigma
sum(res^2)
s2 <- sum(res^2)/(length(X)-2)
s2
sqrt(s2)


#-------- 4. Quality of fit --------#

## method1 : Scatter plot with loess 
# scatter plot
plot(x=X,y=Y,pch=19,cex=2,xlab="temperature",ylab="Sales")
# regression line
model <- lm(Ice~temp,data=Ice_data)
abline(model,col="red",lwd=2)
# local regression(loess)
lo_fit <- loess(Y~X, span= 1)
x_loess <- seq(min(X),max(X), (max(X) - min(X))/1000)
lo_pre <- predict(lo_fit,x_loess)
lines(x_loess, out, col='blue', lwd=2)
legend("topleft",legend = c("lm", "loess"),col = c("red","blue"),lty=1,lwd=2,cex=2)

## method2 : Scatter plot with fitted value and y
plot(Y_hat,Y,pch=19,cex=2,xlab="Fitted value",ylab="Sales")
abline(a=0,b=1,col="grey",lwd=2)

## method3 : Residual-Fit plot (RF plot)
A1 <- Y_hat-mean(Y)
A2 <- Y-Y_hat

Q <- seq(0,1,by=0.1)
A1_Q <- quantile(A1,prob=seq(0,1,by=0.1))
A2_Q <- quantile(A2,prob=seq(0,1,by=0.1))

plot(x=Q,y=A1_Q,ylim=c(-5,5),pch=19,col="blue",xlab="probability",ylab="Fitted values minus Mean")
abline(h=0,col="grey",lty=2,lwd=2)
plot(x=Q,y=A2_Q,ylim=c(-5,5),pch=19,col="red",xlab="probability",ylab="Resdiduals")
abline(h=0,col="grey",lty=2,lwd=2)


## Ex3. quality of fit 

# Partition of sums of squares
SST <- sum((Y-mean(Y))^2)
SSR <- sum((Y_hat-mean(Y))^2)
SSE <- sum((Y-Y_hat)^2)
SST;SSR;SSE

# (1) residual standard error and coefficient of determination, R2

# resi std error
s <- sqrt(SSE/(length(Y)-2))
s

# R2
SSR/SST

# using lm function
model <- lm(math~science,data=Score_data)
summary(model)

# (2) PRESS and R2_PRESS

# 'for' example
test <- rep(NA,5)
for(i in 1:5){
  test[i] <- i+2
}
test

# PRESS
Y_i <- rep(NA,length(Y))
for(i in 1:length(Y)){
  Score_data_i <- Score_data[-i,]
  model_i <- lm(math~science,data=Score_data_i)
  Y_i[i] <- predict(model_i,newdata=data.frame(science=Score_data$science))[i]
}
PRESS <- sum((Y-Y_i)^2)
PRESS
SSE


(Y-Y_i)^2 # (Y - Yi(i))^2
(Y-Y_hat)^2 # resi^2

# R2_PRESS
1-(PRESS/SST)

# (3) ANOVA
MSE <- SSE/(length(Y)-2);MSE
MSR <- SSR/1;MSR
F_val <- MSR/MSE;F_val
pf(F_val,df1=1,df2=length(Y)-2,lower.tail=F)

# using anova function
anova(model)


#-------- 5. Inference of coefficient --------#

## Ex4. Test of estimated beta

# 1) beta1
SE1 <- s/sqrt(sum((X-mean(X))^2));SE1
beta1_t0 <- beta1_hat/SE1;beta1_t0
pt(beta1_t0,df=length(Y)-2,lower.tail=F)*2

# 95% CI of beta1
beta1_hat - qt(p=0.025,df=length(Y)-2,lower.tail=F)*SE1
beta1_hat + qt(p=0.025,df=length(Y)-2,lower.tail=F)*SE1
mean(X)^2
# 2) beta0
SE0 <- s*sqrt((1/length(Y))+sum((X-mean(X))^2));SE0
beta0_t0 <- beta0_hat/(SE0);beta0_t0
pt(beta0_t0,df=length(Y)-2,lower.tail=T)*2

# 95% CI of beta0
beta0_hat - qt(p=0.025,df=length(Y)-2,lower.tail=F)*SE0
beta0_hat + qt(p=0.025,df=length(Y)-2,lower.tail=F)*SE0

# using function
summary(model)
confint(model)


## Ex5. Inference of mean, new data given data

# 1) mean
Est_value <- beta0_hat + beta1_hat*25;Est_value
predict(model,newdata=data.frame(temp=25))
SE_mean <- s*sqrt((1/(length(Y))+(25-mean(X))^2/sum((X-mean(X))^2)));SE_mean
Est_value-qt(p=0.025,df=length(Y)-2,lower.tail = F)*SE_mean
Est_value+qt(p=0.025,df=length(Y)-2,lower.tail = F)*SE_mean
model <- lm(Ice~temp,data=Ice_data)
# using predict function
predict(model,newdata=data.frame(temp=25),interval="confidence")

# 2) predict
Est_value <- beta0_hat + beta1_hat*25;Est_value
SE_pre <- s*sqrt((1+(1/(length(Y)))+(25-mean(X))^2/sum((X-mean(X))^2)))
Est_value-qt(p=0.025,df=length(Y)-2,lower.tail = F)*SE_pre
Est_value+qt(p=0.025,df=length(Y)-2,lower.tail = F)*SE_pre
# using predict function
model <- lm(Ice~temp,data=Ice_data)
predict(model,newdata=data.frame(temp=25),interval="prediction")


## Ex6. Analysis of residual

# (1) std residual VS fitted value
hii <- (1/length(Y)) + (X-mean(X))^2/sum((X-mean(X))^2);hii
res <- (Y-Y_hat);res
resid(model) # using resid function

std_resi <- res/(s*sqrt(1-hii));std_resi
rstandard(model) # using rstandard

plot(x=Y_hat,y=std_resi,ylim=c(-3,3),cex=2,pch=19,ylab="standardized residual",xlab="fitted value",
     main="std residual VS fitted value")
abline(h=-2,col="red",lty=2,lwd=2)
abline(h=2,col="red",lty=2,lwd=2)

# (2) std residual VS X
plot(x=X,y=std_resi,ylim=c(-3,3),cex=2,pch=19,ylab="standardized residual",xlab="Temperature(X)",
     main="std residual VS X")
abline(h=-2,col="red",lty=2,lwd=2)
abline(h=2,col="red",lty=2,lwd=2)

# (3) std residual VS obs i
plot(x=c(1:7),y=std_resi,ylim=c(-3,3),cex=2,pch=19,ylab="standardized residual",xlab="observation i",
     main="std residual VS i")
abline(h=-2,col="red",lty=2,lwd=2)
abline(h=2,col="red",lty=2,lwd=2)

# (4) Q-Q plot
qqnorm(std_resi,cex=2,pch=19,ylab="standardized residual")
qqline(std_resi,col="red",lwd=2,lty=2)






