
###########################################
##### Ch3. Multiple linear regression #####
###########################################

#-------- 2. Estimation of regression parameters --------#

## Sale data
Sale_data <- data.frame(Y=c(65,33,90,85,60,57,73),X1=c(4.3,1.1,7.2,6.5,3.8,2.9,5.1),X2=c(15,28,100,34,34,45,65))
Sale_data
dim(Sale_data)

## Ex3. Estimation of coefficients
X_mat <- cbind(rep(x=1,times=dim(Sale_data)[1]),Sale_data[,-1]);X_mat
X_mat <- as.matrix(X_mat);X_mat
colnames(X_mat) <- c("X0","X1","X2");X_mat
Y_vec <- Sale_data[,1];Y_vec

# 1. estimate of betas
solve(t(X_mat)%*%X_mat)%*%t(X_mat)%*%Y_vec

# using lm function
model <- lm(Y~X1+X2,data=Sale_data)
model

# 2. prediction value
Y_hat <- predict(model,newdata=data.frame(X1=Sale_data[,2],X2=Sale_data[,3]))

# 3. residual
res <- resid(model);res
Sale_data[,1] - Y_hat

# 4. Estimate of sigma
s2 <- sum(res^2)/(dim(Sale_data)[1]-3)
s2
sqrt(s2)


#-------- 3. Quality of fit and ANOVA --------#

## Ex4 and 5. ANOVA
Y <- Sale_data[,1]

# SS
SST <- sum((Y-mean(Y))^2);SST
SSR <- sum((Y_hat-mean(Y))^2);SSR
SSE <- sum((Y-Y_hat)^2);SSE

# MS
MSR <- SSR/(3-1);MSR
MSE <- SSE/(dim(Sale_data)[1]-3);MSE

# F-value
F_val <- MSR/MSE;F_val
pf(F_val,df1=2,df=4,lower.tail=F)

# using function
anova(model)
summary(model)


#-------- 4. Inference of coefficients --------#

## Ex6. Test of estimated betas

# diagonal elements of solve(t(X)%*%X)
XX_inv <- solve(t(X_mat)%*%X_mat);XX_inv
diag_val <- diag(XX_inv);diag_val

# SE of betas
s2 <- sum(res^2)/(dim(Sale_data)[1]-3);s2
s <- sqrt(s2);s
SE_Betas <- s*sqrt(diag_val);SE_Betas

# t-test
coef_mat <- summary(model)$coef
coef_mat
t_values <- (coef_mat[,1] - 0)/SE_Betas;t_values

pt(t_values[1],df=dim(Sale_data)[1]-3,lower.tail=F)*2
pt(t_values[2],df=dim(Sale_data)[1]-3,lower.tail=F)*2
pt(t_values[3],df=dim(Sale_data)[1]-3,lower.tail=T)*2

# 95% CI
coef_mat[,1]-qt(p=0.025,df=dim(Sale_data)[1]-3,lower.tail=F)*SE_Betas
coef_mat[,1]+qt(p=0.025,df=dim(Sale_data)[1]-3,lower.tail=F)*SE_Betas

confint(model) # using confint function


## Ex7. Inference of mean, new data given data

# 1) mean
x0 <- c(1,5,75);x0
Est_value <- x0%*%coef_mat[,1];Est_value
predict(model,newdata=data.frame(X1=5,X2=75))
SE_mean <- s*sqrt(t(x0)%*%solve(t(X_mat)%*%X_mat)%*%x0);SE_mean
Est_value-qt(p=0.025,df=dim(Sale_data)[1]-3,lower.tail = F)*SE_mean
Est_value+qt(p=0.025,df=dim(Sale_data)[1]-3,lower.tail = F)*SE_mean
# using predict function
predict(model,newdata=data.frame(X1=5,X2=75),interval="confidence")

# 2) predict
Est_value <- x0%*%coef_mat[,1];Est_value
SE_pre <- s*sqrt(1+t(x0)%*%solve(t(X_mat)%*%X_mat)%*%x0);SE_pre
Est_value-qt(p=0.025,df=dim(Sale_data)[1]-3,lower.tail = F)*SE_pre
Est_value+qt(p=0.025,df=dim(Sale_data)[1]-3,lower.tail = F)*SE_pre
# using predict function
predict(model,newdata=data.frame(X1=5,X2=75),interval="prediction")


## Ex8. Analysis of residual

# (1) std residual VS fitted value
hii <- diag(X_mat%*%solve(t(X_mat)%*%X_mat)%*%t(X_mat));hii
res <- (Y-Y_hat);res
resid(model) # using resid function

std_resi <- res/(s*sqrt(1-hii));std_resi
rstandard(model) # using rstandard

plot(x=Y_hat,y=std_resi,ylim=c(-3,3),cex=2,pch=19,ylab="standardized residual",xlab="fitted value",
     main="std residual VS fitted value")
abline(h=-2,col="red",lty=2,lwd=2)
abline(h=2,col="red",lty=2,lwd=2)

# (2) std residual VS obs i
plot(x=c(1:7),y=std_resi,ylim=c(-3,3),cex=2,pch=19,ylab="standardized residual",xlab="observation i",
     main="std residual VS i")
abline(h=-2,col="red",lty=2,lwd=2)
abline(h=2,col="red",lty=2,lwd=2)

# (3) Q-Q plot
qqnorm(std_resi,cex=2,pch=19,ylab="standardized residual")
qqline(std_resi,col="red",lwd=2,lty=2)


## using plot(model)
par(mfrow=c(2,2))
plot(model)


