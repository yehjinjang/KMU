
###################################
##### Ch6. Variable selection #####
###################################

#-------- 6.1 Criteria for variable selection --------#

## Advertisement data
setwd('C:/.../')
Adver_dat <- read.csv('Advertising.csv',header=T)
head(Adver_dat)
tail(Adver_dat)

## Ex1. 
library(leaps)
Adver_reg <- Adver_dat[,c("sales","TV","radio","newspaper")] 
regfit_sel <- regsubsets(x=sales~.,data=Adver_reg,method="exhaustive",nbest=3)
summary(regfit_sel)
result_regfit <- summary(regfit_sel)
result_regfit$adjr2 # adj_R2

# plot of adj_R2
plot(result_regfit$adjr2,ylim=c(0,1),pch=19,cex=2,ylab="adj_R2",xlab="model",type="b")

# final model
final_model_R2 <- lm(sales ~ TV + radio,data=Adver_reg)
summary(final_model_R2)

# comparison with null model using partial-F test
null_model_R2 <- lm(sales ~ 1,data=Adver_reg)
anova(final_model_R2,null_model_R2)

## Ex2. 
library(leaps)
Adver_reg <- Adver_dat[,c("sales","TV","radio","newspaper")] 
regfit_sel <- regsubsets(x=sales~.,data=Adver_reg,method="exhaustive",nbest=3)
summary(regfit_sel)
result_regfit <- summary(regfit_sel)
result_regfit$cp # Mallows_Cp

# plot of Mallows-Cp
plot(result_regfit$cp,pch=19,cex=2,ylab="Mallows-Cp",xlab="model",type="b")

# final model
final_model_Cp <- lm(sales ~ TV + radio,data=Adver_reg)
summary(final_model_Cp)

# comparison with null model using partial-F test
null_model_Cp <- lm(sales ~ 1,data=Adver_reg)
anova(final_model_Cp,null_model_Cp)

## Ex3. 
library(qpcR)
model1 <- lm(sales ~ TV,data=Adver_dat)
model2 <- lm(sales ~ radio,data=Adver_dat)
model3 <- lm(sales ~ newspaper,data=Adver_dat)
model4 <- lm(sales ~ TV + radio,data=Adver_dat)
model5 <- lm(sales ~ TV + newspaper,data=Adver_dat)
model6 <- lm(sales ~ radio + newspaper,data=Adver_dat)
model7 <- lm(sales ~ TV + radio + newspaper,data=Adver_dat)

PRESS(model1)
PRESS1 <- PRESS(model1)
PRESS2 <- PRESS(model2)
PRESS3 <- PRESS(model3)
PRESS4 <- PRESS(model4)
PRESS5 <- PRESS(model5)
PRESS6 <- PRESS(model6)
PRESS7 <- PRESS(model7)

Y_PRESS <- c(PRESS1$stat,PRESS2$stat,PRESS3$stat,PRESS4$stat,PRESS5$stat,PRESS6$stat,PRESS7$stat)
Y_PRESS

# plot of PRESS
plot(Y_PRESS,pch=19,cex=2,ylab="PRESS",xlab="model",type="b")

# final model
final_model_PRESS <- lm(sales ~ TV + radio,data=Adver_reg)
summary(final_model_PRESS)

# comparison with null model using partial-F test
null_model_PRESS <- lm(sales ~ 1,data=Adver_reg)
anova(final_model_PRESS,null_model_PRESS)


## Ex4. 
library(leaps)
Adver_reg <- Adver_dat[,c("sales","TV","radio","newspaper")] 
regfit_sel <- regsubsets(x=sales~.,data=Adver_reg,method="exhaustive",nbest=3)
summary(regfit_sel)
result_regfit <- summary(regfit_sel)
result_regfit$bic # BIC

# plot of BIC
plot(result_regfit$bic,pch=19,cex=2,ylab="BIC",xlab="model",type="b")

# final model
final_model_bic <- lm(sales ~ TV + radio,data=Adver_reg)
summary(final_model_bic)

# comparison with null model using partial-F test
null_model_bic <- lm(sales ~ 1,data=Adver_reg)
anova(final_model_bic,null_model_bic)


## Ex5. 

# stepwise
null_model <- lm(sales~1,data=Adver_dat)
step(null_model,scope = ~ TV + radio + newspaper,direction="both",test="F")

# forward
null_model <- lm(sales~1,data=Adver_dat)
step(null_model,scope = ~ TV + radio + newspaper,direction="forward",test="F")

# backward
Full_model <- lm(sales ~ TV + radio + newspaper,data=Adver_dat)
step(Full_model,direction="backward",test="F")


## Ex6. 
set.seed(1234)
rn <- sample(x=c(1:200),size=200,replace=F);rn
Adver_dat$rn <- rn
head(Adver_dat)

train_dat <- Adver_dat[Adver_dat$rn>60,]
test_dat <- Adver_dat[Adver_dat$rn<=60,]
dim(train_dat)
dim(test_dat)

# predicted error
train_model <- lm(sales ~ TV + radio,data=train_dat);summary(train_model)
predict_value <- predict(train_model,newdata=test_dat[,c("TV","radio")])
predict_error <- sum((test_dat$sales-predict_value)^2)
predict_error

# PRESS
library(qpcR)
Final_model <- lm(sales ~ TV + radio,data=Adver_dat)
PRESS_Final <- PRESS(Final_model)
PRESS_Final$stat # PRESS = 582.1352

# SST, SSE, SSR
SST <- sum((Adver_dat$sales-mean(Adver_dat$sales))^2)
SSE <- sum(resid(Final_model)^2) # SSE = 556.9 
SSR <- SST-SSE

# R2 vs R2_predic
1-(SSE/SST) # R2 = 0.8972
1-(PRESS_Final$stat/SST) # R2_predic = 0.8925




















