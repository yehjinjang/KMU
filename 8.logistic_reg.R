
###############################
##### Logistic regression #####
###############################

#-------- 8.1 Logistic regression model --------#

## Ex1
Ex1 <- data.frame(MI=c(0,1,0,0,1,1,0),Age=c(20,20,15,18,40,50,25))
Ex1
model <- glm(MI~Age,family='binomial',data=Ex1)
summary(model)


#-------- 8.5 Multiple logistic regression model --------#

## Ex2
setwd('C:/.../')
lower_born <- read.csv('lowerborn.csv',header=T)
head(lower_born)
tail(lower_born)

model <- glm(low~age + lwt + factor(smoke) + factor(ht),family='binomial',data=lower_born)
summary(model)

