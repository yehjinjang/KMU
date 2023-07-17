setwd('C:/Users/SEC/Desktop')
#extracting variables library 
library(dplyr)
#coming data file-Hn19_all 
data <- read.csv(file='C:/Users/SEC/Desktop/Hn19_all.csv',header=T)
data
#extracting variables 
vars <- c('ID','sex','age','HE_DM_HbA1c','DE1_31','HE_glu','HE_HbA1c')

data_F <- select(data,vars)

#checking datas
head(data_F,10)
# delete null data
data_F <-na.omit(data_F)
#checking datas
head(data_F,10)
#reorganization
data_F$HE_DM2 <- ifelse(data_F$HE_DM_HbA1c==3,1,0)

#extract sub 
data_subj <- subset(data_F,data_F$HE_DM_HbA1c==1 & data_F$DE1_31==0 | data_F$DE1_31==1)

#final sub
nrow(data_subj)

#check datas
head(data_subj,10)

#histogram 
hist(data_subj$HE_glu,freq=F,ylim=c(0,0.05),main='Histogram',xlab='He_glue °øº¹Ç÷´ç',col='white')
#kernel density plot
lines(density(data_subj$HE_glu),col='red',lwd=2)

#Q-QpLOT 
qqnorm(data_subj$HE_glu,col='red')
qqline(data_subj$HE_glu,col='blue',lwd=2)
#shapiro-wilk test 
shapiro.test(data_subj$HE_glu)
#equal variance 
var.test(data_subj$HE_glu ~ data_subj$DE1_31)
var.test(x,y)
t.test(data_subj$HE_glu ~ data_subj$DE1_31, paired=F, var.equal=F,conf.level=0.95)
t.test(x,y)

