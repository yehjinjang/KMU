
#-------- 1. Calculation of probability --------#

# probability distribution in R

# 1) normal dist
dnorm(x=0.5,mean=0,sd=1)
pnorm(q=1.96,mean=0,sd=1,lower.tail=F)
pnorm(q=1.96,mean=0,sd=1,lower.tail=T)
qnorm(p=0.025,mean=0,sd=1,lower.tail=F)
qnorm(p=0.025,mean=0,sd=1,lower.tail=T)
rnorm(n=10,mean=0,sd=1)

# 2) t-dist
dt(x=0.5,df=10)
pt(q=1.96,df=10,lower.tail=F)
qt(p=0.025,df=10,lower.tail=F)
rt(n=10,df=10)

# 3) chi-squared dist
dchisq(x=0.5,df=2)
pchisq(q=1.2,df=2,lower.tail=T)
qchisq(p=0.05,df=2,lower.tail=F)
rchisq(n=10,df=2)

# 4) F dist
df(x=0.8,df1=2,df2=3)
pf(q=1.2,df1=2,df2=3,lower.tail=T)
qf(p=0.05,df1=2,df2=3,lower.tail=F)
rf(n=10,df1=2,df2=3)


## Ex1) Normal distribution

# Q1) P(0<Z<=1.21)
normal_area1 <- pnorm(q=1.21,mean=0,sd=1,lower.tail=T)
normal_area1
normal_area2 <- pnorm(q=0,mean=0,sd=1,lower.tail=T)
normal_area2

normal_area1-normal_area2

# Q2) P(Z>1.21)
pnorm(q=1.21,mean=0,sd=1,lower.tail=F)
1-normal_area1


# Q3) P(Z<-1.21)
normal_area3 <- pnorm(q=-1.21,mean=0,sd=1,lower.tail=T)
normal_area3


# Q4) P(-1.21<Z<=1.21)
normal_area1-normal_area3
(normal_area1-normal_area2)*2


# Q5) P(-1.21<Z<=1.29)
normal_area4 <- pnorm(q=1.29,mean=0,sd=1,lower.tail=T)
normal_area4
normal_area4-normal_area3

# Q6) P(1.21<Z<=1.29)
normal_area4-normal_area1


## Ex2) t-distribution

# Q1) P(0<t(3)<=1.21)
t_area1 <- pt(q=1.21,df=3,lower.tail=T)
t_area1
t_area2 <- pt(q=0,df=3,lower.tail=T)
t_area2

t_area1-t_area2

# Q2) P(t(3)>1.21)
pt(q=1.21,df=3,lower.tail=F)
1-t_area1

# Q3) P(t(3)<-1.21)
t_area3 <- pt(q=-1.21,df=3,lower.tail=T)
t_area3

# Q4) P(-1.21<t(3)<=1.21)
t_area1-t_area3
(t_area1-t_area2)*2

# Q5) P(-1.21<t(5)<=1.29)
t_area4 <- pt(q=1.29,df=5,lower.tail=T)
t_area4
t_area5 <- pt(q=-1.21,df=5,lower.tail=T)
t_area5
t_area4-t_area5

# Q6) P(1.21<t(100)<=1.29)
t_area6 <- pt(q=1.29,df=100,lower.tail=T)
t_area6
t_area7 <- pt(q=1.21,df=100,lower.tail=T)
t_area7
t_area6-t_area7




## Ex3) Chi-squared distribution

# Q1) P(0<X2(3)<=3)
chi_area1 <- pchisq(q=0,df=3,lower.tail=T)
chi_area1
chi_area2 <- pchisq(q=3,df=3,lower.tail=T)
chi_area2

chi_area2-chi_area1

# Q2) P(X2(3)>3)
pchisq(q=3,df=3,lower.tail=F)
1-chi_area2

# Q3) P(X2(3)<-1.21)
chi_area3 <- pchisq(q=-1.21,df=3,lower.tail=T)
chi_area3

# Q4) P(-1.21<X2(3)<=1.21)
pchisq(q=1.21,df=3,lower.tail=T)



## Ex4) F-distribution

# Q1) P(0<F(3,1)<=1.21)
F_area1 <- pf(q=0,df1=3,df2=1,lower.tail=T)
F_area1
F_area2 <- pf(q=1.21,df1=3,df2=1,lower.tail=T)
F_area2


# Q2) P(F(3,1)>1.21)
pf(q=1.21,df1=3,df2=1,lower.tail=F)
1-F_area2

# Q3) P(F(3,1)<-1.21)
pf(q=-1.21,df1=3,df2=1,lower.tail=T)


# Q4) P(-1.21<F(3,1)<=1.21)
F_area2


# Q5) P(1.21<F(3,100)<=1.29)
F_area4 <- pf(q=1.21,df1=3,df2=100,lower.tail=T)
F_area4
F_area5 <- pf(q=1.29,df1=3,df2=100,lower.tail=T)
F_area5
F_area5-F_area4

# Q6) P(1.21<F(100,3)<=1.29)
F_area6 <- pf(q=1.21,df1=100,df2=3,lower.tail=T)
F_area6
F_area7 <- pf(q=1.29,df1=100,df2=3,lower.tail=T)
F_area7
F_area7-F_area6




#-------- 2. Estimation and Test --------#

## Ex8) Male height example - Estimation

# sample
Data <- c(192,180,190,186,169,176,179,173,191,170)

mean(Data) # sample mean
sd(Data) # sample standard deviance

X_bar <- mean(Data)
std_dev <- sd(Data)

# 95% CI
X_bar - qt(p=0.025,df=10-1,lower.tail=F)*(std_dev/sqrt(10))
X_bar + qt(p=0.025,df=10-1,lower.tail=F)*(std_dev/sqrt(10))

# 99% CI
X_bar - qt(p=0.005,df=10-1,lower.tail=F)*(std_dev/sqrt(10))
X_bar + qt(p=0.005,df=10-1,lower.tail=F)*(std_dev/sqrt(10))



## Ex9) Female height example - Estimation
150.5 - qnorm(p=0.025,mean=0,sd=1,lower.tail=F)*(sqrt(65)/sqrt(100))
150.5 + qnorm(p=0.025,mean=0,sd=1,lower.tail=F)*(sqrt(65)/sqrt(100))

150.5 - qnorm(p=0.005,mean=0,sd=1,lower.tail=F)*(sqrt(65)/sqrt(100))
150.5 + qnorm(p=0.005,mean=0,sd=1,lower.tail=F)*(sqrt(65)/sqrt(100))


## Ex10) Male height example - Testing
Data <- c(192,180,190,186,169,176,179,173,191,170)

X_bar <- mean(Data)
X_bar
std_dev <- sd(Data)
std_dev

# under H0, test statistic
t0 <- (X_bar-175)/(std_dev/sqrt(10))
t0
pt(t0,df=10-1,lower.tail=F)*2

# using t.test function
t.test(Data, mu=175, alternative="two.sided",conf.level=0.95)


## Ex11) Female height example - Testing

# under H0, test statistic
z0 <- (150.5-152)/(sqrt(65)/sqrt(100))
z0
pnorm(z0,mean=0,sd=1,lower.tail=T)*2





#-------- 3. Correlation analysis --------#

## Ex14)
Ex14_Data <- data.frame(X=c(28,26,31,30,32,29,27),Y=c(27,25,30,28,33,31,26))
Ex14_Data

# 1) scatter plot
plot(x=Ex14_Data$X,y=Ex14_Data$Y,pch=19,cex=2,xlab="temperature",ylab="Sales")

# 2) correlation coefficient
Cov <- mean((Ex14_Data$X-mean(Ex14_Data$X))*(Ex14_Data$Y-mean(Ex14_Data$Y)))
Cov
SSX <- sd(Ex14_Data$X)*sqrt(7-1)
SSX
SSY <- sd(Ex14_Data$Y)*sqrt(7-1)
SSY
Corr <- (Cov*7)/(SSX*SSY)
Corr

# 3) test
t0 <- sqrt(7-2)*Corr/sqrt(1-Corr^2)
t0
pt(t0,df=7-2,lower.tail=F)*2


# using cor.test function
cor.test(x=Ex14_Data$X,y=Ex14_Data$Y)



















