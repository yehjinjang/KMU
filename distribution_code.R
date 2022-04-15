
### T-dist with df=3 ###
df_n <- 100
t <- seq(-5,5,by=0.01)
ft <- dt(x=t,df=df_n)
plot(x=t,y=ft,type="l",lwd=5,col="red",ylim=c(0,0.5))
abline(v=0)


### Chisqared-dist with df=3 ###
df_n <- 100
Q <- seq(0,10,by=0.01)
fchisq <- dchisq(x=Q,df=df_n)
plot(x=Q,y=fchisq,type="l",lwd=5,col="red",ylim=c(0,1))
