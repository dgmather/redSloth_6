rm(list=ls())
library(dplyr)
library(tidyverse)
# library(reshape2)

ns <- seq(40,1000,24)

mus <- seq(-1,1,length.out=41)

h0 <- 0

obs <- rnorm(40,-5)
t.test(obs)

test <- function(n=40,mu=-1) {
  obs<- rnorm(n,mu)
  t <- t.test(obs)
  # return((mean(obs))
  # return(t$statistic)
  return(as.numeric(abs(t$statistic)>1.96))
  # return(cbind(mean(obs),t$statistic,as.numeric(abs(t$statistic)>1.96)))
}

# tt <- mapply(test,n=ns)

# pars <- cbind(40,-5)



 # reject(mu=0.50)
# tt<-mapply(reject,mu=mus)
 
tt <- function(nn,m=mus) {
  reject <- function(n=nn,mu=-1,s=100){
    sims <- replicate(s,test(n,mu))
    return(mean(sims))
  }
  v <- mapply(reject, m=mus)
  return(v)
} 
tt(nn=1000)

ss <- as.data.frame(mapply(tt,nn=ns))
names(ss)<-ns
row.names(ss) <- mus


ggplot(aes(x=mus,y=ns),data=ss) + geom_tile()
