rm(list=ls())
library(dplyr)
library(tidyverse)
library(magrittr)
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
# tt(nn=1000)

ss <- as.data.frame(mapply(tt,nn=ns))
names(ss)<-ns
row.names(ss) <- mus


ss2 <- ss %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
head(ss2)

ss2$rowname <- as.character(ss2$rowname)
ss2$rowname <- factor(ss2$rowname,levels=unique(ss2$rowname))

ss2$colname <- as.character(ss2$colname)
ss2$colname <- factor(ss2$colname,levels=unique(ss2$colname))



ggplot(ss2, aes(x = rowname, y = colname, fill = value)) +
  geom_tile()
