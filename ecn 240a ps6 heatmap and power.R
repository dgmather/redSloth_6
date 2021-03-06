rm(list = ls())
cat("\014")

library(tidyverse)
library(tidyr)

n <- (2:30)^2
u <- seq(-1,1,length.out = length(n))

g <- function(n, u, s) {
  R <- c()
  for (i in 1:s) {
    y <- rnorm(n, u, 1)
    t <- t.test(y, alternative = "two.sided")
    r <- t$p.value < 0.05
    R <- c(R,r)
  }
  return(mean(R))
}

G <- mapply(g,rep(n,29),rep(u,each=29),100)
M <- cbind(rep(n,29), rep(u, each = 29), G)
M <- as.data.frame(M)
colnames(M) <- c("n", "u", "G")

ggplot(data = M, aes(x = factor(u), y = factor(n))) + geom_tile(aes(fill = G)) + theme(axis.text = element_blank()) + xlab(expression(mu)) + ylab("n") + scale_fill_continuous(guide = guide_legend(title = "% reject"), limits=c(0, 1), breaks=seq(0,1,by=0.05))

U <- reshape(M, idvar = "u", direction = "wide", timevar = "n")
colnames(U) <- c("u",as.character(n))

ggplot(data = U) +
  geom_line(aes(x=u, y=`16`), linetype = "dashed", color = "red") + 
  geom_line(aes(x=u, y = 1 - pnorm(1.96 - 4*u) + pnorm(-1.96 - 4*u))) +
  geom_line(aes(x=u, y=`25`), linetype = "dashed", color = "green") + 
  geom_line(aes(x=u, y = 1 - pnorm(1.96 - 5*u) + pnorm(-1.96 - 5*u))) +
  geom_line(aes(x=u, y=`36`), linetype = "dashed", color = "blue") + 
  geom_line(aes(x=u, y = 1 - pnorm(1.96 - 6*u) + pnorm(-1.96 - 6*u))) + 
  ylab(expression(pi)) + xlab(expression(mu))

N <- reshape(M, idvar = "n", direction = "wide", timevar = "u")
colnames(N) <- c("n", as.character(1:29))

ggplot(data = N) +
  geom_line(aes(x=n, y=`17`), linetype = "dashed", color = "red") + 
  geom_line(aes(x=n, y = 1 - pnorm(1.96 - sqrt(n)*u[17]) + pnorm(-1.96 - sqrt(n)*u[17]))) +
  geom_line(aes(x=n, y=`18`), linetype = "dashed", color = "green") + 
  geom_line(aes(x=n, y = 1 - pnorm(1.96 - sqrt(n)*u[18]) + pnorm(-1.96 - sqrt(n)*u[18]))) +
  geom_line(aes(x=n, y=`1`), linetype = "dashed", color = "blue") + 
  geom_line(aes(x=n, y = 1 - pnorm(1.96 - sqrt(n)*u[1]) + pnorm(-1.96 - sqrt(n)*u[1]))) +
  ylab(expression(pi))
