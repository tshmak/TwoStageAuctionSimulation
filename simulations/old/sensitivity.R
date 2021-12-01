Tmisc()
setwd0("~/Dropbox/Apps/FanoTimMak")
filename <- Rfilename("sensitivity", seed="prompt")
# dir.create("sensitivity.results")

source("functions.R")

K <- c(0, 1,2,4,8)
N <- c(10, 100, 1000)
Prop <- c(0.1, 0.5)

# K <- 8; N <- 100; Prop <- 0.5

# True values
Beta1 <- 0.9
B1 <- 0.8
V1 <- B1 * Beta1 
Maxx1 <- 50
Dip <- seq(0,1,by=0.1)

B1hat <- sort(unique(c(B1, seq(B1/2, B1*2, len=10))))
Maxx1hat <- sort(unique(c(Maxx1, seq(Maxx1/2, Maxx1*2, len=10))))
Xhat1prop <- seq(0.5, 1, len=10)

downweight <- function(x, Maxx1, dipp) {
  dipstart <- Maxx1*dipp
  Max <- Maxx1 + (Maxx1 - dipstart)
  stopifnot(x >= 0)
  slope <- -1/2 / (Maxx1 - dipstart)
  result <- ifelse(x <= dipstart, 1, 
                   ifelse(x >= Max, 0, 1 + (x - dipstart)* slope))
  return(result)
}
# y <- downweight(x <- seq(0, 30, len=100), Maxx1=10, dipp=0.1); plot(x,y, type="l")
# y <- downweight(x, Maxx1=10, dipp=1); lines(x,y)


sdlog <- 0.3
bmax <- qlnorm(0.99, sdlog=sdlog)
ffun <- function(x) dlnorm2(x, sdlog=sdlog, max=bmax)
Ffun <- function(x) plnorm2(x, sdlog=sdlog, max=bmax)
###################

for(n in N) {
  
  b <- rlnorm2(n, sdlog=sdlog, max=bmax)
  maxx <- rlnorm(n, sdlog=sdlog) * 100
  beta <- runif(n, 0.9, 1)
  
  for(b1hat in B1hat) {
    for(maxx1hat in Maxx1hat) {
      b[1] <- b1hat
      maxx[1] <- maxx1hat
      beta[1] <- Beta1
      
      delt <- delta(b,f=ffun, F=Ffun)

      for(prop in Prop) {
        sumx <- n*100*prop
        for(k in K) {
          
          x1 <- X2(delt, sumx, maxx, k=k)[1]
          pay1 <- MyersonPay(b, X2, sumx=sumx, maxx=maxx, k=k, f=ffun, F=Ffun, 
                            bmax=bmax, subset=1)[1]
          
          for(xhat1prop in Xhat1prop) {
            ideal <- xhat1prop == 1 & b1hat == B1 & maxx1hat == Maxx1
            xhat1 <- x1 * xhat1prop
            cost1 <- xhat1*V1
            dw1 <- downweight(xhat1, Maxx1 = Maxx1, dipp = Dip)
            actualpay1 <- pay1 * xhat1prop * beta[1]
            util1 <- dw1 * (actualpay1 - cost1)
            fill.in.results.table(k=k,n=n,prop=prop, dipp=Dip,
                                  B1=B1,Beta1=Beta1,V1=V1,Maxx1=Maxx1,
                                  xhat1=xhat1, xhat1prop=xhat1prop, 
                                  b1hat=b1hat, maxx1hat=maxx1hat, 
                                  x1=x1, pay1=pay1, dw1=dw1, actualpay1=actualpay1, 
                                  cost1=cost1, util1=util1, ideal=ideal)
          }

          
        }
        x1 <- Xlinear(delt, sumx, maxx)[1]
        pay1 <- MyersonPay(b, Xlinear, sumx=sumx, maxx=maxx, k=k, f=ffun, F=Ffun, 
                          bmax=bmax, subset=1)[1]
        for(xhat1prop in Xhat1prop) {
          ideal <- xhat1prop == 1 & b1hat == B1 & maxx1hat == Maxx1
          xhat1 <- x1 * xhat1prop
          cost1 <- xhat1*V1
          dw1 <- downweight(xhat1, Maxx1 = Maxx1, dipp = Dip)
          actualpay1 <- pay1 * xhat1prop * beta[1]
          util1 <- dw1 * (actualpay1 - cost1)
          fill.in.results.table(k=Inf,n=n,prop=prop, dipp=Dip,
                                B1=B1,Beta1=Beta1,V1=V1,Maxx1=Maxx1,
                                xhat1=xhat1, xhat1prop=xhat1prop, 
                                b1hat=b1hat, maxx1hat=maxx1hat, 
                                x1=x1, pay1=pay1, dw1=dw1, actualpay1=actualpay1, 
                                cost1=cost1, util1=util1, ideal=ideal)
        }
      }
    }
  }
}

save(results.table, file=paste0(filename, ".RData"))
# load("sensitivity.3835830.RData")

library(data.table)
results <- as.data.table(results.table)
results[, max_util1 := max(util1), by=.(n,prop,k,dipp,B1,Beta1,Maxx1)]
results[, regret := max_util1 - util1]
results[, best_params := paste0("x1p=", signif(xhat1prop, 2), 
                                ",b1h=", signif(b1hat, 2),
                                ",m1h=", signif(maxx1hat, 2))[
  max_util1==util1][1], by=.(n,prop,k,dipp,B1,Beta1,Maxx1)]

control <- subset(results, dipp==1)
best <- subset(results, max_util1 %==% util1)
control_best <- subset(best, dipp==1)

Ideal <- subset(results, ideal)
control_ideal <- subset(Ideal, dipp==1)

# print(control)
print(control_ideal)
# print(control_best)

# Graphs 
# library(ggplot2)
# ggplot(subset(control, k==Inf), aes(x=b1hat, y=util1, colour=as.factor(maxx1hat))) +
#   geom_line() + facet_wrap(~xhat1prop)
# 
# ggplot(subset(control, k==8), aes(x=b1hat, y=util1, colour=as.factor(maxx1hat))) +
#   geom_line() + facet_wrap(~xhat1prop)

dipp0.9_best <- subset(best, dipp==0.9)
ideal0.9 <- subset(Ideal, dipp==0.9)
ideal0.1 <- subset(Ideal, dipp==0.1)