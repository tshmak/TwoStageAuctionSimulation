Tmisc()
setwd0("~/Dropbox/FanoShare")
filename <- Rfilename("sim1", seed=1000)

source("functions.R")

# K <- c(0, 1,2,4,8)
K <- c(0, 2, 8)
# N <- c(10, 100, 1000)
N <- c(10, 100)
# Prop <- c(0.1, 0.3)
Prop <- c(0.3)
sdlog <- 0.3
bmax <- qlnorm(0.99, sdlog=sdlog)
ffun <- function(x) dlnorm2(x, sdlog=sdlog, max=bmax)
Ffun <- function(x) plnorm2(x, sdlog=sdlog, max=bmax)
repeats <- 100
###################

for(n in N) {
  
  for(rep in 1:repeats) {
    
    print.counter(rep, text="Repeat:",max=repeats)
    
    b <- rlnorm2(n, sdlog=sdlog, max=bmax)
    maxx <- rlnorm2(n, sdlog=sdlog, max=bmax) * 100
    
    m <- rep %% 10
    q <- NA
    if(m > 0) {
      q <- qlnorm(m / 10, sdlog=sdlog)
      b[1] <- q 
      maxx[1] <- 100
    }

    v <- delta(b,f=ffun, F=Ffun)
    
    
    for(prop in Prop) {
      for(k in K) {
        
        sumx <- sum(maxx)*prop
        x <- X2(v, sumx, maxx, k=k)
        pay <- MyersonPay(b, X2, sumx=sumx, maxx=maxx, k=k, f=ffun, F=Ffun, 
                          bmax=bmax, subset=1:min(100,n), interpolate=TRUE)
        vwel <- x*v
        fill.in.results.table(k=k,v=v,b=b,x=x,n=n,prop=prop,maxx=maxx,sumx=sumx, 
                              q=m/10, roi=(1:n)==1 & m > 0, 
                              pay=pay, vwel=vwel, rep=rep)
      }
      x <- Xlinear(v, sumx, maxx)
      pay <- MyersonPay(b, Xlinear, sumx=sumx, maxx=maxx, k=k, f=ffun, F=Ffun, 
                        bmax=bmax, subset=1:min(100,n), interpolate=TRUE)
      vwel <- x*v
      fill.in.results.table(k=Inf,v=v,b=b,x=x,n=n,prop=prop,maxx=maxx,sumx=sumx, 
                            q=m/10, roi=(1:n)==1 & m > 0, 
                            pay=pay, vwel=vwel, rep=rep)
    }
  }
}

results.table$direct <- results.table$x * results.table$b
results.table$util <- results.table$pay - results.table$direct
results.table$ratio <- results.table$vwel / results.table$pay
results.table$k <- as.factor(results.table$k)
# results.table[,c("k", "direct", "pay", "util", "vwel", "ratio")]

#### Summaries ####
library(data.table)
results <- as.data.table(results.table)
summary <- subset(results, q==0)[,list(totalpay=sum(pay), totalvwel=sum(vwel)), 
                     by=.(k,n,prop,rep)]
summary2 <- summary[,list(totalpay=mean(totalpay), totalvwel=mean(totalvwel)), 
                    by=.(k,n,prop)]
summary2[, baseline := sum(totalpay * (k==Inf)), by=.(n,prop)]
summary2[, inflation := totalpay / baseline]
roisumm <- subset(results, roi)[,list(pay=mean(pay), direct=mean(direct), q=mean(q)), 
                                by=.(k,n,prop,b)]
S <- lapply(0:5, function(indirect) cbind(roisumm, indirect=indirect))
roisumm2 <- do.call(rbind, S)
roisumm2$ROI <- with(roisumm2, pay/(direct+indirect))
roisumm2$ROI[!is.finite(roisumm2$ROI)] <- 0

#### Plots ####
library(ggplot2)
### Check vwel ~ pay
ggplot(summary, aes(x=totalpay, y=totalvwel)) + geom_point() + 
  geom_abline(slope=1,intercept = 0) + 
  facet_grid(n~k,scales = "free")

# ggplot(subset(summary, rep %% 10 == 0), aes(x=totalpay, y=totalvwel)) + geom_point() + 
#   geom_abline(slope=1,intercept = 0) + 
#   facet_wrap(~k,scales = "free")

### ROI vs bid by k
ggplot(roisumm2, aes(x=b, y=ROI, color=as.factor(indirect))) + geom_line() + 
  facet_grid(n~k)

### total cost vs k 
ggplot(summary2, aes(x=k, y=inflation)) + ggbar() + 
  facet_grid(n ~ prop)

roisumm2$smoothROI <- NA
roisumm2$smoothROI1 <- NA
for(nn in N) {
  for(pp in Prop) {
    for(kk in c(K, Inf)) {
      for(ii in unique(roisumm2$indirect)) {
        # nn <- N[1]; pp <- Prop[1]; kk <- K[2]; ii <- 5
        selected <- with(roisumm2, indirect==ii & k==kk & prop==pp & n==nn )
        ss <- subset(roisumm2, selected)
        sf <- monospline(ss$q, ss$ROI, k=length(ss$q))
        sf0 <- function(x) return(sf(x) - 1)
        roisumm2$smoothROI[selected] <- sf(ss$q)
        if(sf0(1) > 0) {
          roisumm2$smoothROI1[selected] <- 1
        } else if(sf0(0) < 0) {
          roisumm2$smoothROI1[selected] <- 0
        } else {
          roisumm2$smoothROI1[selected] <- uniroot(sf0, c(0,1))$root
        }
      }
    }
  }
}
ggplot(roisumm2, aes(x=b, y=smoothROI, color=as.factor(indirect))) + geom_line() + 
  facet_grid(n~k)

roisumm3 <- roisumm2[,list(ROI1=mean(smoothROI1)), by=.(k,n,prop,indirect)]
roisumm3 <- merge(roisumm3, summary2)
ggplot(roisumm3, aes(x=indirect, y=ROI1)) + ggbar() + 
  facet_grid(n~k)
ggplot(roisumm3, aes(x=totalpay, y=ROI1, colour=as.factor(indirect))) + 
  geom_line() + geom_point() + scale_x_log10() +
  facet_grid(. ~ n, scales="free")

# ggplot(roisumm3, aes(x=totalpay, y=ROI1, colour=as.factor(k), 
#                      group=as.factor(indirect))) + 
#   geom_line() + geom_point() + scale_x_log10() +
#   facet_grid(. ~ n, scales="free")
