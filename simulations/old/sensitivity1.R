Tmisc()
setwd0("~/Dropbox/Apps/FanoTimMak")
filename <- Rfilename("sensitivity1", seed="prompt")
1000

# dir.create("sensitivity1.results")
source("functions.R")

K <- c(0, 1,2,4,8)
N <- c(10, 100, 1000)
Prop <- c(0.1, 0.5)
Dip <- seq(0,1,by=0.1)
npoints <- 500

K <- 8; N <- 100; Prop <- 0.5 # Testing 

sdlog <- 0.3
bmax <- qlnorm(0.99, sdlog=sdlog)
ffun <- function(x) dlnorm2(x, sdlog=sdlog, max=bmax)
Ffun <- function(x) plnorm2(x, sdlog=sdlog, max=bmax)

# True values
Beta1 <- runif(1, 0.9, 1)
# B1 <- rlnorm2(1, sdlog=sdlog, max=bmax)
# B1 set to be dependent on prop. See within loop below. 
Maxx1 <- rlnorm(1, sdlog=sdlog) * 100

# Beta1 <- 0.9; B1 <- 0.8; Maxx1 <- 50 # Testing

B1hat <- c(0.1, bmax)
Maxx1hat <- c(1, 200)
Xhat1prop <- c(0.1, 1)

###################


for(n in N) {
  # n <- N[1]
  b <- rlnorm2(n, sdlog=sdlog, max=bmax)
  maxx <- rlnorm(n, sdlog=sdlog) * 100
  # beta <- runif(n, 0.9, 1)
  
  for(prop in Prop) {
    B1inv <- runif(1, 0, prop)
    B1 <- qlnorm2(B1inv, sdlog=sdlog, max=bmax)
    V1 <- B1 * Beta1 
    
    for(k in K) {
      for(dipp in Dip) {
        # prop <- Prop[1]; k <- K[1]; dipp <- 1
        cat("n=", n, ";prop=", prop, ";k=", k, ";dipp=", dipp, "\n")
        sumx <- n*100*prop
        params <- c(b1hat=B1, maxx1hat=Maxx1, xhat1prop=1)
        
        # Reference 
        util1_ref <- getutil(params, 
                             B1=B1, Maxx1=Maxx1, Beta1=Beta1, 
                             b=b, maxx=maxx, sumx=sumx, 
                             k=k, bmax=bmax, dipp=dipp, npoints=npoints)
        ref <- attributes(util1_ref)

        fill.in.results.table(k=k,n=n,prop=prop, dipp=dipp,
                              B1=B1,Beta1=Beta1,V1=V1,Maxx1=Maxx1,
                              xhat1=ref$xhat1, xhat1prop=1, 
                              b1hat=B1, maxx1hat=Maxx1, 
                              x1=ref$x1, pay1=ref$pay1, dw1=ref$dw1, 
                              actualpay1=ref$actualpay1, 
                              cost1=ref$cost1, util1=util1_ref, ref=TRUE)
        
        # Best
        O <- optim(params, getutil, 
                   B1=B1, Maxx1=Maxx1, Beta1=Beta1, 
                   b=b, maxx=maxx, sumx=sumx, 
                   k=k, bmax=bmax, dipp=dipp, npoints=npoints, 
                   method="L-BFGS-B", 
                   lower=c(B1hat[1], Maxx1hat[1], Xhat1prop[1]), 
                   upper=c(B1hat[2], Maxx1hat[2], Xhat1prop[2]), 
                   control=list(fnscale=-1))
        
        util1_best <- getutil(O$par, 
                             B1=B1, Maxx1=Maxx1, Beta1=Beta1, 
                             b=b, maxx=maxx, sumx=sumx, 
                             k=k, bmax=bmax, dipp=dipp, npoints=npoints)
        best <- attributes(util1_best)
        best_params <- O$par
        
        fill.in.results.table(k=k,n=n,prop=prop, dipp=dipp,
                              B1=B1,Beta1=Beta1,V1=V1,Maxx1=Maxx1,
                              xhat1=best$xhat1, xhat1prop=best_params["xhat1prop"], 
                              b1hat=best_params["b1hat"], maxx1hat=best_params["maxx1hat"], 
                              x1=best$x1, pay1=best$pay1, dw1=best$dw1, 
                              actualpay1=best$actualpay1, 
                              cost1=best$cost1, util1=util1_best, ref=FALSE)
        
      }
    }
  }
}

save(results.table, file=paste0("sensitivity1.results/", filename, ".RData"))
# load("sensitivity1.3835830.RData")
