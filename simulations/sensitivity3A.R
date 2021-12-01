Tmisc()
setwd0("~/Dropbox/Apps/FanoTimMak/simulations")
filename <- Rfilename("sensitivity3A", seed="prompt")
1000

# dir.create("sensitivity3A")
devtools::load_all("../flexalloc")
library(optimx)

K <- c(0, 1,2,4,8, Inf)
N <- c(10, 100, 1000)
Prop <- c(0.1, 0.5)
Slopes <- c(-4, -2, -1, -0.5, -0.25)
npoints <- 500

# K <- c(8,Inf); Slopes <- c(0, 0.5, 1) # Testing
# K <- c(Inf); Slopes <- c(0.8) # Testing

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
Maxx1hat <- c(1, Inf)
Xhat1prop <- c(0.1, 1)

###################

n <- sample(rep(N, 2), 1) # The rep is to prevent unexpected behavior if N is length 1
prop <- sample(rep(Prop,2), 1)
# n <- N[1]
b <- rlnorm2(n, sdlog=sdlog, max=bmax)
maxx <- rlnorm(n, sdlog=sdlog) * 100
# beta <- runif(n, 0.9, 1)

# prop <- Prop[1]
B1inv <- runif(1, 0, prop)
B1 <- qlnorm2(B1inv, sdlog=sdlog, max=bmax)
V1 <- B1 * Beta1

for(k in K) {
  for(slope in Slopes) {

    # k <- K[1]; slope <- -1
    cat("n=", n, ";prop=", prop, ";k=", k, ";slope=", slope, "\n")
    sumx <- n*100*prop
    params <- c(b1hat=B1, maxx1hat=Maxx1, xhat1prop=1)

    # Reference
    util1_ref <- getutil2(params,
                         V1=V1, Maxx1=Maxx1, Beta1=Beta1,
                         b=b, maxx=maxx, sumx=sumx,
                         k=k, bmax=bmax, slope=slope, npoints=npoints)
    ref <- attributes(util1_ref)

    fill.in.results.table(k=k,n=n,prop=prop, slope=slope,
                          B1=B1,Beta1=Beta1,V1=V1,Maxx1=Maxx1,
                          xhat1=ref$xhat1, xhat1prop=1,
                          b1hat=B1, maxx1hat=Maxx1,
                          x1=ref$x1, pay1=ref$pay1, dw1=ref$dw1,
                          actualpay1=ref$actualpay1,
                          cost1=ref$cost1, util1=util1_ref, ref=TRUE)

    # Best
    getutil2_ <- function(...) {
      res <- getutil2(...)
      attributes(res) <- NULL
      return(-res)
    }
    suppressWarnings({
      O <- optimx(params, getutil2_,
                  V1=V1, Maxx1=Maxx1, Beta1=Beta1,
                  b=b, maxx=maxx, sumx=sumx,
                  k=k, bmax=bmax, slope=slope, npoints=npoints,
                  method="bobyqa",
                  lower=c(B1hat[1], Maxx1hat[1], Xhat1prop[1]),
                  upper=c(B1hat[2], Maxx1hat[2], Xhat1prop[2]))
    })

    best_params <- t(O[1, c("b1hat", "maxx1hat", "xhat1prop")])[,1]
    util1_best <- getutil2(best_params,
                         V1=V1, Maxx1=Maxx1, Beta1=Beta1,
                         b=b, maxx=maxx, sumx=sumx,
                         k=k, bmax=bmax, slope=slope, npoints=npoints)
    
    # Test if the same could be achieved by the reference. 
    for(par in c("b1hat", "maxx1hat", "xhat1prop")) {
      if(best_params[par] %==% params[par]) next
      
      params2 <- best_params; params2[par] <- params[par]
      util1_test <- getutil2(params2,
                            V1=V1, Maxx1=Maxx1, Beta1=Beta1,
                            b=b, maxx=maxx, sumx=sumx,
                            k=k, bmax=bmax, slope=slope, npoints=npoints)
      if(util1_test %==% util1_best) {
        best_params <- params2
        util1_best <- util1_test
      }
    }
    
    best <- attributes(util1_best)

    fill.in.results.table(k=k,n=n,prop=prop, slope=slope,
                          B1=B1,Beta1=Beta1,V1=V1,Maxx1=Maxx1,
                          xhat1=best$xhat1, xhat1prop=best_params["xhat1prop"],
                          b1hat=best_params["b1hat"], maxx1hat=best_params["maxx1hat"],
                          x1=best$x1, pay1=best$pay1, dw1=best$dw1,
                          actualpay1=best$actualpay1,
                          cost1=best$cost1, util1=util1_best, ref=FALSE)

  }
}

save(results.table, file=paste0(attr(filename, "filestub"), "/", 
                                filename, ".RData"))
