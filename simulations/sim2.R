Tmisc()
setwd0("~/Dropbox/Apps/FanoTimMak/simulations")
filename <- Rfilename("sim2")
# dir.create("sim2")

opts <- parseargs()
if(is.null(opts$seed)) {
  seeds <- as.integer((as.numeric(Sys.time()) %% 86400) * 100)
} else {
  seeds <- eval(parse(text=opts$seed))
}

Tim.load("flexalloc", "..")

K <- c(0, 1,2,4,8)
N <- c(10, 100, 1000)
Prop <- c(0.1, 0.5)

sdlog <- 0.3
bmax <- qlnorm(0.99, sdlog=sdlog)
ffun <- function(x) dlnorm2(x, sdlog=sdlog, max=bmax)
Ffun <- function(x) plnorm2(x, sdlog=sdlog, max=bmax)
###################

for(s in 1:length(seeds)) {
  # s <- 1
  seed <- seeds[s]
  set.seed(seed)
  results.table <- data.frame()
  
  print.counter(s, text=paste0(seed,":"), max=length(seeds))
  
  for(n in N) {
    
    b <- rlnorm2(n, sdlog=sdlog, max=bmax)
    maxx <- rlnorm(n, sdlog=sdlog) * 100
    beta <- runif(n, 0.9, 1)
    
    m <- seed %% 10
    q <- NA
    if(m > 0) {
      q <- qlnorm2(m / 10, sdlog=sdlog, max=bmax)
      b[1] <- q 
      maxx[1] <- 100
      beta[1] <- 0.95
    }
    
    v <- delta(b,f=ffun, F=Ffun)
    
    
    for(prop in Prop) {
      for(k in K) {
        
        sumx <- n*100*prop
        x <- X2(v, sumx, maxx, k=k)
        pay <- MyersonPay(b, X2, sumx=sumx, maxx=maxx, k=k, f=ffun, F=Ffun, 
                          bmax=bmax, subset=1:min(100,n), interpolate=TRUE)
        vwel <- x*v
        fill.in.results.table(k=k,v=v,b=b,x=x,n=n,prop=prop,maxx=maxx,sumx=sumx, 
                              q=m/10, roi=(1:n)==1 & m > 0, beta=beta, 
                              pay=pay, vwel=vwel, seed=seed)
      }
      x <- Xlinear(v, sumx, maxx)
      pay <- MyersonPay(b, Xlinear, sumx=sumx, maxx=maxx, k=k, f=ffun, F=Ffun, 
                        bmax=bmax, subset=1:min(100,n), interpolate=TRUE)
      vwel <- x*v
      fill.in.results.table(k=Inf,v=v,b=b,x=x,n=n,prop=prop,maxx=maxx,sumx=sumx, 
                            q=m/10, roi=(1:n)==1 & m > 0, beta=beta, 
                            pay=pay, vwel=vwel, seed=seed)
    }
  }
  
  save(results.table, file=paste(paste0("sim2/", filename), 
                                 seed, "RData", sep="."))
}

