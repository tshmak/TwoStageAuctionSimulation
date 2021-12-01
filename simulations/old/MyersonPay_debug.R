MyersonPay <- function(b, alloc.fun, ..., f=dunif, F=punif, bmax=1, 
                       subset=1:length(b), interpolate=FALSE, 
                       npoints=200) {
  n <- length(b)
  stopifnot(b <= bmax)
  int1 <- int2 <- b*NA
  v <- delta(b, f=f, F=F)
  x0 <- alloc.fun(v, ...)

  if(interpolate) {
    # Get the max and min x first 
    i_min <- which(x0 == min(x0))[1]
    i_max <- which(x0 == max(x0))[1]
    subset <- union(subset, c(i_min, i_max))
  }
  
  #' I'm using fixed quadrature points to ensure that the integration is 
  #' monotonic with respect to the lower limit. 
  qpoints <- seq(0, bmax, length=npoints)

  
  for(i in subset) {
    b2 <- b
    p <- (bmax-b[i])/(bmax - min(b))
    
    gt <- which(qpoints > b[i])
    if(length(gt) == 0) {
      if(b[i] == bmax) gt1 <- bmax else 
        stop(paste0("b[",i,"] is greater than bmax."))
    } else {
      gt1 <- c(min(gt)-1, gt)
    }
    y <- qpoints[gt1]
    np <- length(y)
    x <- sapply(y, function(yi, b2) {
      b2[i] <- yi
      v2 <- delta(b2, f=f, F=F)
      x <- alloc.fun(v2, ...)
      return(x[i])
    }, b2)
    # Integration by trapezium rule
    if(np >= 2) {
      d <- y[2] - y[1]
      z <- (b[i] - y[1]) / d
      xi <- x[1]*(1-z) + x[2]*z # Interpolated x
      int1[i] <- xi * b[i]
      int2[i] <- d*(1-z) * (xi+x[2])/2
      if(np > 2) {
        int2[i] <- int2[i] + d*(sum(x[-1]) - 0.5*(x[2]+x[np]))
      }
    } else {
      int2[i] <- 0
      int1[i] <- bmax * x[1]
    }
  }
  pay <- int1 + int2
  if(interpolate) {
    if(any(is.na(pay))) {
      ok <- !is.na(pay)
      # sf <- splinefun(x=x0[ok], y=pay[ok])
      sf <- monospline(x=x0[ok], y=pay[ok])
      pay[!ok] <- sf(x0[!ok])
    }
  }
  attr(pay, "int2") <- data.frame(x=x,y=y)
  return(pay)
}

# Probs... 
prob <- subset(control, k==Inf & dipp==1)
ggplot(prob, aes(x=b1hat, y=x1, colour=as.factor(maxx1hat))) + geom_line()

test <- subset(prob, maxx1hat == Maxx1 & xhat1prop == 1)
npoints <- 200
b2 <- b
b2[1] <- 0.8; pay0.8 <- MyersonPay(b2, Xlinear, sumx=sumx, maxx=maxx, k=k, f=ffun, F=Ffun, 
                                   bmax=bmax, subset=1, npoints=npoints); x0.8 <- attr(pay0.8, "int2")
delt <- delta(b2,f=ffun, F=Ffun)
X0.8 <- Xlinear(delt, sumx, maxx)[1]
np <- length(x0.8)

b2[1] <- 0.933; pay0.933 <- MyersonPay(b2, Xlinear, sumx=sumx, maxx=maxx, k=k, f=ffun, F=Ffun, 
                                   bmax=bmax, subset=1, npoints=npoints); x0.933 <- attr(pay0.933, "int2")
delt <- delta(b2,f=ffun, F=Ffun)
X0.933 <- Xlinear(delt, sumx, maxx)[1]
np <- length(x0.933)

pay0.933[1]; pay0.8[1]
