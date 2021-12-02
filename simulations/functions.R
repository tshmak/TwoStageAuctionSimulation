#' Functions to perform allocation
library(quadprog)
X1 <- function(v, sumx=1, maxx=sumx, k=2, round=FALSE) {
  n <- length(v)
  stopifnot(v > 0)
  stopifnot(maxx > 0)
  v <- v^k
  if(length(maxx) == 1) maxx <- rep(maxx, n)
  stopifnot(length(maxx) == n)

  Dmat <- diag(v)
  dvec <- rep(0, n)
  Amat <- t(rbind(rep(1,n),
                  -diag(n)))
  bvec <- c(sumx, -maxx)

  s <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
  x <- s$solution

  if(round) x <- round(x)
  return(x)
}

X2 <- function(v, sumx=1, maxx=sumx, k=2, round=FALSE) {
  n <- length(v)
  stopifnot(v > 0)
  stopifnot(maxx > 0)
  v <- v^k
  if(length(maxx) == 1) maxx <- rep(maxx, n)
  stopifnot(length(maxx) == n)

  x <- rep(0,n)
  while(TRUE) {
    notmax <- x < maxx
    v2 <- v[notmax]
    v2inv <- 1/v2
    dist <- sumx * v2inv / sum(v2inv)
    exceeds <- dist >= maxx[notmax]

    if(any(exceeds)) {
      toupdate <- notmax
      toupdate[toupdate] <- exceeds
      x[toupdate] <- maxx[toupdate]
      sumx <- sumx - sum(maxx[toupdate])
    } else {
      x[notmax] <- dist
      break
    }
  }
  if(round) x <- round(x)
  return(x)
}

Xlinear <- function(v, sumx=1, maxx=sumx, ..., round=FALSE) {
  n <- length(v)
  stopifnot(v > 0)
  stopifnot(maxx > 0)
  if(length(maxx) == 1) maxx <- rep(maxx, n)
  stopifnot(length(maxx) == n)
  stopifnot(sum(maxx) >= sumx)

  # n <- 10; v <- ceiling(runif(n) * 8); maxx <- ceiling(runif(n) * 2); sumx <- sum(maxx)*0.6
  xo <- rep(0, n)

  o <- order(v)
  oo <- order(o) # To reverse the ordering

  vo <- v[o]
  maxxo <- maxx[o]

  ro <- rank(vo, ties.method = "min")
  tiedo <- ro == c(ro[-1], 0) | ro == c(0, ro[-length(ro)])
  cmaxxo <- cumsum(maxxo)

  within_quota <- cmaxxo <= sumx
  if(all(within_quota == FALSE)) {
    xo[1] <- sumx
  } else {
    within_quota_max <- max(which(within_quota))
    if(within_quota_max == n) {
      xo <- maxxo
    } else if(ro[within_quota_max] == ro[within_quota_max+1]) {
      full_assign <- ro < ro[within_quota_max]
      xo[full_assign] <- maxxo[full_assign]
      split_group <- ro == ro[within_quota_max]
      remaining <- sumx - sum(xo)
      xo[split_group] <- waterfill(remaining, maxxo[split_group])
    } else {
      full_assign <- ro <= ro[within_quota_max]
      xo[full_assign] <- maxxo[full_assign]
      remaining <- sumx - sum(xo)
      if(remaining > 0) {
        next_group_ro <- min(ro[(1:n) > within_quota_max])
        split_group <- ro == next_group_ro
        xo[split_group] <- waterfill(remaining, maxxo[split_group])
      }
    }
  }

  x <- xo[oo]
  return(x)
}

delta <- function(b, f=dunif, F=punif, ...) {
  return(b + F(b, ...)/f(b, ...))
}

X2pay <- function(b, ..., f=dunif, F=punif, bmax=1, npoints=100) {
  # Deprecated. Use MyersonPay() instead
  stop("X2pay may be wrong. Use MyersonPay() instead")
  n <- length(b)
  stopifnot(b <= bmax)
  int1 <- int2 <- b*NA
  v <- delta(b, f=f, F=F)
  x0 <- X2(v, ...)
  int1 <- b*x0

  for(i in 1:n) {
    v2 <- v
    y <- seq(v[i], bmax, len=npoints)[]
    x <- sapply(y, function(yi, v2) {
      v2[i] <- yi
      x <- X2(v2, ...)
      return(x[i])
    }, v2)
    int2[i] <- sum(x) / npoints * (bmax - b[i])
  }
  pay <- int1 + int2
  return(pay)
}

waterfill <- function(water, capacity) {
  # water <- 10; capacity <- c(1,2,2,4,5)
  x <- capacity * 0
  v <- unique(capacity)
  vs <- sort(v)
  stopifnot(water <= sum(capacity))
  steps <- vs - c(0,vs[-length(vs)])
  for(i in 1:length(vs)) {
    # i <- 1
    vss <- vs[i]
    s <- steps[i]
    tofill <- capacity >= vss
    n <- sum(tofill)
    total <- n * s
    remaining <- water - sum(x)
    if(remaining < total) {
      x[tofill] <- x[tofill] + remaining / n
      break
    } else {
      x[tofill] <- x[tofill] + s
    }
  }
  return(x)
}

# Fit a monotonic spline
# Taken from https://stats.stackexchange.com/questions/197509/how-to-smooth-data-and-force-monotonicity
library(scam)
monospline <- function(x,y, bs="mpd", ...) {
  #' bs="mpd" for descending; bs="mpi" for ascending
  dat <- data.frame(x=x,y=y)
  sc <- scam(y ~ s(x, bs=bs, ...), data = dat)
  f <- function(x0) {
    return(predict(sc, data.frame(x=x0)))
  }
  return(f)
}
# Example:
# df <- data.frame(x=1:10, y=c(100,41,22,10,6,7,2,1,3,1))
# plot(df$x,df$y)
# ms <- monospline(df$x, df$y)
# x0 <- seq(1,10, len=100)
# lines(x0, ms(x0))

dlnorm2 <- function(..., max) {
  P <- plnorm(q=max, ...)
  return(dlnorm(...)/P)
}

plnorm2 <- function(..., max) {
  P <- plnorm(q=max, ...)
  return(plnorm(...)/P)
}

qlnorm2 <- function(p, ..., max) {
  P <- plnorm(q=max, ...)
  return(qlnorm(P*p, ...))
}

rlnorm2 <- function(n, ..., max) {
  u <- runif(n)
  return(qlnorm2(u, ..., max=max))
}

#' MyersonPay <- function(b, alloc.fun, ..., f=dunif, F=punif, bmax=1,
#'                        subset=1:length(b), interpolate=FALSE,
#'                        qpoints=200) {
#'   n <- length(b)
#'   stopifnot(b <= bmax)
#'   int1 <- int2 <- b*NA
#'   v <- delta(b, f=f, F=F)
#'   x0 <- alloc.fun(v, ...)
#'   int1 <- b*x0
#'
#'   if(interpolate) {
#'     # Get the max and min x first
#'     i_min <- which(x0 == min(x0))[1]
#'     i_max <- which(x0 == max(x0))[1]
#'     subset <- union(subset, c(i_min, i_max))
#'   }
#'
#'   if(length(qpoints) == 1) {
#'     #' I'm using fixed quadrature points to ensure that the integration is
#'     #' monotonic with respect to the lower limit.
#'     qpoints <- seq(0, bmax, length=qpoints)
#'   }
#'
#'
#'   for(i in subset) {
#'     b2 <- b
#'     p <- (bmax-b[i])/(bmax - min(b))
#'     y <- c(b[i], qpoints[qpoints > b[i]])
#'     np <- length(y)
#'     x <- sapply(y, function(yi, b2) {
#'       b2[i] <- yi
#'       v2 <- delta(b2, f=f, F=F)
#'       x <- alloc.fun(v2, ...)
#'       return(x[i])
#'     }, b2)
#'     # Integration by trapezium rule
#'     if(np >= 2) {
#'       d <- y[-1] - y[-np]
#'       int2[i] <- d[1] * (x[1]+x[2])/2
#'       if(np > 2) {
#'         int2[i] <- int2[i] + d[2]*(sum(x[-1]) - 0.5*(x[2]+x[np]))
#'       }
#'     } else int2[i] <- 0
#'   }
#'   pay <- int1 + int2
#'   if(interpolate) {
#'     if(any(is.na(pay))) {
#'       ok <- !is.na(pay)
#'       # sf <- splinefun(x=x0[ok], y=pay[ok])
#'       sf <- monospline(x=x0[ok], y=pay[ok], bs="mpi")
#'       pay[!ok] <- sf(x0[!ok])
#'     }
#'   }
#'   # attr(pay, "int2") <- x
#'   return(pay)
#' }

MyersonPay <- function(b, alloc.fun, ..., f=dunif, F=punif, bmax=1,
                       subset=1:length(b), interpolate=FALSE,
                       npoints=200) {
  n <- length(b)
  stopifnot(b <= bmax)
  x0i <- int1 <- int2 <- b*NA
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

    gt <- which(qpoints > b[i])
    if(length(gt) > 0) {
      gt1 <- c(min(gt)-1, gt)
      y <- qpoints[gt1]
    } else if(b[i] == bmax) {
      y <- bmax
    } else {
      stop(paste0("b[",i,"] is greater than bmax."))
    }
    np <- length(y)
    x <- sapply(y, function(yi, b2) {
      b2[i] <- yi
      v2 <- delta(b2, f=f, F=F)
      x <- alloc.fun(v2, ...)
      return(x[i])
    }, b)
    # Integration by trapezium rule
    if(np >= 2) {
      d <- y[2] - y[1]
      z <- (b[i] - y[1]) / d
      x0i[i] <- x[1]*(1-z) + x[2]*z # x0i[i]: Interpolated x
      int1[i] <- x0i[i] * b[i]
      int2[i] <- d*(1-z) * (x0i[i]+x[2])/2
      if(np > 2) {
        int2[i] <- int2[i] + d*(sum(x[-1]) - 0.5*(x[2]+x[np]))
      }
    } else {
      int2[i] <- 0
      x0i[i] <- x[1]
      int1[i] <- bmax * x[1]
    }
  }
  pay <- int1 + int2
  if(interpolate) {
    if(any(is.na(pay))) {
      ok <- !is.na(pay)
      # sf <- splinefun(x=x0[ok], y=pay[ok])
      sf <- monospline(x=x0[ok], y=pay[ok], bs="mpi")
      pay[!ok] <- sf(x0[!ok])
    }
  }
  # attr(pay, "int2") <- data.frame(x=x,y=y)
  attributes(pay) <- list(x0=x0, x0i=x0i)
  return(pay)
}

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
getutil <- function(params, ..., npoints=200) {

  # Options needed: V1, Maxx1, Beta1, b, maxx, sumx, k, bmax, dipp
  o <- list(...)

  o$b[1] <- params["b1hat"]
  o$maxx[1] <- params["maxx1hat"]
  stopifnot(is.finite(c(o$b, o$maxx, o$beta)))
  xhat1prop <- params["xhat1prop"]
  names(xhat1prop) <- NULL

  util1 <- with(o, {
    delt <- delta(b,f=ffun, F=Ffun)

    if(k < Inf) {
      Pay <- MyersonPay(b, X2, sumx=sumx, maxx=maxx, k=k, f=ffun, F=Ffun,
                         bmax=bmax, subset=1, npoints=npoints)
    } else {
      Pay <- MyersonPay(b, Xlinear, sumx=sumx, maxx=maxx, k=k, f=ffun, F=Ffun,
                         bmax=bmax, subset=1, npoints=npoints)
    }

    pay1 <- Pay[1]
    # x1 <- attr(Pay, "x0")[1]
    x1 <- attr(Pay, "x0i")[1] # Use interpolated x rather than real x to enforce monotonicity of util on b
    x1real <- attr(Pay, "x0")[1]

    xhat1 <- x1 * xhat1prop
    cost1 <- xhat1*V1
    # dw1 <- downweight(xhat1, Maxx1 = Maxx1, dipp = dipp)
    dw1 <- downweight(x1real*xhat1prop, Maxx1=Maxx1, dipp=dipp) # We need to use the real x for downweight, however, to avoid mis-adjustment

    actualpay1 <- pay1 * xhat1prop * Beta1
    util1 <- dw1 * (actualpay1 - cost1)

    details <- list(x1=x1, x1real=x1real, xhat1=xhat1, cost1=cost1, dw1=dw1, pay1=pay1,
                    actualpay1=actualpay1)
    attributes(util1) <- details
    util1
  } )

  return(util1)
}
