#' Testing if x_i(s) = x_j(s) if x_i(b_i) == x_j(b_j)
#' It seems they don't equal... 
#' Means that two persons can get different pay for equal amount of work

Tmisc()
setwd("~/Dropbox/Apps/FanoTimMak/simulations/")
Tim.load("flexalloc", "..")
n <- 5
v <- seq(0,1,len=n+1)[-1]
sumx <- 1
maxx <- rep(sumx*2/n, n)
x <- X2(v, sumx, maxx)
x2 <- x; x2[3] <- x2[4]; sumx2 <- sum(x2)
maxx2 <- maxx; maxx2[3] <- x2[4]
x <- X2(v, sumx2, maxx2) 
print(x) # I've contrived to make x[3] = x[4]

inc <- 0.001
rep <- 1000
result <- data.frame(v=rep(NA, rep), x3=NA, x4=NA)
# result[1,] <- x[3:4]
for(i in 1:rep) {
  v3 <- v
  v3[3] <- v3[3] + inc*(i-1)
  x3 <- X2(v3, sumx2, maxx2)
  result[i,"x3"] <- x3[3]
  
  result[i, "v"] <- v3[3]
  if(v3[3] < v[4]) next
  
  v4 <- v
  v4[4] <- v3[3]
  x4 <- X2(v4, sumx2, maxx2)
  result[i,"x4"] <- x4[4]
  
}
with(result, plot(v, x3, type="l"))
with(result, lines(v, x4, col="red"))

result$diff <- result$x4 - result$x3
with(result, plot(v, diff))
