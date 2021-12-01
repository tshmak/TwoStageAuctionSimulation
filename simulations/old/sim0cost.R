Tmisc()
setwd0("~/Dropbox/Apps/FanoTimMak/power_allocate/")
source.some("sim0.R", start='source("functions.R")', 
            end="###################", run=TRUE)

for(maxx in Maxx) {
  for(n in N) {
    for(prop in Prop) {
      for(k in K) {
        b <- seq(0, 1, length.out = n+1)[-1] 
        v <- delta(b)
        sumx <- maxx*n*prop
        x <- X2(v, sumx, maxx, k=k, round=TRUE)
        cost <- X2pay(b, sumx, maxx, k=k, round=FALSE, bmax=1)
        total_cost <- sum(cost)
        fill.in.results.table(b=b, k=k,n=n,prop=prop,maxx=maxx,sumx=sumx, 
                              cost=cost, total_cost=total_cost)
      }
      # x <- Xlinear(v, sumx, maxx, round=TRUE)
      # fill.in.results.table(v=v,x=x,k=0,n=n,prop=prop,maxx=maxx,sumx=sumx, 
      #                       n0=n0,cost=cost)
    }
  }
}

library(ggplot2)
results.table$nlabel <- paste0("n=", results.table$n)
for(var in c("prop", "k", "maxx", "maxx", "sumx")) {
  results.table[[var]] <- as.factor(results.table[[var]])
}
p <- ggplot(results.table, aes(x=b,y=cost, colour=prop)) + facet_grid(k~nlabel)
p + geom_line()

ss <- subset(results.table, v==1)
p2 <- ggplot(ss, aes(x=k, y=cost, group=prop, fill=prop)) + 
  facet_grid(nlabel~., scales="free_y")
p2 + ggbar()

p3 <- ggplot(ss, aes(x=k, y=n-n0, group=prop, fill=prop)) + 
  facet_grid(nlabel~., scales="free_y")
p3 + ggbar()
