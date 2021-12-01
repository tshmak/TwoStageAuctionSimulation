#' Actually the results from this analysis may not be accurate because the optimization algorithm in sensitivity2.R 
#' may not have converged 

Tmisc()
setwd0("~/Dropbox/Apps/FanoTimMak/simulations")
filename <- Rfilename("sensitivity2graphs")
load("sensitivity2results.RData")

library(data.table)
results <- as.data.table(results.table)
results[, ID := ceiling((1:nrow(results))/2)]
results[, max_util1 := max(util1), by=.(n,prop,k,dipp,B1,Beta1,Maxx1)]
results[, regret := max_util1 - util1]
results[, b1inf := b1hat / B1]
results[, maxx1inf := maxx1hat / Maxx1]
results[, ref_is_best := (util1[1] %==% util1[2]), 
        by=.(n,prop,k,dipp,B1,Beta1,Maxx1)]

results[, best_params := paste0("x1p=", signif(xhat1prop, 2), 
                                ",b1h=", signif(b1hat, 2),
                                ",m1h=", signif(maxx1hat, 2))[!ref], 
        by=.(n,prop,k,dipp,B1,Beta1,Maxx1)]
results[, best_params_inf := paste0("x1p=", signif(xhat1prop, 2), 
                                ",b1h=", signif(b1inf, 2),
                                ",m1h=", signif(maxx1inf, 2))[!ref], 
        by=.(n,prop,k,dipp,B1,Beta1,Maxx1)]


control <- subset(results, dipp==1)
best <- subset(results, !ref)
control_best <- subset(best, dipp==1)

ref <- subset(results, ref)
control_ref <- subset(ref, dipp==1)

# print(control)
# print(control_ref)
# print(control_best)
summary(control_ref$regret)

ref_not_best <- subset(best, !ref_is_best)

# probs <- subset(ref_not_best, b1inf < 0.99)$ID
# subset(results, ID %in% probs)
# stop()

library(ggplot2)
ggplot(ref_not_best, aes(x=as.factor(dipp), y=b1inf, colour=as.factor(k))) + 
  geom_jitter(size=0.2) + facet_grid(n~prop) + 
  scale_colour_brewer(palette="RdGy") + theme_classic()
ggplot(ref_not_best, aes(x=as.factor(dipp), y=maxx1inf, colour=as.factor(k))) + 
  geom_jitter(size=0.2) + facet_grid(n~prop) + theme_classic()
ggplot(ref_not_best, aes(x=as.factor(dipp), y=xhat1prop, colour=as.factor(k))) + 
  geom_jitter(size=0.2) + facet_grid(n~prop) + theme_classic()

summary <- results[, .(ref_is_best=mean(ref_is_best)), by=.(n,prop,k,dipp)]
ggplot(summary, aes(x=dipp, y=ref_is_best, colour=as.factor(k))) + 
  geom_line() + facet_grid(n~prop)
