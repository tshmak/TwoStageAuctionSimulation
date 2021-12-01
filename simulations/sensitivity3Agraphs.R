Tmisc()
setwd0("~/Dropbox/Apps/FanoTimMak/simulations")
filename <- Rfilename("sensitivity3Agraphs")
load("sensitivity3Aresults.RData")

library(data.table)
results <- as.data.table(results.table)
results[, ID := ceiling((1:nrow(results))/2)]
results[, max_util1 := max(util1), by=.(n,prop,k,slope,B1,Beta1,Maxx1)]
results[, regret := max_util1 - util1]
results[, regretprop:= ifelse(max_util1 == 0, 0, regret / max_util1)]
results[, b1inf := b1hat / B1]
results[, maxx1inf := maxx1hat / Maxx1]
results[, b1reldiff := (b1hat - B1)/B1]
results[, maxx1reldiff := (maxx1hat - Maxx1)/Maxx1]
results[, ref_is_best := (util1[1] %==% util1[2]), 
        by=.(n,prop,k,slope,B1,Beta1,Maxx1)]

results[, best_params := paste0("x1p=", signif(xhat1prop, 2), 
                                ",b1h=", signif(b1hat, 2),
                                ",m1h=", signif(maxx1hat, 2))[!ref], 
        by=.(n,prop,k,slope,B1,Beta1,Maxx1)]
results[, best_params_inf := paste0("x1p=", signif(xhat1prop, 2), 
                                ",b1h=", signif(b1inf, 2),
                                ",m1h=", signif(maxx1inf, 2))[!ref], 
        by=.(n,prop,k,slope,B1,Beta1,Maxx1)]


control <- subset(results, slope<=-1)
best <- subset(results, !ref)
control_best <- subset(best, slope<=-1)

ref <- subset(results, ref)
control_ref <- subset(ref, slope<=-1)

# print(control)
# print(control_ref)
# print(control_best)
summary(control_ref$regret)

ref_not_best <- subset(best, !ref_is_best)

# probs <- subset(ref_not_best, b1inf < 0.99)$ID
# subset(results, ID %in% probs)
# stop()

library(ggplot2)
ggplot(ref_not_best, aes(x=as.factor(slope), y=b1inf, colour=as.factor(k))) + 
  geom_point(size=0.2) + facet_grid(n~prop) + 
  scale_colour_brewer(palette="RdGy") + theme_classic()
ggplot(ref_not_best, aes(x=as.factor(slope), y=maxx1inf, colour=as.factor(k))) + 
  geom_point(size=0.2) + facet_grid(n~prop) + theme_classic()
ggplot(ref_not_best, aes(x=as.factor(slope), y=xhat1prop, colour=as.factor(k))) + 
  geom_point(size=0.2) + facet_grid(n~prop) + theme_classic()

summary <- results[, .(ref_is_best=mean(ref_is_best)), by=.(n,prop,k,slope)]
ggplot(summary, aes(x=slope, y=ref_is_best, colour=as.factor(k))) + 
  geom_line() + facet_grid(n~prop)

#### Results for table ####
results[slope > -1, .(b1inf=mean(b1inf), maxx1inf=mean(maxx1inf), xhat1prop=mean(xhat1prop)), 
            by=.(k, slope)]

table <- results[slope > -1, .(mean=mean(maxx1inf), prop=mean(maxx1hat!=Maxx1)), 
        by=.(k, slope)]
print(table)

for(i in 1:nrow(table)) {
  if(i %% 2 == 1) {
    cat(paste0("$k=", table$k[i], "$ & "))
  }
  cat(paste0(
    "$\\begin{array}{c} ", 
    "(1,", round(table$mean[i], 2), ",1) \\\\", 
    " {}[0,", round(table$prop[i], 2), ",0]", 
    " \\end{array}$"))
  if(i %% 2 == 1) {
    cat(" & ")
  } else {
    cat(" \\\\")
    cat("\n")
  }
}

#### Regret table #### 
tableb <- results[slope > -1 & ref == TRUE, .(mean=mean(regretprop), 
                                              prop=mean(regretprop<0.01)), 
                  by=.(k, slope)]
print(tableb)

for(i in 1:nrow(tableb)) {
  if(i %% 2 == 1) {
    cat(paste0("$k=", tableb$k[i], "$ & "))
  }
  cat(paste0(
    "(", signif(tableb$mean[i]*100, 2), "%,", 
         signif(tableb$prop[i]*100, 2), "%)"))
  if(i %% 2 == 1) {
    cat(" & ")
  } else {
    cat(" \\\\")
    cat("\n")
  }
}

#### reldiff table ####
table <- results[!ref & slope > -1, 
                 .(b1p=mean(b1hat %==% B1), 
                   maxx1p=mean(maxx1hat %==% Maxx1)), 
                 by=.(k, slope)]
print(table)
table_b1 <- results[!ref & !(b1hat %==% B1) & slope > -1, 
                    .(b1=mean(abs(b1reldiff))), by=.(k, slope)]
table_maxx1 <- results[!ref & !(maxx1hat %==% Maxx1) & slope > -1, 
                    .(maxx1=mean(maxx1reldiff)), by=.(k, slope)]

table <- merge(table, table_b1, all.x=TRUE)
table <- merge(table, table_maxx1, all.x=TRUE)

for(i in 1:nrow(table)) {
  if(i %% 2 == 1) {
    cat(paste0("$k=", table$k[i], "$ & "))
    cat("$\\theta_1 = \\begin{array}{c} v_1/\\beta_1 \\\\ x^{\\max}_1 \\\\ x_1 \\end{array}$ & ")
  }
  cat(paste0(
    "$\\begin{array}{c} ", 
    signif(table$b1p[i], 2)*100,       "\\%\\ (",
    signif(table$b1[i], 2),            ") \\\\", 
    signif(table$maxx1p[i], 2)*100,    "\\%\\ (",
    signif(table$maxx1[i], 2),         ") \\\\", 
    "100\\% (",
    "NA) \\\\", 
    " \\end{array}$"))
  if(i %% 2 == 1) {
    cat(" & ")
  } else {
    cat(" \\vspace{1mm} \\\\")
    cat("\n")
  }
}

