Tmisc()
setwd0("~/Dropbox/Apps/FanoTimMak/simulations")
filename <- Rfilename("sim2graphsA")
load("sim2results.RData")
Tim.load(flexalloc, "..")

#### Other variables ####
results.table$pay2 <- results.table$pay * results.table$beta
results.table$direct <- results.table$x * results.table$b * results.table$beta
results.table$util <- results.table$pay2 - results.table$direct
results.table$ratio <- results.table$vwel / results.table$pay2
# results.table$k <- as.factor(results.table$k)
results.table$nprop <- with(results.table, paste0("rho=", prop,"\nn=",n))

#### Summaries ####
library(data.table)
results <- as.data.table(results.table)
summary <- subset(results, q==0)[,list(totalpay=sum(pay2), totalvwel=sum(vwel), nprop=nprop[1]), 
                                 by=.(k,n,prop,seed)]
summary2 <- summary[,list(totalpay=mean(totalpay), totalvwel=mean(totalvwel), nprop=nprop[1]), 
                    by=.(k,n,prop)]
summary2[, baseline := sum(totalpay * (k==Inf)), by=.(n,prop)]
summary2[, inflation := totalpay / baseline]
roisumm <- subset(results, roi)[,list(pay2=mean(pay2), direct=mean(direct), 
                                      q=mean(q), nprop=nprop[1], beta=mean(beta)), 
                                by=.(k,n,prop,b)]
S <- lapply(0:5, function(indirect) cbind(roisumm, indirect=indirect))
roisumm2 <- do.call(rbind, S)
roisumm2$ROI <- with(roisumm2, pay2/(direct+indirect))
roisumm2$ROI[!is.finite(roisumm2$ROI) & roisumm2$indirect > 0] <- 0
roisumm2$ROI[!is.finite(roisumm2$ROI) & roisumm2$indirect == 0] <- 1
roisumm2$k2 <- paste0("k=", roisumm2$k)
roisumm2$v <- roisumm2$b * roisumm2$beta

#### Plots ####
library(ggplot2)
rotate <- theme(axis.text.x = element_text(angle = 90))
bottom_legend <- theme(legend.position="bottom")


### ROI vs bid by k
ggplot(roisumm2, aes(x=b, y=ROI, color=as.factor(indirect))) + 
  geom_line() + geom_hline(yintercept = 1) + facet_grid(k2~nprop, scales="free_y") + 
  rotate + bottom_legend + labs(colour = "Indirect costs")
# ggsave(paste0(filename, ))

### total cost vs k 
summary2$ref <- summary2$k == Inf
ggplot(summary2, aes(x=as.factor(k), y=inflation, fill=ref)) + ggbar() + 
  facet_grid(paste0("n=",n) ~ paste0("rho=",prop)) + 
  labs(y="Inflation of total cost", x="k") +
  theme_light() + theme(legend.position = "none")
ggsave(paste0(filename, "_Inflation.pdf"), height = 5, width = 4) 

roisumm2$smoothROI <- NA
roisumm2$smoothROI1 <- NA
for(nn in N <- unique(roisumm2$n)) {
  for(pp in Prop <- unique(roisumm2$prop)) {
    for(kk in K <- c(unique(roisumm2$k), Inf)) {
      for(ii in unique(roisumm2$indirect)) {
        # nn <- N[1]; pp <- Prop[1]; kk <- K[2]; ii <- 5
        selected <- with(roisumm2, indirect==ii & k==kk & prop==pp & n==nn )
        ss <- subset(roisumm2, selected)
        sf <- monospline(ss$q, ss$ROI, k=length(ss$q))
        sf0 <- function(x) return(sf(x) - 1)
        if(ii == 0) sf0 <- function(x) return(max(0, sf(x) - 1))
        roisumm2$smoothROI[selected] <- sf(ss$q)
        if(sf0(1) >= 0) {
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
ggplot(roisumm2, aes(x=v, y=smoothROI, color=as.factor(indirect))) +  
  facet_grid(k2~nprop, scales = "free_y") + 
  geom_line() + geom_hline(yintercept = 1) + 
  labs(y="ROI (Smoothed)", colour = "Indirect costs") + 
  theme_light() + bottom_legend + rotate
ggsave(paste0(filename, "_ROI.pdf"), height=6, width=5)

roisumm3 <- roisumm2[,list(ROI1=mean(smoothROI1)), by=.(k,n,prop,indirect)]
roisumm3 <- merge(roisumm3, summary2)
ggplot(roisumm3, aes(x=as.factor(indirect), y=ROI1)) + ggbar() + 
  facet_grid(paste0("k=",k)~nprop) + 
  labs(x="Indirect cost", y="Proportion ROI > 1") +
  theme_light()
ggsave(paste0(filename, "_ROIprop.pdf"), height=5, width=4)

ggplot(roisumm3, aes(x=inflation, y=ROI1)) + 
  geom_line(aes(colour=as.factor(indirect))) + 
  geom_point(aes(shape=as.factor(k)), colour="gray50") + 
  facet_grid(paste0("n=",n)~paste0("rho=",prop), scales="free_x") + 
  labs(colour="Indirect cost", y="Proportion ROI > 1", shape="k") + 
  theme_light()+ bottom_legend
ggsave(paste0(filename, "_InflationProp.pdf"), height=5, width=5)
