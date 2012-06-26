if(!require(plyr)){install.packages("plyr")}
if(!require(ggplot2)){install.packages("ggplot2")}

load("~/Documents/PhD/Experiments/community-DNA-fingerprinting/R/Old/Sensitivity analysis/sensitivity_results_full.RData")

f.p.results$n.sp<-paste(f.p.results$n.sp, "species")
f.p.results$n.loci<-paste(f.p.results$n.loci, "loci")

pdf("./Plots/sensitivity analysis.pdf")
p<-qplot(data=f.p.results,
      x=true.f.p,
      y=optim.f.p,
      colour=n.sp)+
      facet_grid(n.loci~noise, scales = "free") +
      geom_smooth(method="lm", se=F)
print(p)
dev.off()      