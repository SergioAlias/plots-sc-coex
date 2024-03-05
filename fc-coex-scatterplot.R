#!/usr/bin/env Rscript

# Sergio Alías, 20240226
# Last modified 20240305

# fc-coex-scatterplot.R

# Plots p-value for fc and coex in a scatterplot


# Libs

library(data.table)
library(ggpubr)


# Main script

## load file

data <- fread("/home/sergio/projects/plots-sc-coex/outs/solo_comunes_pvals_FDR.tsv")

dt <- data

dt <- dt %>%
  mutate(coex_FDR = coex_FDR + 1,
        fc_FDR = fc_FDR + 1)

dt$coex_FDR <- -log10(dt$coex_FDR)
dt$fc_FDR <- -log10(dt$fc_FDR)

mod = lm(fc_FDR~coex_FDR, data = dt)
modsum = summary(mod)

pdf("outs/fc-coex-scatterplot.pdf")

ggscatter(dt, x = "coex_FDR", y = "fc_FDR",
          xlab = "-log10 COEX adjusted p-value",
          ylab = "-log10 Fold change adjusted p-value",
          color = "black", shape = 21, size = 3) +
          stat_smooth(method = "lm", 
            formula = y ~ x, 
            geom = "smooth") 

dev.off()
