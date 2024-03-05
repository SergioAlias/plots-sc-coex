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

dt$coex_FDR <- -log10(dt$coex_FDR)
dt$coex_FDR <- -log10(dt$fc_FDR)

pdf("outs/fc-coex-scatterplot.pdf")

ggscatter(dt, x = "coex_FDR", y = "fc_FDR",
          xlab = "-log10 COEX adjusted p-value",
          ylab = "-log10 Fold change adjusted p-value",
          color = "black", shape = 21, size = 3,
          add = "reg.line",
          add.params = list(color = "darkgreen", fill = "lightgray"),
          conf.int = TRUE,
          cor.coef = TRUE,
          cor.coeff.args = list(method = "pearson", label.sep = "\n", geom = "label")
)

dev.off()
