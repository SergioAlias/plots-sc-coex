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

dt <- fread("/home/sergio/projects/plots-sc-coex/outs/solo_comunes_pvals_FDR.tsv")

pdf("outs/fc-coex-scatterplot.pdf")

ggscatter(dt, x = "coex_FDR", y = "fc_FDR",
          xlab = "COEX adjusted p-value",
          ylab = "Fold change adjusted p-value",
          color = "black", shape = 21, size = 3,
          add = "reg.line",
          add.params = list(color = "darkgreen", fill = "lightgray"),
          conf.int = TRUE,
          cor.coef = TRUE,
          cor.coeff.args = list(method = "pearson", label.x = 0.7, label.y = 0.6, label.sep = "\n", geom = "label")
)

dev.off()
