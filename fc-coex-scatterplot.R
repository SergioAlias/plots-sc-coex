#!/usr/bin/env Rscript

# Sergio Alías, 20240226
# Last modified 20240305

# fc-coex-scatterplot.R

# Plots p-value for fc and coex in a scatterplot


# Libs

library(data.table)
library(ggpubr)
library(dplyr)


# Main script

## load file

data <- fread("/home/sergio/projects/plots-sc-coex/outs/solo_comunes_pvals_FDR.tsv")

dt <- data

true_min <- min(dt$coex_FDR[!dt$coex_FDR == 0])

dt <- dt %>%
  mutate(coex_FDR = case_when(coex_FDR == 0 ~ true_min,
                              coex_FDR != 0 ~ coex_FDR))

dt$coex_FDR <- -log10(dt$coex_FDR)
dt$fc_FDR <- -log10(dt$fc_FDR)

## linear model R, R-adj and p-value

mod <- lm(fc_FDR ~ coex_FDR, data = dt)
modsum <- summary(mod)
rsquared <- modsum$r.squared
adj_rsquared <- modsum$adj.r.squared
f <- modsum$fstatistic
p <- pf(f[1], f[2], f[3], lower.tail = FALSE)
attributes(p) <- NULL

pdf("outs/fc-coex-scatterplot.pdf")

ggscatter(dt, x = "coex_FDR", y = "fc_FDR",
          xlab = "-log10 COEX adjusted p-value",
          ylab = "-log10 Fold change adjusted p-value",
          color = "#757575", shape = 21, size = 2) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              color = "darkgreen") +
  geom_label(aes(x = 200, y = 20), hjust = 0,
             label = paste("R² = ", signif(rsquared, 5),
                           "\nAdjusted R² =", signif(adj_rsquared, 5),
                           " \nP =", signif(p, 5)))

dev.off()
