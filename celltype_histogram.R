#!/usr/bin/env Rscript

# Sergio Alías, 20240306
# Last modified 20240311

# celltype_histogram.R

# Histogram and boxplot % celltypes significant coment, fc, COEX


# Libs

library(dplyr)
library(reshape2)
library(ggpubr)


# Main script

data <- read.table("outs/solo_comunes_pvals_FDR.tsv",
                   header = TRUE,
                   sep = "\t")

df <- as.data.frame(data)

## Cambiar guiones por espacios y primera letra en mayúsculas

df$tissue <- sapply(strsplit(as.character(df$tissue), "-"), function(x) {
  paste(toupper(substring(x, 1, 1)),
        substring(x, 2),
        sep = "",
        collapse = " ")
})

df <- df %>%
  group_by(HPO_code, HPO_name, tissue) %>%
  summarise(CoMent = (sum(coment_pval <= 0.001) / n()) * 100,
            FC = (sum(fc_FDR <= 0.001) / n()) * 100,
            COEX = (sum(coex_FDR <= 0.001) / n()) * 100)
            # nCTcoment = sum(coment_pval <= 0.001))

write.table(df,
            file = "outs/celltype_histogram.tsv",
            sep = "\t",
            row.names = FALSE)

df <- df %>%
  melt(id.vars = c("HPO_code", "HPO_name", "tissue"),
       variable.name = "metric",
       value.name = "percent")

pdf("outs/celltype_histogram.pdf")

gghistogram(df,
            x = "percent",
            add = "mean",
            rug = TRUE,
            color = "metric",
            fill = "metric",
            palette = c("#00AFBB",
                        "#E7B800",
                        "#03d17b"))

dev.off()

png("outs/celltype_boxplot.png")

ggboxplot(df,
          "metric",
          "percent",
          xlab = FALSE,
          ylab = "% of significant cell types",
          color = "metric",
          palette = c("#00AFBB",
                      "#E7B800",
                      "#03d17b"),
          add = "jitter",
          shape = "metric") + theme(legend.position = "none")

dev.off()
