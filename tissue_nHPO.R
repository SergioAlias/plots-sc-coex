#!/usr/bin/env Rscript

# Sergio Alías, 20240214
# Last modified 20240215

# tissue_nHPO.R

# Plots for the number of HPO terms per tissue


# Libs

library(ggplot2)
library(dplyr)


# Main script

data <- read.table("data/solo_comunes_pvals_FDR.tsv",
                        header = TRUE,
                        sep = "\t")

HPO_totales <- nrow(unique(data[, c("HPO_code", "tissue")]))
HPO_totales_non_redundant <- length(unique(data$HPO_code))
HPO_coex <- nrow(unique(data[complete.cases(data$coex_pval), ][, c("HPO_code", "tissue")]))
HPO_coex_non_redundant <- length(unique(data[complete.cases(data$coex_pval), ]$HPO_code))

df <- as.data.frame(data)


## Eliminar tissues no analizados

tissues_to_remove <- c("lymph-node",
                        "rectum",
                        "placenta",
                        "bone-marrow")


df <- subset(df, !grepl(paste(tissues_to_remove, collapse="|"), tissue))

## Cambiar guiones por espacios y primera letra en mayúsculas

df$tissue <- sapply(strsplit(as.character(df$tissue), "-"), function(x) {
    paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " ")
})

## Contar HPOs

df<- df %>%
  group_by(tissue) %>%
  summarize(nHPOterms = n_distinct(HPO_code))


## Ordering

df <- df[order(df$nHPOterms), ]
df$tissue <- factor(df$tissue, levels = df$tissue)

## Plotting

pdf("outs/tissue_nHPO.pdf")

ggplot(data = df, aes(x = tissue, y = nHPOterms, fill = nHPOterms)) +
    geom_bar(stat = "identity", position = position_dodge())+
    geom_text(aes(label = nHPOterms), hjust = 0, color = "black",
            size=3) +
    scale_fill_gradient(low = "green", high = "darkgreen") +
    labs(x = element_blank(), y = "Number of HPO terms") +
    coord_flip() +
    theme_minimal() +
    theme(legend.title = element_blank())

dev.off()

## Save df

write.table(df, file = "outs/tissue_nHPO.tsv", sep = "\t", row.names = FALSE)
