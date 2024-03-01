#!/usr/bin/env Rscript

# Sergio Alías, 20240214
# Last modified 20240215

# tissue_nHPO.R

# Plots for the number of HPO terms per tissue


# Libs

library(ggplot2)


# Main script

data <- read.table("data/resumen_pvals_FDR.tsv",
                        header = TRUE,
                        sep = "\t")

HPO_totales <- nrow(unique(data[, c("HPO_code", "tissue")]))
HPO_totales_non_redundant <- length(unique(data$HPO_code))
HPO_coex <- nrow(unique(data[complete.cases(data$coex_pval), ][, c("HPO_code", "tissue")]))
HPO_coex_non_redundant <- length(unique(data[complete.cases(data$coex_pval), ]$HPO_code))

data[complete.cases(data$coex_pval), ]

df <- as.data.frame(data)
colnames(df) <- c("Tissue", "nHPOterms")

## Eliminar tissues no analizados

tissues_to_remove <- c("lymph-node",
                        "rectum",
                        "placenta",
                        "endometrium")

df <- subset(df, !grepl(paste(tissues_to_remove, collapse="|"), Tissue))

## Cambiar guiones por espacios y primera letra en mayúsculas

df$Tissue <- sapply(strsplit(as.character(df$Tissue), "-"), function(x) {
    paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " ")
})

## Ordering

df <- df[order(df$nHPOterms), ]
df$Tissue <- factor(df$Tissue, levels = df$Tissue)

## Plotting

pdf("outs/tissue_nHPO.pdf")

ggplot(data = df, aes(x = Tissue, y = nHPOterms, fill = nHPOterms)) +
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
