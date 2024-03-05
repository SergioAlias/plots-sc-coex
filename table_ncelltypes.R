#!/usr/bin/env Rscript

# Sergio Alías, 20240305
# Last modified 20240305

# table_ncelltypes.R

# Plots for the number of HPO terms per tissue


# Libs

library(ggplot2)
library(dplyr)


# Main script

data <- read.table("outs/solo_comunes_pvals_FDR.tsv",
                        header = TRUE,
                        sep = "\t")

df <- as.data.frame(data)

## Cambiar guiones por espacios y primera letra en mayúsculas

df$tissue <- sapply(strsplit(as.character(df$tissue), "-"), function(x) {
    paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " ")
})

## Contar celltypes y significativos coex y fdr

df_sum <- df %>%
         group_by(tissue, HPO_code) %>%
         mutate(n_ct = length(cell_type),
                sign_ct_fc = sum(fc_FDR <= 0.001),
                sign_ct_coex = sum(coex_FDR <= 0.001),
                cell_type = NULL,
                coment_pval = NULL,
                coex_pval = NULL,
                fc_pval = NULL,
                coex_FDR = NULL,
                fc_FDR = NULL) %>% sample_n(1)


## Save df

write.table(df_sum, file = "outs/tissues_ncelltypes.tsv", sep = "\t", row.names = FALSE)
