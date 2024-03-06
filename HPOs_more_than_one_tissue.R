#!/usr/bin/env Rscript

# Sergio Alías, 20240306
# Last modified 20240306

# HPOs_more_than_one_tissue.R

# Table with HPOs present in more than one tissue

# Libs

library(data.table)
library(dplyr)

# Main

data <- fread("/home/sergio/projects/plots-sc-coex/outs/solo_comunes_pvals_FDR.tsv") # nolint

dt <- data

dt <- dt %>%
  group_by(HPO_code, tissue) %>%
  sample_n(1)

common_hpos <- as.data.frame(table(dt$HPO_code, dt$HPO_name, dt$tissue))
common_hpos <- common_hpos[common_hpos$Freq > 0, ]

common_hpos <- common_hpos %>%
  group_by(Var1) %>%
  mutate(ntis = sum(Freq))

common_hpos <- common_hpos[common_hpos$ntis > 1, ]

common_table <- common_hpos %>%
  group_by(Var1) %>%
  mutate(Var3 = gsub("-", " ", Var3),
         Var3 = sub("(^.)(.*)", "\\U\\1\\L\\2", Var3, perl = TRUE)) %>%
  summarise(tissues = paste(Var3, collapse = ", "))

common_hpos <- common_hpos %>%
  group_by(Var1) %>%
  sample_n(1)

common_table <- common_table %>%
  mutate(HPO_name = common_hpos$Var2)

common_hpos <- as.character(common_hpos$Var1)

common_table <- common_table %>%
  mutate(HPO_code = Var1,
         HPO_name = HPO_name,
         tissues = tissues,
         Var1 = NULL)

common_table <- common_table %>%
  relocate(HPO_code, .before = HPO_name) %>%
  relocate(tissues, .after = HPO_name)

write.table(common_table, file = "outs/HPOs_comunes.tsv", sep = "\t", row.names = FALSE)
