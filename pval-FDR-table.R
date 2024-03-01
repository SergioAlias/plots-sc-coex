#!/usr/bin/env Rscript

# Sergio Alías, 20240229
# Last modified 20240229

# pval-FDR-table.R

# Create table with results
# Formato: HPO_code, HPO_name, tissue, cell_type, coment_pval, coex_pval, fc_pval, coex_FDR, fc_FDR


# Libs

library(data.table)
library(dplyr)

# Main script

# urales_home <- "/run/user/1000/gvfs/sftp:host=urales/home/salias"
# urales_home <- "/run/user/1013/gvfs/sftp:host=urales,user=salias/home/salias"
urales_home <- "/run/user/1001/gvfs/sftp:host=urales,user=salias/home/salias"

tissues <- c("adipose-tissue",
             "blood",
             "brain",
             "breast",
             "bronchus",
             "colon",
             "endometrium",
             "esophagus",
             "eye",
             "heart-muscle",
             "kidney",
             "liver",
             "lung",
             "ovary",
             "pancreas",
             "prostate-gland",
             "skeletal-muscle-organ",
             "skin",
             "small-intestine",
             "spleen",
             "stomach",
             "testis")

comentions <- fread(file.path(urales_home,
                              "TFM/annotations/comention/ALL_ACT-HPO_add_s.tsv"))

colnames(comentions) <- c("ACT.id",
                          "cell_type",
                          "HPO_code",
                          "HPO.name",
                          "str.sim",
                          "mentions.ACT",
                          "mentions.HPO",
                          "comentions",
                          "ratio",
                          "coment_pval")

comentions <- comentions[, c("HPO_code", "cell_type", "coment_pval"), with = FALSE]


results <- data.table()

for (i in seq_along(tissues)){
  
  tissue <- tissues[i]
  
  results_fc <- fread(file.path(urales_home,
                             "TFM/res_finales/fc",
                             paste0("wil_results_fc_",
                                    tissue,
                                    ".tsv")))
  results_coex <- fread(file.path(urales_home,
                                "TFM/res_finales/coex",
                                paste0("wil_results_HPA_",
                                       tissue,
                                       ".tsv")))
  results_fc_corrected <- fread(file.path(urales_home,
                                "TFM/res_finales/fc",
                                paste0("wil_results_fc_",
                                       tissue,
                                       "_corrected.tsv")))
  results_coex_corrected <- fread(file.path(urales_home,
                                  "TFM/res_finales/coex",
                                  paste0("wil_results_HPA_",
                                         tissue,
                                         "_corrected.tsv")))
  results_coex$tissue <- gsub(tissue, "c", results_coex$tissue)
  results_coex$hpo <- gsub("HP", "HP:", results_coex$hpo)
  results_coex_corrected$tissue <- gsub(tissue, "c", results_coex_corrected$tissue)
  results_coex_corrected$hpo <- gsub("HP", "HP:", results_coex_corrected$hpo)
  
  results_coex <- results_coex[, c("hpo", "hpo.name", "annotation", "wilcoxon.pval"), with = FALSE]
  colnames(results_coex) <- c("HPO_code", "HPO_name", "cell_type", "coex_pval")
  setkey(results_coex, HPO_code, cell_type)
  results_coex[, row_num := .I]
  results_coex <- results_coex[, .SD[which.min(coex_pval)], by = .(HPO_code, cell_type)]
  results_coex <- results_coex[, .SD[sample(.N, 1)], by = .(HPO_code, cell_type)]
  results_coex[, row_num := NULL]
  
  
  results_coex_corrected <- results_coex_corrected[, c("hpo", "hpo.name", "annotation", "wilcoxon.pval"), with = FALSE]
  colnames(results_coex_corrected) <- c("HPO_code", "HPO_name", "cell_type", "coex_FDR")
  setkey(results_coex_corrected, HPO_code, cell_type)
  results_coex_corrected[, row_num := .I]
  results_coex_corrected <- results_coex_corrected[, .SD[which.min(coex_FDR)], by = .(HPO_code, cell_type)]
  results_coex_corrected <- results_coex_corrected[, .SD[sample(.N, 1)], by = .(HPO_code, cell_type)]
  results_coex_corrected[, row_num := NULL]
  
  results_fc <- results_fc[, c("hpo", "hpo.name", "annotation", "high_wil_pval"), with = FALSE]
  colnames(results_fc) <- c("HPO_code", "HPO_name", "cell_type", "fc_pval")
  setkey(results_fc, HPO_code, cell_type)
  results_fc[, row_num := .I]
  results_fc <- results_fc[, .SD[which.min(fc_pval)], by = .(HPO_code, cell_type)]
  results_fc <- results_fc[, .SD[sample(.N, 1)], by = .(HPO_code, cell_type)]
  results_fc[, row_num := NULL]
  
  results_fc_corrected <- results_fc_corrected[, c("hpo", "hpo.name", "annotation", "high_wil_pval"), with = FALSE]
  colnames(results_fc_corrected) <- c("HPO_code", "HPO_name", "cell_type", "fc_FDR")
  setkey(results_fc_corrected, HPO_code, cell_type)
  results_fc_corrected[, row_num := .I]
  results_fc_corrected <- results_fc_corrected[, .SD[which.min(fc_FDR)], by = .(HPO_code, cell_type)]
  results_fc_corrected <- results_fc_corrected[, .SD[sample(.N, 1)], by = .(HPO_code, cell_type)]
  results_fc_corrected[, row_num := NULL]
  
  common_cols <- c("HPO_code", "HPO_name", "cell_type")
  
  combined_data <- merge(merge(merge(results_coex, results_coex_corrected, by = common_cols, all = TRUE),
                               results_fc, by = common_cols, all = TRUE),
                         results_fc_corrected, by = common_cols, all = TRUE)

  comentions_cols <- c("HPO_code", "cell_type")
  
  combined_with_comentions <- merge(combined_data, comentions, by = comentions_cols, all.x = TRUE)
  
  combined_with_comentions$tissue <- tissue
  
  na_indices <- is.na(combined_with_comentions$coment_pval)
  combined_with_comentions$coment_pval[na_indices] <- 1
  
  replacement <- "NA"
  
  for (col in names(combined_with_comentions)) {
    combined_with_comentions[[col]][is.na(combined_with_comentions[[col]])] <- replacement
  }
  
  results <- rbind(results, combined_with_comentions)
  
}

column_order <- c("HPO_code", "HPO_name", "tissue", "cell_type", "coment_pval", "coex_pval", "fc_pval", "coex_FDR", "fc_FDR")

results <- results[, ..column_order]

fwrite(results, file = file.path(urales_home, "TFM/res_finales/resumen_pvals_FDR.tsv"), sep = "\t")
