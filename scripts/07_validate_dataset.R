# scripts/07_validate_dataset.R

library(dplyr)
library(forcats)
library(readr)

# === Load cleaned dataset ===
df <- readRDS("data/processed/survey_cleaned_01.rds")


# === Setup ===
n <- nrow(df)
id_var <- "ID"  # Optional: exclude ID column
df_check <- df[, !names(df) %in% id_var]


# === Check 1: All factors? ===
non_factors <- names(df_check)[!sapply(df_check, is.factor)]
non_factors


# === Check 2: Any NA values? ===
na_summary <- sapply(df_check, function(x) sum(is.na(x)))
sum(na_summary)
na_summary[na_summary > 0]

table(df$q16_BIL_ANTALL, useNA = "always")


# === Check 3: Single-level variables ===
single_level_vars <- names(df_check)[sapply(df_check, function(x) n_distinct(x) <= 1)]


# === Check 4: Rare modalities (< 2% threshold) ===
modality_frequencies <- bind_rows(lapply(names(df_check), function(var) {
  x <- df_check[[var]]
  tab <- as.data.frame(table(x, useNA = "no"))
  colnames(tab) <- c("modality", "count")
  tab$variable <- var
  tab$proportion <- tab$count / n
  tab
}))

rare_modalities <- modality_frequencies %>% filter(proportion < 0.02)

# === Output summary ===
dir.create("output/inspection", showWarnings = FALSE, recursive = TRUE)

write_csv(modality_frequencies, "output/inspection/modality_frequencies.csv")
write_csv(rare_modalities, "output/inspection/rare_modalities.csv")

cat("✅ Validation report\n")
cat("----------------------\n")
cat("Total variables checked:", ncol(df_check), "\n")
cat("Variables that are not factors:", length(non_factors), "\n")
if (length(non_factors) > 0) print(non_factors)

cat("Variables with NA values:\n")
print(na_summary[na_summary > 0])

cat("Variables with only 1 level:", length(single_level_vars), "\n")
if (length(single_level_vars) > 0) print(single_level_vars)

cat("Rare modalities (<2%) found:", nrow(rare_modalities), "\n")
cat("Details saved to output/inspection/rare_modalities.csv\n")
