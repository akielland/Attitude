# scripts/04_clean_data.R

library(dplyr)
library(labelled)
library(forcats)
library(readr)

# === Load raw data and review checklist ===
df_raw <- readRDS("data/processed/survey_responses.rds")
checklist <- read_csv("output/inspection/variable_overview_test.csv", show_col_types = FALSE)
problems(checklist)
print(checklist, n=20)

table(df_raw$drivstoff_bil1, useNA= "always")
table(df$drivstoff_bil1, useNA= "always")

# Checking: before script
table(df_raw$A2a_arsaker_4, useNA= "always")
lapply(df_raw %>% select(starts_with("A2a_arsaker_")), table, useNA= "always")

# check type
type_summary <- sapply(df_raw, class)
table(sapply(type_summary, `[`, 1))


# Step 1: Keep only selected variables
vars_to_keep <- checklist %>% 
  filter(include_mca == "yes") %>%
  pull(variable)

head(vars_to_keep, n=10)
length(vars_to_keep)
vars_to_keep

# Select vars:
df <- df_raw[, vars_to_keep]

# type check
type_summary <- sapply(df, class)
table(sapply(type_summary, `[`, 1))


# Step 2: Convert labelled and numeric (ALL) to factor
df <- df %>%
  mutate(across(where(is.labelled), to_factor)) %>%
  mutate(across(where(is.numeric), ~ factor(as.character(.x))))


# check type
type_summary <- sapply(df, class)
table(sapply(type_summary, `[`, 1))
# non-factor vars:
names(df)[!sapply(df, is.factor)]


# Step 3: Recode passive-like responses and NA to "NS"
passive_patterns <- c("ns", "other", "not applicable", "unknown", "don't know", 
                      "refuse", "prefer not", "ingen", "vet ikke", "n/a")

recode_na_and_passives_to_ns <- function(x) {
  if (!is.factor(x)) return(x)
  
  # Normalize level casing
  levels(x) <- tolower(levels(x))
  
  # Match to passive patterns
  passive_levels <- levels(x)[levels(x) %in% tolower(passive_patterns)]
  
  # Collapse passive-like levels
  if (length(passive_levels) > 0) {
    x <- fct_collapse(x, NS = passive_levels)
  }
  
  # Replace true NA with "NS"
  if (anyNA(x)) {
    x <- fct_na_value_to_level(x, level = "NS")
  }
  
  return(x)
}

df <- df %>% mutate(across(where(is.factor), recode_na_and_passives_to_ns))
# check:
table(df$A2a_arsaker_4, useNA= "always")



# Step 4: Drop unused levels
# First look at unused levels to check if everything is OK
check_unused_levels <- function(x) {
  if (!is.factor(x)) return(0)
  length(setdiff(levels(x), unique(x)))
}

unused_levels_summary <- sapply(df, check_unused_levels)
unused_levels_summary <- unused_levels_summary[unused_levels_summary > 0]

cat("Variables with unused levels:\n")
unused_levels_summary
table(df$familietype)

# Drop unused levels:
df <- df %>% mutate(across(where(is.factor), droplevels))


# Step 5: Add sequential ID column
df <- df %>%
  mutate(ID = seq_len(n())) %>%
  relocate(ID, .before = 1)


# Checking: after script
table(df$A2a_arsaker_4, useNA= "always")
lapply(df %>% select(starts_with("A2a_arsaker_")), table, useNA= "always")


# Save cleaned dataset
# dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(df, "data/processed/survey_cleaned_01.rds")

cat("Use `options(passive = \"NS\")` before calling `soc.mca()`.\n")
