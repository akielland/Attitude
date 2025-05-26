# scripts/02_inspect_data.R

library(labelled)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
library(stringr)

# Load data
df <- readRDS("data/processed/survey_responses.rds")

# ==== Variable names and labels ====
var_info <- tibble(
  name = names(df),
  label = sapply(var_label(df), as.character)
)

head(var_info, 100)
names(df)


# ==== Missingness per row ====
df$missing_fraction <- rowMeans(is.na(df))
summary(df$missing_fraction)

# Check for fully empty rows (should be zero)
sum(rowSums(is.na(df)) == ncol(df))  # SAFER than using sum(df[...]): this gives row count

df$missing_fraction <- rowMeans(is.na(df))
summary(df_01$missing_fraction)

# Histogram of missingness per respondent
ggplot(df, aes(missing_fraction)) +
  geom_histogram(bins = 30) +
  labs(title = "Missingness per Respondent", x = "Fraction Missing", y = "Count")



# ==== Missingness per variable ====
missing_per_col <- sapply(df, function(x) mean(is.na(x)))
missing_df <- tibble(
  variable = names(df),
  missing_fraction = missing_per_col
) %>% arrange(desc(missing_fraction))

# Print variables with 100% missing
missing_df_100 <- missing_df %>% 
  filter(missing_fraction == 1)

print(missing_df_100)

# If you want just the names:
print(missing_df_100$variable)



# ==== Create Output for Inspection ====

# Get basic info
variable <- names(df)
var_label <- sapply(var_label(df), as.character)
var_label_fixed <- sapply(var_label(df), function(x) {
  if (length(x) == 0) NA_character_ else as.character(x)
})
is_labelled <- sapply(df, is.labelled)
type <- sapply(df, function(x) class(x)[1])
is_factor <- sapply(df, is.factor)
n_levels <- sapply(df, function(x) n_distinct(x, na.rm = TRUE))
missing_fraction <- sapply(df, function(x) mean(is.na(x)))


# Detect if variable contains passive categories
# Define pattern for non-concrete/passive answers
passive_patterns <- c(
  "not applicable", "unknown", "don't know", "refuse", 
  "prefer not", "other", "ingen", "vet ikke", "n/a", "ns", "Not_Specified"
)

# Passive modality detection: robust for haven_labelled
has_passive_modalities <- sapply(df, function(x) {
  # convert to character for inspection if labelled
  if (is.labelled(x)) x <- to_factor(x)
  if (!is.factor(x) && !is.character(x)) return(FALSE)
  lvls <- unique(as.character(x))
  pattern <- paste0("\\b(", paste(passive_patterns, collapse = "|"), ")\\b")
  any(grepl(pattern, tolower(lvls)))
})


# Build inspection summary
# Combine into summary table
inspect_summary <- tibble(
  variable,
  var_label_fixed,
  is_labelled,
  type,
  is_factor,
  n_levels,
  missing_fraction,
  has_passive_modalities,
  include_mca = NA_character_,
  needs_recode = NA_character_,
  notes = NA_character_
)

# Save summary for manual annotation
write_csv(inspect_summary, "output/inspection/variable_overview_test.csv")

# Print preview
print(head(inspect_summary, 10))


