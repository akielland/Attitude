# scripts/05_transform_data.R

# I have made complete and working transformation manually.
# Her the goal is to systematize and simplify it:
# make it smarter, more modular, and less manual for next time


library(dplyr)
library(readr)


df <- readRDS("data/processed/survey_cleaned_01.rds")

# Generate a frequency summary CSV (one row per variable × modality)
get_modality_frequencies <- function(df) {
  bind_rows(lapply(names(df), function(var) {
    x <- df[[var]]
    if (!is.factor(x)) return(NULL)
    
    tab <- as.data.frame(table(modality = x, useNA = "no"))
    tab$variable <- var
    tab$proportion <- round(tab$Freq / nrow(df), 3)  #  Rounded here
    tab <- tab[, c("variable", "modality", "Freq", "proportion")]
    colnames(tab) <- c("variable", "modality", "count", "proportion")
    tab
  }))
}

df <- df[, !names(df) %in% "ID"]  # remove ID for now

mod_freq <- get_modality_frequencies(df)
mod_freq

write_csv(mod_freq, "output/inspection/modality_frequencies.csv")



## Next:
# generate the raw table with all combinations:

df <- readRDS("data/processed/survey_cleaned_01.rds")
df <- df[, !names(df) %in% "ID"]

modality_grid <- bind_rows(lapply(names(df), function(var) {
  x <- df[[var]]
  if (!is.factor(x)) return(NULL)
  tibble(
    variable = var,
    old_modality = levels(x),
    new_modality = NA_character_,
    action = NA_character_,
    notes = NA_character_
  )
}))

write.csv(modality_grid, "output/transformation/modality_recode_plan.csv", row.names = FALSE)


## Next:
# Generic Transformation Script
# The script reads the plan and applies merges, drops and renamings across all variables.
recode_from_plan <- function(df, plan) {
  plan <- plan %>% filter(!is.na(action))
  
  for (var in unique(plan$variable)) {
    if (!var %in% names(df)) next
    x <- df[[var]]
    
    this_plan <- plan %>% filter(variable == var)
    
    if (!is.factor(x)) {
      x <- as.factor(as.character(x))
    }
    
    if ("merge" %in% this_plan$action) {
      merge_map <- this_plan %>%
        filter(action == "merge" & !is.na(new_modality)) %>%
        distinct(old_modality, new_modality) %>%
        deframe()
      x <- forcats::fct_recode(x, !!!merge_map)
    }
    
    if ("drop" %in% this_plan$action) {
      drop_levels <- this_plan %>% filter(action == "drop") %>% pull(old_modality)
      x <- forcats::fct_drop(x, only = drop_levels)
    }
    
    df[[var]] <- droplevels(x)
  }
  
  df
}


plan <- read.csv("output/transformation/modality_recode_plan.csv", stringsAsFactors = FALSE)
df_transformed <- recode_from_plan(df, plan)
saveRDS(df_transformed, "data/processed/survey_cleaned_transformed.rds")


