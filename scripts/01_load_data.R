# scripts/01_load_data.R

# Data from multiple-choice questioner in attitudes toward cars 


library(haven)  # For reading SPSS .sav files


# Import SPSS data
file_path <- "data/raw/Hovedutvalg.sav"
df_sav <- read_sav(file_path)

# Quick check
print(dim(df_sav))
print(names(df_sav)[1:20])  # Peek at first 20 column names

# Inspect variables
labelled::var_label(df_sav)

# Save as compressed R object
saveRDS(df_sav, "data/processed/survey_responses.rds")



############## some paly code
lapply(df_sav %>% select(starts_with("A2a_arsaker_")), table)
