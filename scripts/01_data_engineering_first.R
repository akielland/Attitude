## Initial clean of multiple-choice questioner in attitudes toward cars 

library(haven)
library(labelled)  # Required for var_label()
library(dplyr)
library(tidyr)


df_01 <- readRDS("data/processed/survey_responses.rds")



## INSPECT RAW DATA 

# Count var: need to match ncol(df) else, there might be a list issue.
length(sapply(df_0, class))
# reveal the class in each var
head(sapply(df_0, class))
# see range of classes (for var) used in the data
unique(sapply(df_0, function(x) length(class(x))))

# Get the first class per variable to able table() to work.
class_summary <- sapply(df_0, function(x) class(x)[1])
table(class_summary)

# Identify Var with multiple classes: which are the have_label correctly red from the spss file
multi_class_vars <- names(df_0)[sapply(df_0, function(x) length(class(x)) > 1)]
length(multi_class_vars)
table(sapply(df_0[multi_class_vars], class))

# A multi-class variable in R is a variable (a column in a data frame) that has more than one class assigned to it. 
# In R, every object has a class that determines how it behaves, and some objects can have multiple classes at the same time.
# Normally, a column in a data frame has a single class, like "numeric", "character", or "factor". 
# However, some special types of data, such as those imported from SPSS (.sav files) using the haven package, may have multiple classes.
# For example, when reading an SPSS .sav file, variables with value labels (like survey responses) often have these classes:
# "double" (because the data is stored as numbers)
# "haven_labelled" (because the data contains SPSS-style value labels)
# "vctrs_vctr" (internal class used by tidyverse packages)

# Inspecting var that is only numeric class (not multi-class)
class_summary <- sapply(df_0, function(x) class(x)[1]) 
only_numeric_vars <- names(class_summary[class_summary == "numeric"])
length(only_numeric_vars)  # Should return 22
only_numeric_vars
# Count unique values for each numeric var
sapply(df_0[only_numeric_vars], function(x) length(unique(x)))
# NB: a lot of data - inspect meta data for each label
sapply(df_0[only_numeric_vars], function(x) attr(x, "label"))


# Results:
# 178 multi-class variables (all "haven_labelled" type, meaning they contain SPSS-style labels)
# 6 character variables (likely free-text or ID-like variables)
# 22 numeric variables (some might be continuous; others might be misclassified categorical variables)


# Is all meta-data retrieved
# pass


## REMOVE: var obviously not need 
## MAKE:  age group;  dist to public transp; row index

# Row Identifiers
id_vars <- c("Unique_ID_Dataset", "Aid.1", "Serial")

# Numeric var with high numbers of values (59 and above): Probably not categorical
age_vars <- c("A1b_alder.1", "c_alder", "fødselsår", "alder")
# "A1b_alder.1": age of receiving driver licence
# there are categorical made for age in the data set - but was wrongly made
# check equality
all(df_0$c_alder == df_0$alder, na.rm = TRUE)  # Should return TRUE if they are identical

# Make new age_grope var
df_0$age_group <- cut(df_0$alder,
                       breaks = c(16, 24, 34, 44, 54, 64, 74, Inf),
                       labels = c("Age: 16-24", 
                                  "Age: 25-34", 
                                  "Age: 35-44", 
                                  "Age: 45-54", 
                                  "Age: 55-64", 
                                  "Age: 65-74", 
                                  "Age: 75+"),
                       ordered_result = TRUE)

# Count number of respondents in each age group
table(df_0$age_group)

# remove age variables
df_0 <- df_0 %>% select(-all_of(age_vars))
# Verify that `alder` is removed
"alder" %in% names(df_0)  # Should return FALSE


# year: not used in the analysis
year_vars <- c("Year")

# Var That Are Entirely NA
na_only_vars <- names(df_0)[sapply(df_0, function(x) all(is.na(x)))]
na_only_vars
# one answer (usually only NA): no information
one_answear <- names(df_0)[sapply(df_0, function(x) length(unique(x)) == 1)]
one_answear

# Variables with Too Many Levels
too_many_levels_vars <- names(df_0)[sapply(df_0, function(x) length(unique(x)) > 25)]
too_many_levels_vars
too_many_levels_vars <- setdiff(too_many_levels_vars, "q141_HJEM_STOPPESTED_d1")


# Only Text responses: likely not categorical
text_var <- names(df_0)[sapply(df_0, function(x) any(grepl(",", na.omit(x))))]
length(text_var)
unique(df_0[text_var])

# Identify Character var: seems not to be useful var in MCA
character_vars <- names(df_0)[sapply(df_0, is.character)]
character_vars

# print Non-Empty Values from Each Character var
for (var in character_vars) {
  cat("\n---", var, "---\n")
  print(df_0[[var]][df_0[[var]] != "" & !is.na(df_0[[var]])][1:10])  # Print first 10 non-empty values
}

# look at character var
unique(df_0$A2a_arsaker.11)
unique(df_0$A2b_arsaker2.14)
unique(df_0$A12c_aktiviteter_annet.1)
unique(df_0$q15_BILEIE_o6)
unique(df_0$Q12cQaktiviteterQannet)

# But these might be converted to categorical (or ordinal) later => KEEP
potential_categorical_or_ordered_var <- c("q16_BIL_ANTALL", "q141_HJEM_STOPPESTED_d1")


# List of var to Remove
vars_to_remove <- unique(c(
  id_vars,
  age_vars,
  year_vars,
  na_only_vars,
  one_answear,
  too_many_levels_vars,
  text_var,
  character_vars
))
length(vars_to_remove) # 28

# new df
df_01 <- df_0 %>% select(-all_of(vars_to_remove))
df_temp <- df_01
df_01 <- df_temp
saveRDS(df_01, "df_cleaned_01.rds")  # Save to file
df_01 <- readRDS("df_cleaned_01.rds")  # Restore dataset

# new df: var in each class
class_summary <- sapply(df_01, function(x) class(x)[1])  # Extract only the first class
table(class_summary)


## NB: Inspect_ REove or groups, but maybe later
df_01$A1a_fkort # can merge: "3 Nei, men er i ferd med å ta det" too "ja"
df_01f$yrkesstatus # Remove Annet ? Look at frequency
df_0$q141_HJEM_STOPPESTED_d1 # Make groups?

# Dist Public Transp Gr.: Bins for distance to public transport
# Count first:
df_01 %>%
  mutate(bin = case_when(
    q141_HJEM_STOPPESTED_d1 < 0.2 ~ "0-0.2 km",
    q141_HJEM_STOPPESTED_d1 >= 0.2 & q141_HJEM_STOPPESTED_d1 < 0.5 ~ "0.2-0.5 km",
    q141_HJEM_STOPPESTED_d1 >= 0.5 & q141_HJEM_STOPPESTED_d1 < 1 ~ "0.5-1 km",
    q141_HJEM_STOPPESTED_d1 >= 1 & q141_HJEM_STOPPESTED_d1 < 2 ~ "1-2 km",
    q141_HJEM_STOPPESTED_d1 >= 2 & q141_HJEM_STOPPESTED_d1 < 5 ~ "2-5 km",
    q141_HJEM_STOPPESTED_d1 >= 5 & q141_HJEM_STOPPESTED_d1 < 20 ~ "5-20 km",
    q141_HJEM_STOPPESTED_d1 >= 20 ~ ">20 km",
    TRUE ~ "Missing"
  )) %>%
  count(bin, sort = TRUE)
# make groups
df_01 <- df_01 %>%
  mutate(q141_HJEM_STOPPESTED_d1 = case_when(
    q141_HJEM_STOPPESTED_d1 < 0.2 ~ "Very Close (0-0.2 km)",
    q141_HJEM_STOPPESTED_d1 >= 0.2 & q141_HJEM_STOPPESTED_d1 < 0.5 ~ "Close (0.2-0.5 km)",
    q141_HJEM_STOPPESTED_d1 >= 0.5 & q141_HJEM_STOPPESTED_d1 < 1 ~ "Moderate (0.5-1 km)",
    q141_HJEM_STOPPESTED_d1 >= 1 & q141_HJEM_STOPPESTED_d1 < 2 ~ "Far (1-2 km)",
    q141_HJEM_STOPPESTED_d1 >= 2 & q141_HJEM_STOPPESTED_d1 < 5 ~ "Very Far (2-5 km)",
    q141_HJEM_STOPPESTED_d1 >= 5 & q141_HJEM_STOPPESTED_d1 < 20 ~ "Extremely Far (5-20 km)",
    q141_HJEM_STOPPESTED_d1 >= 20 ~ "Outliers (>20 km)",
    TRUE ~ NA_character_  # Keep NA values as NA
  ))


df_01$q141_HJEM_STOPPESTED_d1

# new df: var in each class
class_summary <- sapply(df_01, function(x) class(x)[1])  # Extract only the first class
table(class_summary)

# Convert "q141_HJEM_STOPPESTED_d1" to factor for MCA
df_01$q141_HJEM_STOPPESTED_d1 <- factor(df_01$q141_HJEM_STOPPESTED_d1)
class_summary <- sapply(df_01, function(x) class(x)[1])  # Extract only the first class
table(class_summary)

# Save to file
saveRDS(df_01, "df_cleaned_01.rds")


####################################
## Convert Variables to Factors
####################################

# 3 data types: 
# - Havel labeled
# - Character I just made => q141_HJEM_STOPPESTED_d1 
# - Numeric
# - Numeric that should have been Haven labeled => arsake2_


# Convert haven_labelled
df_02 <- df_01 %>%
  mutate(across(where(~ "haven_labelled" %in% class(.)), as_factor)) 
# df_02 <- df_01 %>% mutate(across(where(function(x) "haven_labelled" %in% class(x)), as_factor))
class_summary <- sapply(df_02, function(x) class(x)[1])  # Extract only the first class
table(class_summary)
cs <- names(sapply(df_02, function(x) class(x))) 
names(cs[cs == "character"])

# Convert Character Var
df_02 <- df_02 %>% 
  mutate(across(where(is.character), as_factor))
class_summary <- sapply(df_02, function(x) class(x)[1])
table(class_summary)


# Look at Numeric Var
names(df_02 %>%
  select(where(~ is.numeric(.) & !is.factor(.))))

# Convert Numeric = number of cars. ("q16_BIL_ANTALL")
df_02 <- df_02 %>%
  mutate(q16_BIL_ANTALL = as_factor(q16_BIL_ANTALL)) %>%  # Convert to factor
  mutate(q16_BIL_ANTALL = fct_na_value_to_level(q16_BIL_ANTALL, level = "0"))  # Replace NA with "0"
# count
table(df_02$q16_BIL_ANTALL)

## "arsaker2_"
# Convert arsaker to factor - Preserve Metadata While Converting to Factors
class_summary <- sapply(df_02, function(x) class(x)[1])
table(class_summary)

# store names of numeric variables that are not factors (its only arsaker2_ left)
original_labels <- names(df_02 %>%
                           select(where(~ is.numeric(.) & !is.factor(.))))
original_labels


saveRDS(df_02, "df_cleaned_02.rds")  # Save to file
df_02 <- readRDS("df_cleaned_02.rds")  # Restore dataset





# Store original metadata (question labels)
metadata_labels <- sapply(original_labels, function(var) var_label(df_02[[var]]))
metadata_labels

df_02[original_labels] <- lapply(original_labels, function(var) {
  new_factor <- factor(ifelse(is.na(df_02[[var]]), 0, 1),  
                       levels = c(0, 1),
                       labels = c("Not Selected", metadata_labels[[var]]))  # Use question text for "Selected"
  
  # Restore metadata label after conversion
  var_label(new_factor) <- metadata_labels[[var]]
  
  return(new_factor)
})


# Check if the factor levels are correct
sapply(original_labels, function(var) levels(df_02[[var]]))

# Check if metadata labels were restored correctly
sapply(original_labels, function(var) var_label(df_02[[var]]))


# Check metadata BEFORE conversion
var_label(df_01$arsaker2_1)  # Should show original question label

# Check metadata AFTER conversion
var_label(df_02$arsaker2_1)  # Should still show the same label (not NULL)

# Ensure all variables are factors:
table(sapply(df_02, function(x) class(x)[1]))


# assign ID
df_02$ID <- 1:nrow(df_02)


# new df
df_temp <- df_02
df_02 <- df_temp
saveRDS(df_02, "df_cleaned_02.rds")  # Save to file
df_02 <- readRDS("df_cleaned_02.rds")  # Restore dataset




#####################################################################
# For Plotting: Filter out "Not Selected" before plotting
df_plot <- df_02 %>%
  pivot_longer(cols = all_of(original_labels), names_to = "Question", values_to = "Response") %>%
  filter(Response != "Not Selected")  # Exclude "Not Selected" responses




