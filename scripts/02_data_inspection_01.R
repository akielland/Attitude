# ✅ Pre-MCA Data Preparation Workflow

# 1️⃣ Inspect and Understand Each Variable
# Categorize variables (binary, multi-level categorical, ordinal, unexpected types)
# Identify variables with high NA proportions
# Group variables by questionnaire section (using prefixes)
# Check metadata labels to ensure they align with the variable content

# 2️⃣ Rename Variables to English (Shorter & Meaningful)
# Rename factor levels to ensure meaningful plotting
# Rename variable names to be compact yet descriptive
# Remove special characters from variable names if needed

# 3️⃣ Address Missing Data
# Remove variables with too many missing values (>20%)
# Decide how to handle missing values (NS, imputation, or exclusion)

# 4️⃣ Ensure Categorical Variables are Properly Formatted
# Convert necessary variables to factors and check factor levels
# Ensure binary and multiple-choice questions are consistently coded (0/1, "Selected"/"Not Selected")
# Ensure all ordered variables are explicitly defined as ordered factors


## Re-load data from the raw cleaning

saveRDS(df_02, "df_cleaned_02.rds")

df_02 <- readRDS("df_cleaned_02.rds")


saveRDS(df_03, "df_cleaned_03.rds")

df_03 <- readRDS("df_cleaned_03.rds")



## 🔍 1. INSPECTION: Categorize & Group Variables

# Identify variable types
ordinal_vars <- names(df_02)[sapply(df_02, function(x) is.ordered(x))]
multi_level_vars <- names(df_02)[sapply(df_02, function(x) is.factor(x) && nlevels(x) > 2)]
binary_vars <- names(df_02)[sapply(df_02, function(x) is.factor(x) && nlevels(x) == 2)]
character_vars <- names(df_02)[sapply(df_02, is.character)]
numeric_vars <- setdiff(names(df_02)[sapply(df_02, is.numeric)], binary_vars)  # Exclude binary vars
single_level_vars <- names(df_02)[sapply(df_02, function(x) is.factor(x) && nlevels(x) == 1)]  # Ensure it's factor

# Create summary table
inspection_summary <- data.frame(
  Category = c("Ordinal", "Multi-Level", "Binary", "Single-Level", "Character", "Numeric"),
  Count = c(length(ordinal_vars), length(multi_level_vars), length(binary_vars),
            length(single_level_vars), length(character_vars), length(numeric_vars))
)
inspection_summary

# Verify that all variables are accounted for
total_count <- sum(inspection_summary$Count)
if (total_count == ncol(df_02)) {
  cat("\n✅ All variables are categorized!\n")
} else {
  cat("\n⚠ WARNING: Some variables are unclassified! Need further investigation.\n")
}

ncol(df_02)
total_count
# probably the ID 
# single-level factors
single_level_vars
numeric_vars

setdiff(names(df_02), c(ordinal_vars, multi_level_vars, binary_vars, single_level_vars, character_vars, numeric_vars))
# Some var is counted twice:
# age_group is both in ordinal and multi_level_vars: so it is both factor and ordinal


# Extract first X letters (adjust if needed)
prefix_length <- 7
variable_groups <- substr(names(df_mca), 1, prefix_length)
# Count distinct question groups
length(unique(variable_groups))
as.data.frame(table(variable_groups))

#  Make Decisions for Each Group
# For each group you identified, decide whether to:
# ✅ Keep (if categorical and well-formed)
# ✂️ Clean (if too many levels, spars)
# ❌ Remove (if too much missing data, redundant, or irrelevant)
# rename it to short english label

# Function to step through each group interactively and display metadata
inspect_groups_with_metadata <- function(df, variable_groups) {
  group_names <- unique(variable_groups)  # Extract unique group prefixes
  total_groups <- length(group_names)  # Total number of groups
  
  for (i in seq_along(group_names)) {
    group <- group_names[i]  # Current group
    
    cat("\n\n==================================================\n")
    cat(sprintf("📌 Inspecting Group %d of %d: %s\n", i, total_groups, group))
    cat("==================================================\n")
    
    # Select variables that match the group prefix
    group_vars <- names(df)[grepl(paste0("^", group), names(df))]
    
    if (length(group_vars) > 0) {
      for (var in group_vars) {
        # Retrieve metadata (question text)
        question <- var_label(df[[var]])  # Extract variable label
        
        cat("\n🔹 Variable:", var, "\n")
        if (!is.null(question)) {
          cat("   ❓ Question:", question, "\n")
        } else {
          cat("   ❓ Question: [No metadata found]\n")
        }
      }
      
      # Print summary for the group
      cat("\n📊 Summary Statistics:\n")
      print(summary(df[group_vars]))
      
      # Print NA proportions
      cat("\n🔍 NA Proportions:\n")
      print(sapply(df[group_vars], function(x) mean(is.na(x))))
      
      # Show progress count
      cat(sprintf("\n✅ Completed %d out of %d groups.\n", i, total_groups))
      
      # Wait for user input before proceeding to the next group
      readline(prompt = "\nPress [Enter] to continue to the next group...")
    } else {
      cat("⚠️ No variables found for this group.\n")
    }
  }
  
  cat("\n🎉 All groups reviewed!\n")
}

inspect_groups_with_metadata(df_02, variable_groups)




#################### END
### NOT In Use;
inspect_and_label_variables <- function(df, variable_groups) {
  group_names <- unique(variable_groups)  # Extract unique group prefixes
  total_groups <- length(group_names)  # Total number of groups
  
  # Create a dataframe to store decisions
  review_log <- data.frame(
    Variable = character(),
    Original_Label = character(),
    Decision = character(),
    New_Label = character(),
    NA_Proportion = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(group_names)) {
    group <- group_names[i]  # Current group
    
    cat("\n\n==================================================\n")
    cat(sprintf("📌 Inspecting Group %d of %d: %s\n", i, total_groups, group))
    cat("==================================================\n")
    
    # Select variables that match the group prefix
    group_vars <- names(df)[grepl(paste0("^", group), names(df))]
    
    if (length(group_vars) > 0) {
      for (var in group_vars) {
        # Retrieve metadata (question text)
        question <- var_label(df[[var]])  # Extract variable label
        na_prop <- mean(is.na(df[[var]]))  # Compute NA proportion
        
        cat("\n🔹 Variable:", var, "\n")
        if (!is.null(question)) {
          cat("   ❓ Question:", question, "\n")
        } else {
          cat("   ❓ Question: [No metadata found]\n")
        }
        
        cat("   📊 NA Proportion:", round(na_prop * 100, 2), "%\n")
        print(summary(df[[var]]))
        
        # Get user decision
        decision <- readline("\n❓ Decision: [K]eep, [C]lean, [R]emove? ")
        
        # Get new label if variable is kept
        new_label <- ifelse(decision %in% c("K", "C"),
                            readline("✏ Enter new short English label: "), 
                            NA)
        
        # Store decision in log
        review_log <- rbind(review_log, data.frame(
          Variable = var,
          Original_Label = question,
          Decision = decision,
          New_Label = new_label,
          NA_Proportion = na_prop,
          stringsAsFactors = FALSE
        ))
      }
      
      # Show progress count
      cat(sprintf("\n✅ Completed %d out of %d groups.\n", i, total_groups))
      readline(prompt = "\nPress [Enter] to continue to the next group...")
    } else {
      cat("⚠️ No variables found for this group.\n")
    }
  }
  
  cat("\n🎉 All groups reviewed!\n")
  
  # Save log for reference
  return(review_log)
}



# Extract variable prefixes for grouping
prefix_length <- 7
variable_groups <- substr(names(df_mca), 1, prefix_length)

# Start interactive review
review_log <- inspect_and_label_variables(df_mca, variable_groups)

# Save log for later use
write.csv(review_log, "variable_review_log.csv", row.names = FALSE)






# 🏷 2. RENAMING VARIABLES & FACTOR LEVELS
# You want shorter, clearer English labels. The best approach is to map old names to new ones.


# Create a dictionary of old and new names (manually or using a mapping file)
rename_dict <- c(
  "arsaker2_1" = "No License - Cost",
  "arsaker2_2" = "No License - No Time",
  "arsaker2_4" = "No License - No Need",
  "arsaker2_5" = "No License - Prefer Other Expenses",
  "arsaker2_6" = "No License - Public Transport",
  "arsaker2_7" = "No License - Ride With Friends",
  "arsaker2_9" = "No License - Fear of Driving",
  "arsaker2_10" = "No License - Will Wait",
  "arsaker2_11" = "No License - Environmental Reasons",
  "arsaker2_13" = "No License - No Specific Reason"
)

# Rename variables in df_mca
names(df_mca) <- rename_dict[names(df_mca)]







