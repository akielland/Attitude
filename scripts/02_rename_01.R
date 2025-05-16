

📌 Step 1: Extract Existing Variable Names & Metadata
We first create a dataframe of all variable names along with:
  
  Current labels (from metadata)
Type (e.g., binary, multiple-choice, ordinal, categorical)
Suggested new name (empty for now)


# Extract metadata for all variables
variable_info <- data.frame(
  Original_Name = names(df_mca),
  Label = sapply(names(df_mca), function(var) var_label(df_mca[[var]])),  # Get question text
  Type = sapply(names(df_mca), function(var) class(df_mca[[var]])[1]),  # Factor, Ordered, etc.
  Suggested_New_Name = NA  # Placeholder for manually updating names
)

# Save to CSV for manual review
write.csv(variable_info, "variable_metadata.csv", row.names = FALSE)




📌 Step 2: Apply Pattern-Based Auto-Renaming
Some names can be automatically improved using pattern matching, e.g.:
  
  Multiple-choice variables (A5_bilbruk_grunnerN1) → "Reason_Car_1"
Ordinal variables (q16_BIL_ANTALL) → "Num_Cars_Owned"
Binary choice variables (A1c_betale_1) → "Paid_By_Self"


# Auto-generate suggested names for common patterns
variable_info$Suggested_New_Name <- variable_info$Original_Name

# Rename multiple-choice variables systematically
variable_info$Suggested_New_Name <- gsub("^A5_bilbruk_grunnerN", "Reason_Car_", variable_info$Suggested_New_Name)
variable_info$Suggested_New_Name <- gsub("^q16_BIL_ANTALL", "Num_Cars_Owned", variable_info$Suggested_New_Name)

# Manually review cases where pattern-matching might not work
write.csv(variable_info, "variable_metadata_updated.csv", row.names = FALSE)



📌 Step 3: Update Factor Level Labels
For categorical and multiple-choice variables, we also rename the factor levels to make them more readable.

# Example: Rename levels of multiple-choice questions
levels(df_mca$Reason_Car_1) <- c("Not Selected", "Convenience")
levels(df_mca$Reason_Car_2) <- c("Not Selected", "Time-saving")
levels(df_mca$Reason_Car_3) <- c("Not Selected", "Family")

# Example: Rename ordinal variables
levels(df_mca$q16_BIL_ANTALL) <- c("None", "One", "Two", "Three", "Four or more")



📌 Step 4: Apply Renaming & Save Cleaned Data

# Apply new names
names(df_mca) <- variable_info$Suggested_New_Name

# Save cleaned dataset
saveRDS(df_mca, "cleaned_mca_dataset.rds")  # Saves as R object
write.csv(df_mca, "cleaned_mca_dataset.csv", row.names = FALSE)  # Save for external use
