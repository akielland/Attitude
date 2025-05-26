# scripts/03_loop_inspect.R

library(labelled)
library(dplyr)
library(readr)



# Load data and existing decision table
df <- readRDS("data/processed/survey_responses.rds")
checklist_path <- "output/inspection/variable_overview.csv"
checklist <- read_csv(checklist_path, show_col_types = FALSE)

# Convert haven-labelled vars to factors just for inspection
df_factor <- df
df_factor[sapply(df, is.labelled)] <- lapply(df[sapply(df, is.labelled)], to_factor)

# === Helper function ===
inspect_variable <- function(varname) {
  cat("\n", strrep("=", 80), "\n")
  cat("Variable:", varname, "\n")
  label <- var_label(df[[varname]])
  if (!is.null(label)) cat("Label:   ", label, "\n")
  cat("Type:    ", class(df[[varname]])[1], "\n")
  cat("Missing: ", sum(is.na(df[[varname]])), "/", length(df[[varname]]),
      sprintf(" (%.1f%%)", 100 * mean(is.na(df[[varname]]))), "\n")
  cat("Unique values: ", n_distinct(df[[varname]], na.rm = TRUE), "\n\n")
  
  # Print value frequencies
  x <- df_factor[[varname]]
  if (is.factor(x) || is.character(x)) {
    print(table(x, useNA = "always"))
  } else {
    print(summary(x))
  }
}

# === Interactive loop ===
for (i in seq_len(nrow(checklist))) {
  row <- checklist[i, ]
  if (!is.na(row$include_mca)) next  # Skip already reviewed
  
  varname <- row$variable
  
  # === Print counter and progress ===
  cat("\n", strrep("=", 80), "\n")
  cat(sprintf("Reviewing variable %d of %d", i, nrow(checklist)), "\n")
  cat(sprintf("Completed: %d | Remaining: %d", 
              sum(!is.na(checklist$include_mca)), 
              sum(is.na(checklist$include_mca))), "\n")
  cat("Variable name:", varname, "\n")
  
  # === Call inspection function ===
  inspect_variable(varname)
  
  # === Prompt user ===
  include <- readline("Include in MCA? (yes / no / maybe): ")
  recode  <- readline("Needs recoding? (yes / no): ")
  note    <- readline("Notes (optional): ")
  
  # === Update checklist ===
  checklist$include_mca[i]  <- tolower(trimws(include))
  checklist$needs_recode[i] <- tolower(trimws(recode))
  checklist$notes[i]        <- note
  
  # === Save ===
  write_csv(checklist, checklist_path)
  cat("✅ Progress saved.\n")
}



# SIMPLER LOOP

inspect_variable <- function(data, varname) {
  x <- data[[varname]]
  
  cat("\n", strrep("=", 80), "\n")
  cat("Variable:", varname, "\n")
  
  if (!is.null(var_label(data[[varname]]))) {
    cat("Label:   ", var_label(data[[varname]]), "\n")
  }
  
  cat("Type:    ", class(x), "\n")
  cat("Missing: ", sum(is.na(x)), " / ", length(x),
      sprintf(" (%.1f%%)", 100 * mean(is.na(x))), "\n")
  cat("Unique values: ", n_distinct(x, na.rm = TRUE), "\n")
  
  if (is.factor(x) || is.character(x)) {
    print(table(x, useNA = "always"))
  } else {
    summary(x)
    cat("Possible codes: ", paste(sort(unique(x)), collapse = ", "), "\n")
  }
}


i = 0
for (v in names(df)) {
  inspect_variable(df, v)
  i = i + 1
  print("Var number: ", i)
  readline("Press [Enter] to continue to the next variable...")
}


