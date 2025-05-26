## Go through varaibles group-wise

library(labelled)
library(dplyr)
# library(purrr)
# library(forcats)  # fct_
library(tidyverse) # Has purr and forcast


## Load and make new df:
df_02 <- readRDS("data/processed/survey_cleaned_01.rds")
df_03 <- df_02

# Checking: random check of input data
table(df_03$A2a_arsaker_4, useNA= "always")  # A var I know
sum(is.na(df_03)) # Total NA values

# A summary for all variables:
summary_df <- data.frame(
  variable = names(df_03),
  is_factor = sapply(df_03, is.factor),
  n_levels = sapply(df_03, function(x) if (is.factor(x)) nlevels(x) else NA),
  n_missing = sapply(df_03, function(x) sum(is.na(x)))
)
summary_df



# => 1 DL yes/no
# Car-Behaviuor
# A1a_fkort
var_label(df_03$A1a_fkort)
levels(df_03$A1a_fkort)
table(df_03$A1a_fkort)

df_03$A1a_fkort <- fct_recode(df_03$A1a_fkort,
          "DL: No" = "nei, men er i ferd med å ta det",
          "DL: No" = "nei",
          "DL: Yes" = "ja"
)

# check
levels(df_03$A1a_fkort)
table(df_03$A1a_fkort)

saveRDS(df_03, "df_cleaned_03.rds")

# => 2 Who paid for DL
# Car-Behaviour
df_03 <- readRDS("df_cleaned_03.rds")
# A1c_betale_

var_label(df_03$A1c_betale_1)
lapply(df_03[, c("A1c_betale_1", "A1c_betale_2")], table)

# mapping for each column individually
df_03$A1c_betale_1 <- factor(df_03$A1c_betale_1,
                             levels = c("NS", " jeg betalte selv"),
                             labels = c("NS", "DL: paied self"))

df_03$A1c_betale_2 <- factor(df_03$A1c_betale_2,
                             levels = c("NS", " foreldre/slektninger betalte"),
                             labels = c("NS", "DL: family paid"))

# Check the frequency of each label after label mapping
lapply(df_03[, c("A1c_betale_1", "A1c_betale_2")], table)

# Merging
table(df_03$A1c_betale_1, df_03$A1c_betale_2)

df_03 <- df_03 %>%
  mutate(A1c_paid_merged = case_when(
    A1c_betale_1 == "DL: paied self" & A1c_betale_2 == "NS" ~ "DL: paid self",
    A1c_betale_1 == "NS" & A1c_betale_2 == "DL: family paid" ~ "DL: family paid",
    A1c_betale_1 == "DL: paied self" & A1c_betale_2 == "DL: family paid" ~ "DL: family paid",
    A1c_betale_1 == "NS" & A1c_betale_2 == "NS" ~ "NS",
  )) %>%
  mutate(A1c_paid_merged = factor(
    A1c_paid_merged,
    levels = c("NS", "DL: paid self", "DL: family paid")
  ))

table(df_03$A1c_paid_merged)

df_03 <- df_03 %>%
  select(-A1c_betale_1, -A1c_betale_2)



saveRDS(df_03, "df_cleaned_03.rds")


# => 3
# reason for having car
# Car-Attitude-Instrumental/Affection/
df_03 <- readRDS("df_cleaned_03.rds")
# A2a_arsaker_

lapply(df_03 %>% select(starts_with("A2a_arsaker_")), table)

# Define short modality labels (DLR = Driver’s License Reasons)
arsak_labels_short <- c(
  "A2a_arsaker_1"  = "DLR Parents Paid",
  "A2a_arsaker_2"  = "DLR Future Need",
  "A2a_arsaker_3"  = "DLR Moved",
  "A2a_arsaker_4"  = "DLR Got Job",
  "A2a_arsaker_5"  = "DLR Family Grew",
  "A2a_arsaker_6"  = "DLR Needed for Job",
  "A2a_arsaker_7"  = "DLR Social Life",
  "A2a_arsaker_8"  = "DLR Independent Travel",
  "A2a_arsaker_9"  = "DLR Friends Had It",
  "A2a_arsaker_10" = "DLR Wanted to Drive"
)

# Save original variable labels if needed
arsak_var_labels <- var_label(df_03)[names(df_03) %in% names(arsak_labels_short)]

# Recode for MCA with short modality levels
df_03[names(arsak_labels_short)] <- lapply(names(arsak_labels_short), function(var) {
  label <- arsak_labels_short[[var]]
  
  # Identify the value that is NOT "NS"
  selected_value <- setdiff(unique(df_03[[var]]), "NS")
  
  # Create "Yes"/"No" levels with short modality names
  yes_level <- paste0(label, ": Yes")
  no_level  <- paste0(label, ": No")
  
  factor(
    ifelse(df_03[[var]] == selected_value, yes_level, no_level),
    levels = c(no_level, yes_level)
  )
})

# Restore variable labels
var_label(df_03)[names(arsak_var_labels)] <- arsak_var_labels

# check
table(df_03$A2a_arsaker_1, useNA = "always")
lapply(df_03 %>% select(starts_with("A2a_arsaker_")), table, useNA = "always")

saveRDS(df_03, "df_cleaned_03.rds")



# => 6 Number of cars
# Car-Behaviour
df_03 <- readRDS("df_cleaned_03.rds")
# antall_biler
table(df_03$antall_biler)

# recode NS (was NA) to "0"
df_03$antall_biler <- fct_recode(df_03$antall_biler,
                                 "0"  = "NS",
                                 "3+" = "3",
                                 "3+" = "4 eller flere")  # merge to "3+")

# Convert to an ordered factor with proper labels
df_03$antall_biler <- factor(df_03$antall_biler,
                             levels = c("0", "1", "2", "3+"),
                             labels = c("Cars Owned: 0",
                                        "Cars Owned: 1", 
                                        "Cars Owned: 2", 
                                        "Cars Owned: 3+"),
                             ordered = TRUE)

table(df_03$antall_biler, useNA = "always")   # Check that missing values are now labeled as "Cars Owned: 0"

saveRDS(df_03, "df_cleaned_03.rds")


# => 7
# Fuel
# Car-Behavior
df_03 <- readRDS("df_cleaned_03.rds")
# drivstoff_bil
table(df_03$drivstoff_bil1, useNA = "always")
table(df_03$drivstoff_bil2, useNA = "always")


df_03$drivstoff_bil1 <- fct_recode(df_03$drivstoff_bil1,
                                   "Gasoline" = "bensin",
                                   "Diesel" = "diesel",
                                   "Electric" = "elektrisitet / strøm",
                                   
                                   # Merge all hybrids
                                   "Hybrid" = "bensin og elektrisitet_ ladbar (hybrid-ladbar)",
                                   "Hybrid" = "bensin og elektrisitet_ (hybrid- ikke ladbar)",
                                   "Hybrid" = "diesel og elektrisitet_ ladbar (hybrid-ladbar)",
                                   "Hybrid" = "diesel og elektrisitet_ (hybrid- ikke ladbar)"
                                   # Leave "NS" untouched
)

df_03$drivstoff_bil2 <- fct_recode(df_03$drivstoff_bil2,
                                   "Gasoline" = "bensin",
                                   "Diesel" = "diesel",
                                   "Electric" = "elektrisitet / strøm",
                                   
                                   # Merge all hybrids
                                   "Hybrid" = "bensin og elektrisitet_ ladbar (hybrid-ladbar)",
                                   "Hybrid" = "bensin og elektrisitet_ (hybrid- ikke ladbar)",
                                   "Hybrid" = "diesel og elektrisitet_ ladbar (hybrid-ladbar)",
                                   "Hybrid" = "diesel og elektrisitet_ (hybrid- ikke ladbar)"
                                   # Leave "NS" untouched
)

# Check tables
table(df_03$drivstoff_bil1, useNA = "always")
table(df_03$drivstoff_bil2, useNA = "always")

saveRDS(df_03, "df_cleaned_03.rds")



# => 8
# Work transport type
# Car/PT/Active commute - Behavior
df_03 <- readRDS("df_cleaned_03.rds")
# A4_transportmidler_arb 
table(df_03$A4_transportmidler_arb, useNA = "always")

df_03 <- df_03 %>%
  mutate(
    A4_transportmidler_arb = fct_recode(A4_transportmidler_arb,
                                        "Work Transp: Driver"       = "bil som sjåfør",
                                        "Work Transp: Passenger"    = "bil som passasjer",
                                        "Work Transp: Public"       = "kollektivtransport (buss, tog, trikk, t-bane, båt)",
                                        "Work Transp: Bicycle"      = "jeg sykler hele veien",
                                        "Work Transp: Walking"      = "jeg går eller løper/jogger hele veien",
                                        "Work Transp: Motorcycle"   = "moped/mc",
                                        "Work Transp: Not Working"  = "jobber ikke/går ikke på skole"
    )
  )

# Check 
table(df_03$A4_transportmidler_arb, useNA = "always")

saveRDS(df_03, "df_cleaned_03.rds")


# => 9
# general transport style
# Car/PT/Active transport - Behavior
df_03 <- readRDS("df_cleaned_03.rds")
# A4_transpmidler_som/vint

# Recoding of frequency categories
freq_map_final <- c(
  "5-7 ganger i uka"     = "Daily",
  "3-4 ganger i uka"     = "Weekly",
  "1-2 ganger i uka"     = "Weekly",
  "1-3 ganger i måneden" = "Monthly",
  "sjeldnere"            = "Monthly",
  "aldri"                = "NS",
  "NS"                   = "NS"
)

# Variable label prefixes
transport_labels <- c(
  "A4_transpmidler_somN1" = "Car Driver S:",
  "A4_transpmidler_somN2" = "Car Passenger S:",
  "A4_transpmidler_somN3" = "Public Transport S:",
  "A4_transpmidler_somN4" = "Bicycle S:",
  "A4_transpmidler_somN5" = "Walking S:",
  "A4_transpmidler_somN6" = "Motorcycle S:",
  "A4_transpmidler_vinN1" = "Car Driver W:",
  "A4_transpmidler_vinN2" = "Car Passenger W:",
  "A4_transpmidler_vinN3" = "Public Transport W:",
  "A4_transpmidler_vinN4" = "Bicycle W:",
  "A4_transpmidler_vinN5" = "Walking W:",
  "A4_transpmidler_vinN6" = "Motorcycle W:"
)

recode_transport_variable <- function(x, prefix) {
  x_chr <- trimws(as.character(x))
  
  # Apply recoding
  x_recoded <- dplyr::recode(x_chr, !!!freq_map_final, .default = "NS")
  
  # Prefix only if not NS
  x_labeled <- ifelse(x_recoded == "NS", "NS", paste(prefix, x_recoded))
  
  # Final level ordering
  all_levels <- c(
    paste(prefix, c("Daily", "Weekly", "Monthly")),
    "NS"
  )
  
  factor(x_labeled, levels = all_levels, ordered = TRUE)
}


# Store original variable labels
transport_var_labels <- var_label(df_03)[names(df_03) %in% names(transport_labels)]

# Apply transformation
for (varname in names(transport_labels)) {
  df_03[[varname]] <- recode_transport_variable(df_03[[varname]], transport_labels[[varname]])
}

# Restore variable labels
var_label(df_03)[names(transport_var_labels)] <- transport_var_labels

# check
table(df_03$A4_transpmidler_somN1, useNA = "always")
table(df_03$A4_transpmidler_vinN1, useNA = "always")


saveRDS(df_03, "df_cleaned_03.rds")



# => 10
# Why I use car
# Car-Attitude
df_03 <- readRDS("df_cleaned_03.rds")
# A5_bilbruk_grunnerN

table(df_03$A5_bilbruk_grunnerN1, useNA = "always")
table(df_03$A5_bilbruk_grunnerN2, useNA = "always")
table(df_03$A5_bilbruk_grunnerN3, useNA = "always")
table(df_03$A5_bilbruk_grunnerN4, useNA = "always")
table(df_02$A5_bilbruk_grunnerN5, useNA = "always")
table(df_03$A5_bilbruk_grunnerN6, useNA = "always")
table(df_02$A5_bilbruk_grunnerN7, useNA = "always")
table(df_03$A5_bilbruk_grunnerN8, useNA = "always")
table(df_03$A5_bilbruk_grunnerN9, useNA = "always")
table(df_03$A5_bilbruk_grunnerN10, useNA = "always")
table(df_03$A5_bilbruk_grunnerN11, useNA = "always")
table(df_03$A5_bilbruk_grunnerN12, useNA = "always")
table(df_03$A5_bilbruk_grunnerN13, useNA = "always")
table(df_03$A5_bilbruk_grunnerN14, useNA = "always")

car_reason_labels <- c(
  "A5_bilbruk_grunnerN1" = "Car use Habit: ",
  "A5_bilbruk_grunnerN2" = "Car use Only When Rushed: ",
  "A5_bilbruk_grunnerN3" = "Car use Time Efficient: ",
  "A5_bilbruk_grunnerN4" = "Car use Cheaper Than Transit: ",
  "A5_bilbruk_grunnerN5" = "Car use Multitasking: ",
  "A5_bilbruk_grunnerN6" = "Car Use Bad Public Transit: ",
  "A5_bilbruk_grunnerN7" = "Car Use Carrying Items: ",
  "A5_bilbruk_grunnerN8" = "Car Use Driving Others: ",
  "A5_bilbruk_grunnerN9" = "Car Use Running Errands: ",
  "A5_bilbruk_grunnerN10" = "Car Use Freedom of Schedule: ",
  "A5_bilbruk_grunnerN11" = "Car Use Control Over Travel: ",
  "A5_bilbruk_grunnerN12" = "Car Use Dislike Public Transit: ",
  "A5_bilbruk_grunnerN13" = "Car Use Easy Parking: ",
  "A5_bilbruk_grunnerN14" = "Car Use Only for Carpooling: "
)

# Extract labels only for car use variables BEFORE modifying values
car_reason_var_labels <- var_label(df_03)[names(df_03) %in% names(car_reason_labels)]


df_03 <- df_03 %>%
  mutate(across(all_of(names(car_reason_labels)), 
                ~ {
                  prefix <- car_reason_labels[cur_column()]
                  x_chr <- tolower(trimws(as.character(.)))
                  
                  new_val <- case_when(
                    x_chr == "helt enig" ~ paste0(prefix, "Strongly Agree"),
                    x_chr == "ganske enig" ~ paste0(prefix, "Agree"),
                    x_chr == "verken enig eller uenig" ~ paste0(prefix, "Neutral"),
                    x_chr == "ganske uenig" ~ paste0(prefix, "Disagree"),
                    x_chr == "helt uenig" ~ paste0(prefix, "Strongly Disagree"),
                    x_chr %in% c("vet ikke/ikke aktuelt", "ns") ~ "NS",
                    TRUE ~ NA_character_  # catch unexpected cases
                  )
                  
                  factor(new_val,
                         levels = c(
                           paste0(prefix, c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")),
                           "NS"
                         ),
                         ordered = TRUE)
                }))



# Restore variable labels after transformation
var_label(df_03)[names(car_reason_var_labels)] <- car_reason_var_labels

# check
table(df_03$A5_bilbruk_grunnerN1, useNA = "always")


saveRDS(df_03, "df_cleaned_03.rds")



# => 11
# Recreational Activities
# Behavior-Lifestyle
df_03 <- readRDS("df_cleaned_03.rds")
# A12b_aktiviteter_uteN

table(df_02$A12b_aktiviteter_uteN1)
table(df_03$A12b_aktiviteter_uteN2)
table(df_02$A12b_aktiviteter_uteN3)
table(df_03$A12b_aktiviteter_uteN4)
table(df_02$A12b_aktiviteter_uteN5)
table(df_03$A12b_aktiviteter_uteN6)
table(df_02$A12b_aktiviteter_uteN7)
table(df_03$A12b_aktiviteter_uteN8)
table(df_03$A12b_aktiviteter_uteN9)
table(df_02$A12b_aktiviteter_uteN10)
table(df_02$A12b_aktiviteter_uteN11)
table(df_02$A12b_aktiviteter_uteN12)
table(df_02$A12b_aktiviteter_uteN13)
table(df_02$A12b_aktiviteter_uteN14)
table(df_02$A12b_aktiviteter_uteN15)
# All in one go:
lapply(df_03[names(activity_labels)], function(x) table(x, useNA = "always"))

# Define labels for each activity variable
activity_labels <- c(
  "A12b_aktiviteter_uteN1" = "Exercise: ",
  "A12b_aktiviteter_uteN2" = "Organized Training: ",
  "A12b_aktiviteter_uteN3" = "Other Organized: ",
  "A12b_aktiviteter_uteN4" = "Café/Restaurant Visit: ",
  "A12b_aktiviteter_uteN5" = "Bar/Nightclub: ",
  "A12b_aktiviteter_uteN6" = "Cultural Events: ",
  "A12b_aktiviteter_uteN7" = "Escorting Others: ",
  "A12b_aktiviteter_uteN8" = "Spectating Sports: ",
  "A12b_aktiviteter_uteN9" = "Volunteering: ",
  "A12b_aktiviteter_uteN10" = "Meeting Friends/Family: ",
  "A12b_aktiviteter_uteN11" = "Hiking: ",
  "A12b_aktiviteter_uteN12" = "Religious Gathering: ",
  "A12b_aktiviteter_uteN13" = "Local Walks: ",
  "A12b_aktiviteter_uteN14" = "Grocery Shopping: ",
  "A12b_aktiviteter_uteN15" = "Other Shopping: "
)

# Save original variable labels
activity_var_labels <- var_label(df_03)[names(df_03) %in% names(activity_labels)]

# Define function to recode values with label prefix
activity_time_collapse <- function(varname, x) {
  label <- activity_labels[[varname]]
  time_category <- case_when(
    x == "5-7 ganger i uka" ~ "Weekly",
    x == "3-4 ganger i uka" ~ "Weekly",
    x == "1-2 ganger i uka" ~ "Weekly",
    x == "1-3 ganger i måneden" ~ "Monthly",
    x == "sjeldnere" ~ "Yearly",
    x == "aldri" ~ "Never",
    x == "NS" | is.na(x) ~ "NS",
    TRUE ~ NA_character_
  )
  
  # Only prefix label if not NS
  level_label <- ifelse(time_category == "NS", "NS", paste0(label, time_category))
  
  factor(
    level_label,
    levels = c(paste0(label, c("Weekly", "Monthly", "Yearly", "Never")), "NS")
  )
}

# Apply to all activity variables
df_03[names(activity_labels)] <- map2_dfc(
  names(activity_labels),
  df_03[names(activity_labels)],
  activity_time_collapse
)

# Restore variable labels
var_label(df_03)[names(activity_var_labels)] <- activity_var_labels


# checks
levels(df_03$A12b_aktiviteter_uteN1)
table(df_03$A12b_aktiviteter_uteN1, useNA = "always")
table(df_03$A12b_aktiviteter_uteN6, useNA = "always")
lapply(df_03[names(activity_labels)], table, useNA = "always")

saveRDS(df_03, "df_cleaned_03.rds")



# => 12
# Attituds to cars
# Car(PT)-Attitude-Instrumental/Symbolic/Affective
df_03 <- readRDS("df_cleaned_03.rds")
# A6_holdn_symbolskN
# A6_holdn_identitetN
# A6_holdn_opplN
# A6_holdn_kunnsk_intN

# Label prefix for each variable
car_attitude_labels <- c(
  "A6_holdn_symbolskN1" = "Car Attitude Symbolic Status: ",
  "A6_holdn_symbolskN2" = "Car Attitude Prefers Zero Emissions: ",
  "A6_holdn_symbolskN3" = "Car Attitude Purely Practical: ",
  "A6_holdn_symbolskN4" = "Car Attitude Prefers Luxury Cars: ",
  "A6_holdn_symbolskN5" = "Car Attitude Necessary with Kids: ",
  "A6_holdn_symbolskN6" = "Car Attitude Electric Cars Are a Trend: ",
  "A6_holdn_symbolskN7" = "Car Attitude Car Enthusiast: ",
  "A6_holdn_symbolskN8" = "Car Attitude Guilt-Free Future: ",
  
  "A6_holdn_identitetN1" = "Car Identity Good Looks Matter: ",
  "A6_holdn_identitetN2" = "Car Identity Buy More Expensive Cars: ",
  "A6_holdn_identitetN3" = "Car Identity Matches Personality: ",
  "A6_holdn_identitetN4" = "Car Identity Anti-Public Transit: ",
  "A6_holdn_identitetN5" = "Car Identity Looks Don’t Matter: ",
  "A6_holdn_identitetN6" = "Car Identity Price & Efficiency Matter: ",
  
  "A6_holdn_opplN1" = "Car Experience Enjoy Driving Alone: ",
  "A6_holdn_opplN2" = "Car Experience Loves Driving: ",
  "A6_holdn_opplN3" = "Car Experience Enjoys Speed: ",
  "A6_holdn_opplN4" = "Car Experience Freedom: ",
  "A6_holdn_opplN5" = "Car Experience Safety at Night: ",
  "A6_holdn_opplN6" = "Car Experience Public Transit Is Bad at Night: ",
  "A6_holdn_opplN7" = "Car Experience Easier to Get Out: ",
  
  "A6_holdn_kunnsk_intN1" = "Car Knowledge Knows Good Cars: ",
  "A6_holdn_kunnsk_intN2" = "Car Knowledge Knows Mechanics: ",
  "A6_holdn_kunnsk_intN3" = "Car Knowledge Research Before Buying: ",
  "A6_holdn_kunnsk_intN4" = "Car Knowledge Prefers Fast Cars: "
)

# Inspect before transform
lapply(df_03[names(car_attitude_labels)], table, useNA = "always")


# Save original labels for car attitude variables BEFORE modifying values
# Backup original labels before overwrite
car_attitude_var_labels <- var_label(df_03)[names(df_03) %in% names(car_attitude_labels)]

# Mapping of cleaned values
attitude_map <- c(
  "helt enig" = "Strongly Agree",
  "ganske enig" = "Agree",
  "verken enig eller uenig" = "Neutral",
  "ganske uenig" = "Disagree",
  "helt uenig" = "Strongly Disagree",
  "vet ikke/ikke aktuelt" = "NS"
)

# Recoding function
recode_car_attitudes <- function(varname, x) {
  label <- car_attitude_labels[[varname]]
  x_chr <- tolower(trimws(as.character(x)))
  
  response <- attitude_map[x_chr]
  response[is.na(response)] <- "NS"
  
  full_label <- ifelse(response == "NS", "NS", paste0(label, response))
  
  factor(
    full_label,
    levels = c(
      paste0(label, c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")),
      "NS"
    )
  )
}


# Apply transformation
recode_results <- map2(
  names(car_attitude_labels),
  df_03[names(car_attitude_labels)],
  recode_car_attitudes
)

df_03[names(car_attitude_labels)] <- as.data.frame(recode_results)

# Restore original labels
var_label(df_03)[names(car_attitude_var_labels)] <- car_attitude_var_labels


# check
sapply(df_03[names(car_attitude_labels)], function(x) sum(is.na(x)))
table(df_03$A6_holdn_symbolskN1, useNA = "always")
table(df_03$A6_holdn_identitetN3, useNA = "always")
table(df_03$A6_holdn_opplN4)
table(df_03$A6_holdn_opplN5)
table(df_03$A6_holdn_kunnsk_intN2)
levels(df_03$A6_holdn_symbolskN1)  # Should now correctly show all labels

saveRDS(df_03, "df_cleaned_03.rds")



# => 13
# PT
# PT-Attitude/Behaviour
df_03 <- readRDS("df_cleaned_03.rds")
# A7_koll_opplN
# A7_koll_arsakN

# Label prefixes for each public transport question
public_transport_labels <- c(
  "A7_koll_opplN1" = "PT Perception Likes Public Transport: ",
  "A7_koll_opplN2" = "PT Perception Feels Unsafe at Stops: ",
  "A7_koll_opplN3" = "PT Perception Feels Unsafe Walking to Stops: ",
  "A7_koll_opplN4" = "PT Perception Complicated Ticketing: ",
  "A7_koll_opplN5" = "PT Perception Social Benefits: ",
  
  "A7_koll_arsakN1" = "PT Reason Too Far: ",
  "A7_koll_arsakN2" = "PT Reason Avoid Crowds: ",
  "A7_koll_arsakN3" = "PT Reason Doesn't Fit Needs: ",
  "A7_koll_arsakN4" = "PT Reason Too Expensive: ",
  "A7_koll_arsakN5" = "PT Reason Uses PT for Work: ",
  "A7_koll_arsakN6" = "PT Reason Uses PT for School: ",
  "A7_koll_arsakN7" = "PT Reason Uses PT for Other Trips: ",
  "A7_koll_arsakN8" = "PT Reason Prefers PT for Relaxation: ",
  "A7_koll_arsakN9" = "PT Reason Avoids PT for Comfort: ",
  "A7_koll_arsakN10" = "PT Reason Avoids PT for Hygiene: ",
  "A7_koll_arsakN11" = "PT Reason Would Use PT If More Available: ",
  "A7_koll_arsakN12" = "PT Reason Never Uses PT: ",
  "A7_koll_arsakN13" = "PT Reason Takes Too Long: "
)

# Save original labels before modifying
public_transport_var_labels <- var_label(df_03)[names(df_03) %in% names(public_transport_labels)]

# Inspect before transform
lapply(df_03[names(public_transport_var_labels)], table, useNA = "always")

# Mapping of cleaned response values
pt_attitude_map <- c(
  "helt enig" = "Strongly Agree",
  "ganske enig" = "Agree",
  "verken enig eller uenig" = "Neutral",
  "ganske uenig" = "Disagree",
  "helt uenig" = "Strongly Disagree"
)

# Recoding function
recode_public_transport <- function(varname, x) {
  label <- public_transport_labels[[varname]]
  x_chr <- tolower(trimws(as.character(x)))
  
  response <- pt_attitude_map[x_chr]
  response[is.na(response)] <- "NS"  # Fallback to NS for "vet ikke/ikke aktuelt"
  
  full_label <- ifelse(response == "NS", "NS", paste0(label, response))
  
  factor(
    full_label,
    levels = c(
      paste0(label, c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")),
      "NS"
    )
  )
}

# Apply transformation safely
recode_pt_results <- map2(
  names(public_transport_labels),
  df_03[names(public_transport_labels)],
  recode_public_transport
)

df_03[names(public_transport_labels)] <- as.data.frame(recode_pt_results)

# Restore original labels
var_label(df_03)[names(public_transport_var_labels)] <- public_transport_var_labels

# Inspect after transform
lapply(df_03[names(public_transport_var_labels)], table, useNA = "always")



table(df_03$A7_koll_opplN1, useNA = "always")
levels(df_03$A7_koll_opplN1)  # Should show "Strongly Agree", "Agree", ..., "Not Applicable"

# Check summaries
table(df_03$A7_koll_opplN1)
table(df_03$A7_koll_arsakN6)
table(df_03$A7_koll_opplN3)
table(df_03$A7_koll_arsakN10)

saveRDS(df_03, "df_cleaned_03.rds")



# => 14
# political opinion
# Attitude/Political orientation
df_03 <- readRDS("df_cleaned_03.rds")
# A9_holdn_polN1

# Define label prefixes
transport_policy_labels <- c(
  "A9_holdn_polN1" = "Policy Supports Toll Roads: ",
  "A9_holdn_polN2" = "Policy Supports Toll Roads If Electric: ",
  "A9_holdn_polN3" = "Policy Supports Limited Parking: ",
  "A9_holdn_polN4" = "Policy Too Many Restrictions: ",
  "A9_holdn_polN5" = "Policy Reduce Private Car Use: ",
  "A9_holdn_polN6" = "Policy Free Workplace Parking: ",
  "A9_holdn_polN7" = "Policy Accepts Restrictions: ",
  "A9_holdn_polN8" = "Policy More Budget for Public Transit: ",
  "A9_holdn_polN9" = "Policy Prioritize Cycling Infrastructure: ",
  "A9_holdn_polN10" = "Policy Prioritize Public Transport Over Roads: "
)

# Save original variable labels
transport_policy_var_labels <- var_label(df_03)[names(df_03) %in% names(transport_policy_labels)]

# Inspect before transform
lapply(df_03[names(public_transport_var_labels)], table, useNA = "always")



# Define mapping (excluding "vet ikke/ikke aktuelt" so it becomes "NS")
policy_attitude_map <- c(
  "helt enig" = "Strongly Agree",
  "ganske enig" = "Agree",
  "verken enig eller uenig" = "Neutral",
  "ganske uenig" = "Disagree",
  "helt uenig" = "Strongly Disagree"
)

# Recoding function
recode_transport_policy <- function(varname, x) {
  label <- transport_policy_labels[[varname]]
  x_chr <- tolower(trimws(as.character(x)))
  
  response <- policy_attitude_map[x_chr]
  response[is.na(response)] <- "NS"
  
  full_label <- ifelse(response == "NS", "NS", paste0(label, response))
  
  factor(
    full_label,
    levels = c(
      paste0(label, c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")),
      "NS"
    )
  )
}

# Apply transformation safely
recode_policy_results <- map2(
  names(transport_policy_labels),
  df_03[names(transport_policy_labels)],
  recode_transport_policy
)

df_03[names(transport_policy_labels)] <- as.data.frame(recode_policy_results)

# Restore original variable labels
var_label(df_03)[names(transport_policy_var_labels)] <- transport_policy_var_labels

# Inspect after transform
lapply(df_03[names(transport_policy_var_labels)], table, useNA = "always")

# check
table(df_03$A9_holdn_polN1, useNA = "always")
table(df_03$A9_holdn_polN2, useNA = "always")
table(df_03$A9_holdn_polN6)
table(df_03$A9_holdn_polN3)
table(df_03$A9_holdn_polN10)

saveRDS(df_03, "df_cleaned_03.rds")




# => 15
# Sharing Economy
# political opinion
df_03 <- readRDS("df_cleaned_03.rds")
# A10_holdn_delingN

# Define Sharing Economy Labels with Consistent Naming Style
sharing_economy_labels <- c(
  "A10_holdn_delingN1" = "Supports Car Sharing: ",
  "A10_holdn_delingN2" = "Supports Bike Sharing: ",
  "A10_holdn_delingN3" = "Supports Airbnb: "
)

# Extract Labels Before Transformation
sharing_economy_var_labels <- var_label(df_03)[names(df_03) %in% names(sharing_economy_labels)]
# Inspect after transform
lapply(df_03[names(sharing_economy_var_labels)], table, useNA = "always")


sharing_attitude_map <- c(
  "helt enig" = "Strongly Agree",
  "ganske enig" = "Agree",
  "verken enig eller uenig" = "Neutral",
  "ganske uenig" = "Disagree",
  "helt uenig" = "Strongly Disagree"
)


recode_sharing_economy <- function(varname, x) {
  label <- sharing_economy_labels[[varname]]
  x_chr <- tolower(trimws(as.character(x)))
  
  response <- sharing_attitude_map[x_chr]
  response[is.na(response)] <- "NS"
  
  full_label <- ifelse(response == "NS", "NS", paste0(label, response))
  
  factor(
    full_label,
    levels = c(
      paste0(label, c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")),
      "NS"
    )
  )
}

recode_sharing_results <- map2(
  names(sharing_economy_labels),
  df_03[names(sharing_economy_labels)],
  recode_sharing_economy
)

df_03[names(sharing_economy_labels)] <- as.data.frame(recode_sharing_results)
var_label(df_03)[names(sharing_economy_var_labels)] <- sharing_economy_var_labels


#  check
table(df_03$A10_holdn_delingN1, useNA = "always")
table(df_03$A10_holdn_delingN2)
table(df_03$A10_holdn_delingN3)

saveRDS(df_03, "df_cleaned_03.rds")




# => 16
# Envinronoment
# Poltical opinion
df_03 <- readRDS("df_cleaned_03.rds")
# A11_holdn_miljoN


environmental_labels <- c(
  "A11_holdn_miljoN1" = "Env Cars Cause Pollution: ",
  "A11_holdn_miljoN2" = "Env My Transport Choices Don't Matter: ",
  "A11_holdn_miljoN3" = "Env Solutions Are Within Reach: ",
  "A11_holdn_miljoN4" = "Env Economy > Environment: ",
  "A11_holdn_miljoN5" = "Env Tech Will Solve Problems: ",
  "A11_holdn_miljoN8" = "Env Pays More for Eco-Friendly Goods: ",
  "A11_holdn_miljoN9" = "Env Chooses Local Activities: ",
  "A11_holdn_miljoN10" = "Env Fossil Cars Should Pay More: ",
  "A11_holdn_miljoN11" = "Env Eats Less Meat for Climate: ",
  "A11_holdn_miljoN12" = "Env Reduces Flights for Climate: "
)

environmental_var_labels <- var_label(df_03)[names(df_03) %in% names(environmental_labels)]

environmental_attitude_map <- c(
  "helt enig" = "Strongly Agree",
  "ganske enig" = "Agree",
  "verken enig eller uenig" = "Neutral",
  "ganske uenig" = "Disagree",
  "helt uenig" = "Strongly Disagree"
)

recode_environmental <- function(varname, x) {
  label <- environmental_labels[[varname]]
  x_chr <- tolower(trimws(as.character(x)))
  
  response <- environmental_attitude_map[x_chr]
  response[is.na(response)] <- "NS"
  
  full_label <- ifelse(response == "NS", "NS", paste0(label, response))
  
  factor(
    full_label,
    levels = c(
      paste0(label, c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")),
      "NS"
    )
  )
}

recode_env_results <- map2(
  names(environmental_labels),
  df_03[names(environmental_labels)],
  recode_environmental
)

df_03[names(environmental_labels)] <- as.data.frame(recode_env_results)

var_label(df_03)[names(environmental_var_labels)] <- environmental_var_labels


# check
table(df_03$A11_holdn_miljoN1, useNA = "always")

saveRDS(df_03, "df_cleaned_03.rds")




# => 17
# Material tangible accets
# Social structure: Social class and Culture space
df_03 <- readRDS("df_cleaned_03.rds")
#A12_inntektN


income_materialism_labels <- c(
  "A12_inntektN1" = "Income Success Indicator: ",
  "A12_inntektN2" = "Income Property Important for Good Life: ",
  "A12_inntektN3" = "Income Likes New Tech: ",
  "A12_inntektN5" = "Income Likes New Sports Gear: "
)

income_materialism_var_labels <- var_label(df_03)[names(df_03) %in% names(income_materialism_labels)]

income_attitude_map <- c(
  "helt enig" = "Strongly Agree",
  "ganske enig" = "Agree",
  "verken enig eller uenig" = "Neutral",
  "ganske uenig" = "Disagree",
  "helt uenig" = "Strongly Disagree"
)

recode_income_materialism <- function(varname, x) {
  label <- income_materialism_labels[[varname]]
  x_chr <- tolower(trimws(as.character(x)))
  
  response <- income_attitude_map[x_chr]
  response[is.na(response)] <- "NS"
  
  full_label <- ifelse(response == "NS", "NS", paste0(label, response))
  
  factor(
    full_label,
    levels = c(
      paste0(label, c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")),
      "NS"
    )
  )
}

recode_income_results <- map2(
  names(income_materialism_labels),
  df_03[names(income_materialism_labels)],
  recode_income_materialism
)

df_03[names(income_materialism_labels)] <- as.data.frame(recode_income_results)

var_label(df_03)[names(income_materialism_var_labels)] <- income_materialism_var_labels

# check
table(df_03$A12_inntektN1, useNA = "always")


saveRDS(df_03, "df_cleaned_03.rds")



# => 18
# Job preference
# Social structure
df_03 <- readRDS("df_cleaned_03.rds")
# A12b_Arbeid

lapply(df_03[, c("A12b_Arbeid", "A12b_Arbeid2_1", "A12b_Arbeid2_2", "A12b_Arbeid2_3", "A12b_Arbeid2_4")], table)

# Remove: A12b_Arbeid (not selectedd very much). Should be updated in modality_frequencies.csv
df_03 <- df_03 %>% select(-A12b_Arbeid)
df_03$A12b_Arbeid

lapply(df_03[, c("A12b_Arbeid2_1", "A12b_Arbeid2_2", "A12b_Arbeid2_3", "A12b_Arbeid2_4")], table)

# Define simplified and MCA-friendly labels
job_preference_labels <- c(
  "A12b_Arbeid2_1" = "Job Pref Career & Salary",
  "A12b_Arbeid2_2" = "Job Pref Low Responsibility",
  "A12b_Arbeid2_3" = "Job Pref Learning & Growth",
  "A12b_Arbeid2_4" = "Job Pref Social Impact"
)

# Save original variable labels (if needed later)
job_preference_var_labels <- var_label(df_03)[names(df_03) %in% names(job_preference_labels)]

# Recode each variable for MCA
df_03[names(job_preference_labels)] <- lapply(names(job_preference_labels), function(var) {
  base_label <- job_preference_labels[[var]]
  
  # Detect actual descriptive value (≠ "NS")
  selected_value <- setdiff(unique(df_03[[var]]), "NS")
  
  # Construct full labels for Yes/No
  yes_level <- paste0(base_label, ": Yes")
  no_level  <- paste0(base_label, ": No")
  
  # Recode based on selection
  factor(
    ifelse(df_03[[var]] == selected_value, yes_level, no_level),
    levels = c(no_level, yes_level)
  )
})

# Restore variable labels (optional, useful for documentation or output)
var_label(df_03)[names(job_preference_var_labels)] <- job_preference_var_labels


lapply(df_03[, c("A12b_Arbeid2_1", "A12b_Arbeid2_2", "A12b_Arbeid2_3", "A12b_Arbeid2_4")], table)


# Check summaries
table(df_03$A12b_Arbeid2_1, useNA = "always")
table(df_03$A12b_Arbeid2_2)
table(df_03$A12b_Arbeid2_3)
table(df_03$A12b_Arbeid2_4)

saveRDS(df_03, "df_cleaned_03.rds")


# => 19
# matertal leave
# Poltical space
df_03 <- readRDS("df_cleaned_03.rds")
# A12c_PermisjonN1
table(df_02$A12c_PermisjonN1)

# Define mapping
parental_leave_mapping <- c(
  "alle ukene til mor" = "Leave: All to Mother",
  "to tredjedeler av ukene til mor" = "Leave: 2/3 to Mother",
  "like mange uker til far og mor" = "Leave: Equal Split",
  "to tredjedeler av ukene til far" = "Leave: 2/3 to Father",
  "alle ukene til far" = "Leave: All to Father"
)

# Recode and convert to factor in one step
df_03 <- df_03 %>%
  mutate(A12c_PermisjonN1 = {
    recoded <- parental_leave_mapping[trimws(as.character(A12c_PermisjonN1))]
    recoded[is.na(recoded)] <- "NS"
    factor(recoded, levels = c(
      "Leave: All to Mother",
      "Leave: 2/3 to Mother",
      "Leave: Equal Split",
      "Leave: 2/3 to Father",
      "Leave: All to Father",
      "NS"
    ))
  })

# check
table(df_03$A12c_PermisjonN1, useNA = "always")
levels(df_03$A12c_PermisjonN1)  # Should show "Leave: All to Mother", ..., "Unknown"
var_label(df_03$A12c_PermisjonN1)

saveRDS(df_03, "df_cleaned_03.rds")



# => 20
# Party
df_03 <- readRDS("df_cleaned_03.rds")
# A13_parti

table(df_03$A13_parti)


# Raw-to-label mapping (only main parties)
party_mapping_simple <- c(
  "arbeiderpartiet" = "Party: Labour",
  "høyre" = "Party: Conservative",
  "fremskrittspartiet" = "Party: Right-Wing",
  "senterpartiet" = "Party: Centre",
  "kristelig folkeparti" = "Party: Christian Democrats",
  "venstre" = "Party: Liberal",
  "sosialistisk venstreparti" = "Party: Socialist Left",
  "miljøpartiet de grønne" = "Party: Green",
  "rødt" = "Party: Red"
)

# Store raw original values
original_party_raw <- df_03$A13_parti

# Save label
party_label <- var_label(df_03$A13_parti)

# Recode only from the raw values
df_03$A13_parti <- {
  input_clean <- tolower(trimws(as.character(original_party_raw)))
  recoded <- party_mapping_simple[input_clean]
  recoded[is.na(recoded)] <- "NS"
  factor(recoded, levels = c(
    "Party: Labour",
    "Party: Conservative",
    "Party: Right-Wing",
    "Party: Centre",
    "Party: Christian Democrats",
    "Party: Liberal",
    "Party: Socialist Left",
    "Party: Green",
    "Party: Red",
    "NS"
  ))
}

table(df_03$A13_parti)

saveRDS(df_03, "df_cleaned_03.rds")




##############

# => 22
df_03 <- readRDS("df_cleaned_03.rds")
# c_kjonn

table(df_02$c_kjonn)

# Save variable label
gender_label <- var_label(df_03$c_kjonn)

# Define mapping (case-insensitive)
gender_mapping <- c(
  "menn" = "Gender: Male",
  "kvinner" = "Gender: Female"
)

# Recode gender with fallback to "NS"
df_03 <- df_03 %>%
  mutate(c_kjonn = {
    input_clean <- tolower(trimws(as.character(c_kjonn)))
    recoded <- gender_mapping[input_clean]
    recoded[is.na(recoded)] <- "NS"
    factor(recoded, levels = c("Gender: Male", "Gender: Female", "NS"))
  })

# Restore variable label
var_label(df_03$c_kjonn) <- gender_label

# check
table(df_03$c_kjonn, useNA = "always")  # Should correctly categorize NAs


saveRDS(df_03, "df_cleaned_03.rds")



# => 23
# Car access
# Car behavior
df_03 <- readRDS("df_cleaned_03.rds")
# q15_BILEIE_1

table(df_03$q15_BILEIE_1, useNA = "always")
var_label(df_03$q15_BILEIE_1)


# Dine Car Access Labels with Consistent Naming Style
car_access_labels <- c(
  "q15_BILEIE_1" = "Car Owner",
  "q15_BILEIE_2" = "Car Borrowers",
  "q15_BILEIE_3" = "Car Sharing",
  "q15_BILEIE_4" = "Car Leaser",
  "q15_BILEIE_5" = "Car from Company",
  # "q15_BILEIE_6" = "NS", # Was "Car Other"
  "q15_BILEIE_7" = "Car No" # This should be chenged to Car: yes / Car: No, need to change the code bellow too
)

df_03$q15_BILEIE_6

# Backup variable labels
car_access_var_labels <- var_label(df_03)[names(df_03) %in% names(car_access_labels)]

# Transform each variable
df_03[names(car_access_labels)] <- lapply(names(car_access_labels), function(var) {
  label <- car_access_labels[[var]]
  x <- tolower(trimws(as.character(df_03[[var]])))
  
  recoded <- dplyr::case_when(
    x == "yes" ~ paste(label, "Yes", sep = ": "),
    x == "no"  ~ paste(label, "No", sep = ": "),
    x == "ns"  ~ "NS",   # ✅ clean passive category
    TRUE       ~ "NS"    # backup just in case
  )
  
  factor(recoded, levels = c("NS", paste(label, c("No", "Yes"), sep = ": ")))
})

# Restore variable labels
var_label(df_03)[names(car_access_var_labels)] <- car_access_var_labels


# check
table(df_03$q15_BILEIE_1, useNA = "always")
table(df_03$q15_BILEIE_2, useNA = "always")
table(df_03$q15_BILEIE_3, useNA = "always")
table(df_03$q15_BILEIE_4, useNA = "always")
table(df_03$q15_BILEIE_5, useNA = "always")
table(df_03$q15_BILEIE_7, useNA = "always")

saveRDS(df_03, "df_cleaned_03.rds")


# => 24
df_03 <- readRDS("df_cleaned_03.rds")
# df_03 <- df_03 %>% select(-q16_BIL_ANTALL)
table(df_03$q16_BIL_ANTALL, useNA = "always")
class(df_03$q16_BIL_ANTALL)

# Convert to character for flexible handling
car_raw <- as.character(df_03$q16_BIL_ANTALL)

# Create binned version
df_03$q16_BIL_ANTALL <- dplyr::case_when(
  car_raw == "NS"           ~ "car own: 0",
  car_raw == "1"           ~ "car own: 1",
  car_raw == "2"           ~ "car own: 2",
  car_raw == "3"           ~ "car own: 3",
  car_raw %in% c("4", "5", "6", "7", "8") ~ "car own: 4+",
)

df_03$q16_BIL_ANTALL <- factor(df_03$q16_BIL_ANTALL,
                               levels = c("car own: 0", "car own: 1", "car own: 2", 
                                          "car own: 3", "car own: 4+"),
                               ordered = TRUE)


# check
table(df_03$q16_BIL_ANTALL, useNA = "always")
class(df_03$q16_BIL_ANTALL)
mode(df_03$q16_BIL_ANTALL)

saveRDS(df_03, "df_cleaned_03.rds")


# => 25
# Distance to PT
# PT structure

# NB: Need FIX
# NB: some few numerics is converted into NA and then into NS
df_03 <- readRDS("df_cleaned_03.rds")
# q141_HJEM_STOPPESTED_d1

table(df_03$q141_HJEM_STOPPESTED_d1, useNA = "always")


# Save variable label
distance_label <- var_label(df_03$q141_HJEM_STOPPESTED_d1)

# Recode with one helper column, no warnings
df_03 <- df_03 %>%
  mutate(
    q141_HJEM_STOPPESTED_d1 = as.character(q141_HJEM_STOPPESTED_d1),
    q141_num = suppressWarnings(as.numeric(q141_HJEM_STOPPESTED_d1)),
    q141_HJEM_STOPPESTED_d1 = case_when(
      q141_HJEM_STOPPESTED_d1 == "NS"             ~ "NS",
      q141_num < 0.2                               ~ "Distance: Very Close",
      q141_num < 0.5                               ~ "Distance: Close",
      q141_num < 1                                 ~ "Distance: Moderate",
      q141_num < 2                                 ~ "Distance: Far",
      q141_num < 5                                 ~ "Distance: Very Far",
      q141_num <= 20                               ~ "Distance: Extremely Far",
      TRUE                                         ~ "NS"  # Safety fallback
    )
  ) %>%
  select(-q141_num)  # Clean up helper

# Convert to factor
distance_levels <- c(
  "Distance: Very Close",
  "Distance: Close",
  "Distance: Moderate",
  "Distance: Far",
  "Distance: Very Far",
  "Distance: Extremely Far",
  "NS"
)

df_03 <- df_03 %>%
  mutate(q141_HJEM_STOPPESTED_d1 = factor(q141_HJEM_STOPPESTED_d1, levels = distance_levels))

# Restore label
var_label(df_03$q141_HJEM_STOPPESTED_d1) <- distance_label


# check
table(df_03$q141_HJEM_STOPPESTED_d1, useNA = "always")


saveRDS(df_03, "df_cleaned_03.rds")





# => 26
# PT freq
# PT strutural
df_03 <- readRDS("df_cleaned_03.rds")
# q142_HJEMME_FREKVENS_2

table(df_03$q142_HJEMME_FREKVENS_2, useNA = "always")

df_03$q142_HJEMME_FREKVENS_2 <- fct_recode(df_03$q142_HJEMME_FREKVENS_2,
     "PT Freq: 12+ per Hour" = "12 ganger eller mer (5 min mellom avgangene)",
     "PT Freq: 8 per Hour"   = "8 ganger (7,5 min mellom avgangene)",
     "PT Freq: 6 per Hour"   = "6 ganger (10 min mellom avgangene)",
     "PT Freq: 4 per Hour"   = "4 ganger (15 min mellom avgangene)",
     "PT Freq: 2-3 per Hour" = "2 – 3 ganger per time",  # Make sure this dash matches
     "PT Freq: 1 per Hour"   = "1 gang per time",
     "PT Freq: Seldom"       = "hver annen time",
     "PT Freq: Seldom"       = "sjeldnere",
     "NS"                    = "NS")  # use consistent label for NS



df_03$q142_HJEMME_FREKVENS_2 <- factor(df_03$q142_HJEMME_FREKVENS_2,
               levels = c("PT Freq: 1 per Hour",
                          "PT Freq: 2-3 per Hour",
                          "PT Freq: 4 per Hour",
                          "PT Freq: 6 per Hour",
                          "PT Freq: 8 per Hour",
                          "PT Freq: 12+ per Hour",
                          "PT Freq: Seldom",
                          "NS"),
               ordered = TRUE)

table(df_03$q142_HJEMME_FREKVENS_2, useNA = "always")
levels(df_03$q142_HJEMME_FREKVENS_2)
df_03$q142_HJEMME_FREKVENS_2

is.factor(df_03$q142_HJEMME_FREKVENS_2)
sum(is.na(df_03$q142_HJEMME_FREKVENS_2))

mode(df_03$q142_HJEMME_FREKVENS_2)
class(df_03$q142_HJEMME_FREKVENS_2)
# Side-by-side comparison
compare_df <- data.frame(
  works = df_03$A5_bilbruk_grunnerN1,
  fails = df_03$q142_HJEMME_FREKVENS_2
)

sapply(compare_df, function(x) list(
  class = class(x),
  levels = levels(as.factor(x)),
  table = table(x, useNA = "always")
))

df_03$q142_HJEMME_FREKVENS_2 <- factor(as.character(df_03$q142_HJEMME_FREKVENS_2))

                        

saveRDS(df_03, "df_cleaned_03.rds")



# => 27
# houshold income
# Social class
df_03 <- readRDS("df_cleaned_03.rds")
# q147_INNTEKT_HUSH
table(df_03$q147_INNTEKT_HUSH)

# Save original variable label
income_label <- var_label(df_03$q147_INNTEKT_HUSH)

# Define grouped binning
income_mapping_binned <- c(
  "under 200 000 nok"                         = "Houshold income: <400k",
  "mellom 200 000 og 399 999 nok"             = "Houshold income: <400k",
  "mellom 400 000 og 599 999 nok"             = "Houshold income: 400–799k",
  "mellom 600 000 og 799 999 nok"             = "Houshold income: 400–799k",
  "mellom 800 000 og 999 999 nok"             = "Houshold income: 800–999k",
  "mellom 1 000 000 og 1 599 999 nok"         = "Houshold income: 1–1.59M",
  "mellom 1 600 000 og 1 999 999 nok"         = "Houshold income: 1.6–1.99M",
  "2 000 000 nok og over"                     = "Houshold income: 2M+"
)

# Transform variable
df_03 <- df_03 %>%
  mutate(q147_INNTEKT_HUSH = {
    raw <- tolower(trimws(as.character(q147_INNTEKT_HUSH)))
    recoded <- income_mapping_binned[raw]
    recoded[is.na(recoded) | raw %in% c("ønsker ikke å oppgi", "vet ikke", "ns")] <- "NS"
    factor(recoded, levels = c(
      "Houshold income: <400k",
      "Houshold income: 400–799k",
      "Houshold income: 800–999k",
      "Houshold income: 1–1.59M",
      "Houshold income: 1.6–1.99M",
      "Houshold income: 2M+",
      "NS"
    ), ordered = TRUE)
  })

# Restore label
var_label(df_03$q147_INNTEKT_HUSH) <- income_label


levels(df_03$q147_INNTEKT_HUSH)  # Should show all ordered levels including "NS"
table(df_03$q147_INNTEKT_HUSH, useNA = "always")  # Should correctly categorize values


saveRDS(df_03, "df_cleaned_03.rds")



# => 28
# Emplyment
# Social class
df_03 <- readRDS("df_cleaned_03.rds")
# yrkesstatus
table(df_03$yrkesstatus, useNA = "always")

# Save original label
employment_label <- var_label(df_03$yrkesstatus)

# Mapping of known categories
employment_mapping <- c(
  "yrkesaktiv, inntektsgivende arbeid" = "Employment: Employed",
  "alderspensjonist, afp eller andre tidligpensjoner" = "Employment: Retired",
  "går på skole/studerer" = "Employment: Student",
  "hjemmeværende" = "Employment: Homemaker",
  "militærtjeneste" = "Employment: Military Service",
  "fødsels- foreldrepermisjon" = "Employment: Parental Leave",
  "langvarig sykemeldt" = "Employment: Long-term Sick Leave",
  "arbeidsledig" = "Employment: Unemployed"
)

# Recode safely
df_03 <- df_03 %>%
  mutate(yrkesstatus = {
    raw <- tolower(trimws(as.character(yrkesstatus)))
    recoded <- employment_mapping[raw]
    recoded[is.na(recoded)] <- "NS"
    factor(recoded, levels = c(
      "Employment: Employed",
      "Employment: Retired",
      "Employment: Student",
      "Employment: Homemaker",
      "Employment: Military Service",
      "Employment: Parental Leave",
      "Employment: Long-term Sick Leave",
      "Employment: Unemployed",
      "NS"
    ))
  })

# Restore label
var_label(df_03$yrkesstatus) <- employment_label

# Verification
levels(df_03$yrkesstatus)  # Should show all employment categories + "NS"
table(df_03$yrkesstatus, useNA = "always")


saveRDS(df_03, "df_cleaned_03.rds")




# => 29 
# educatio
# Social class
df_03 <- readRDS("df_cleaned_03.rds")
# utdanning

# Save label
education_label <- var_label(df_03$utdanning)

# Mapping
education_mapping <- c(
  "grunnskole (inkl. ungdomsskole/framhaldsskole/realskole)" = "Education: Primary",
  "videregående (inkl. gymnas/yrkesskole/handelsskole/påbygging)" = "Education: Secondary",
  "høyskole/universitet – lavere grad (til og med 4 år)" = "Education: Tertiary (Lower)",
  "høyskole/universitet – høyere grad (5 eller flere år)" = "Education: Tertiary (Higher)"
)

# Recode safely
df_03 <- df_03 %>%
  mutate(utdanning = {
    raw <- tolower(trimws(as.character(utdanning)))
    recoded <- education_mapping[raw]
    recoded[is.na(recoded) | raw == "ns"] <- "NS"
    factor(recoded, levels = c(
      "Education: Primary",
      "Education: Secondary",
      "Education: Tertiary (Lower)",
      "Education: Tertiary (Higher)",
      "NS"
    ), ordered = TRUE)
  })

# Restore label
var_label(df_03$utdanning) <- education_label

# Check
table(df_03$utdanning, useNA = "always")


saveRDS(df_03, "df_cleaned_03.rds")



# => 30
# income
df_03 <- readRDS("df_cleaned_03.rds")
# inntekt_pers
# inntekt_hush
# Check
table(df_03$inntekt_pers, useNA = "always")

# Save label
personal_income_label <- var_label(df_03$inntekt_pers)

# Mapping
personal_income_mapping <- c(
  "under 200.000 kroner"            = "Income: <300k",
  "200.000 - 299.999 kroner"        = "Income: <300k",
  "300.000 - 399.999 kroner"        = "Income: 300–499k",
  "400.000 - 499.999 kroner"        = "Income: 300–499k",
  "500.000 - 599.999 kroner"        = "Income: 500–699k",
  "600.000 - 699.999 kroner"        = "Income: 500–699k",
  "700.000 - 799.999 kroner"        = "Income: 700–999k",
  "800.000 - 999.999 kroner"        = "Income: 700–999k",
  "1.000.000 kroner eller mer"      = "Income: 1M+"
)

# Recode with fallback to "NS"
df_03 <- df_03 %>%
  mutate(inntekt_pers = {
    raw <- tolower(trimws(as.character(inntekt_pers)))
    recoded <- personal_income_mapping[raw]
    recoded[is.na(recoded) | raw == "ns"] <- "NS"
    factor(recoded, levels = c(
      "Income: <300k",
      "Income: 300–499k",
      "Income: 500–699k",
      "Income: 700–999k",
      "Income: 1M+",
      "NS"
    ), ordered = TRUE)
  })

# Restore label
var_label(df_03$inntekt_pers) <- personal_income_label


# Check
table(df_03$inntekt_pers, useNA = "always")

########## NEXT
# inntekt_hush
# Check
table(df_03$inntekt_hush, useNA = "always")

# Save original label
hush_income_label <- var_label(df_03$inntekt_hush)

# Define mapping
hush_income_mapping <- c(
  "under 200.000 kroner"         = "Income: <400k",
  "200.000 - 399.999 kroner"     = "Income: <400k",
  "400.000 - 599.999 kroner"     = "Income: 400–799k",
  "600.000 – 799.999 kroner"     = "Income: 400–799k",
  "800.000 - 999.999 kroner"     = "Income: 800–999k",
  "1.000.000 kroner eller mer"   = "Income: 1M+"
)

# Apply transformation
df_03 <- df_03 %>%
  mutate(inntekt_hush = {
    raw <- tolower(trimws(as.character(inntekt_hush)))
    recoded <- hush_income_mapping[raw]
    recoded[is.na(recoded) | raw %in% c("ønsker ikke å svare", "ns")] <- "NS"
    factor(recoded, levels = c(
      "Income: <400k",
      "Income: 400–799k",
      "Income: 800–999k",
      "Income: 1M+",
      "NS"
    ), ordered = TRUE)
  })

# Restore label
var_label(df_03$inntekt_hush) <- hush_income_label

# check
table(df_03$inntekt_hush, useNA = "always") 


saveRDS(df_03, "df_cleaned_03.rds")




# => 31
# houshold
# structural
df_03 <- readRDS("df_cleaned_03.rds")
# hush_antpers
# check
table(df_03$hush_antpers, useNA = "always") 

# Save variable label
household_size_label <- var_label(df_03$hush_antpers)

# Recode factor levels
df_03 <- df_03 %>%
  mutate(hush_antpers = fct_recode(
    hush_antpers,
    "Household Size: 1"  = "1 person",
    "Household Size: 2"  = "2 personer",
    "Household Size: 3"  = "3 personer",
    "Household Size: 4"  = "4 personer",
    "Household Size: 5+" = "5 eller flere personer"
  )) %>%
  mutate(hush_antpers = factor(
    hush_antpers,
    levels = c(
      "Household Size: 1",
      "Household Size: 2",
      "Household Size: 3",
      "Household Size: 4",
      "Household Size: 5+",
      "NS"
    ),
    ordered = TRUE
  ))


# Restore label
var_label(df_03$hush_antpers) <- household_size_label

# check
table(df_03$hush_antpers, useNA = "always")


saveRDS(df_03, "df_cleaned_03.rds")



# => 32
# houshold type
# structural
df_03 <- readRDS("df_cleaned_03.rds")
# familietype

table(df_03$familietype)

# Save original label
family_type_label <- var_label(df_03$familietype)

# Mapping of known family types
family_type_mapping <- c(
  "enslig"             = "Family Type: Single",
  "enslig med barn"    = "Family Type: Single Parent",
  "par uten barn"      = "Family Type: Couple, No Kids",
  "par med barn"       = "Family Type: Couple with Kids",
  "flere voksne"       = "Family Type: Multiple Adults"
)

# Apply mapping
df_03 <- df_03 %>%
  mutate(familietype = {
    raw <- tolower(trimws(as.character(familietype)))
    recoded <- family_type_mapping[raw]
    recoded[is.na(recoded)] <- "NS"  # Only needed if any value wasn't matched
    factor(recoded, levels = c(
      "Family Type: Single",
      "Family Type: Single Parent",
      "Family Type: Couple, No Kids",
      "Family Type: Couple with Kids",
      "Family Type: Multiple Adults",
      "NS"
    ))
  })

# Restore label
var_label(df_03$familietype) <- family_type_label

# check
table(df_03$familietype, useNA = "always")

saveRDS(df_03, "df_cleaned_03.rds")


# => 33
# alder
df_03 <- readRDS("df_cleaned_03.rds")

table(df_03$alder, useNA = "always")
class(df_03$alder)

# Save label
age_label <- var_label(df_03$alder)

# Recode into age groups
df_03 <- df_03 %>%
  mutate(alder = {
    raw <- trimws(as.character(alder))
    # Convert numeric where possible
    age_num <- suppressWarnings(as.numeric(raw))
    
    age_group <- case_when(
      is.na(age_num) | raw == "NS" ~ "NS",
      age_num >= 16 & age_num <= 24 ~ "Age: 16–24",
      age_num >= 25 & age_num <= 34 ~ "Age: 25–34",
      age_num >= 35 & age_num <= 44 ~ "Age: 35–44",
      age_num >= 45 & age_num <= 54 ~ "Age: 45–54",
      age_num >= 55 & age_num <= 64 ~ "Age: 55–64",
      age_num >= 65 & age_num <= 74 ~ "Age: 65–74",
      age_num >= 75                ~ "Age: 75+",
      TRUE ~ "NS"
    )
    
    factor(age_group, levels = c(
      "Age: 16–24",
      "Age: 25–34",
      "Age: 35–44",
      "Age: 45–54",
      "Age: 55–64",
      "Age: 65–74",
      "Age: 75+",
      "NS"
    ), ordered = TRUE)
  })

# Restore label
var_label(df_03$alder) <- age_label

table(df_03$alder, useNA = "always")
class(df_03$alder)



## Save df ready for MCA
saveRDS(df_03, file = "data/processed/survey_cleaned_final.rds")

# load
df_03 <- readRDS("data/processed/survey_cleaned_final.rds")

##################################################################
df_mca <- df_03 %>% select(-ID)
df_mca <- df_mca %>% select(-q142_HJEMME_FREKVENS_2)

result <- soc.mca(active = df_mca)
result <- soc.mca(df_mca, passive = "NS")

names(df_mca)
##################################################################
## Check All data:
summary(df_03)

# Check for Dirty "NS" Levels
find_bad_ns_levels <- function(df) {
  lapply(names(df), function(var) {
    x <- df[[var]]
    if (!is.factor(x)) return(NULL)
    
    bad <- levels(x)[grepl("\\bNS\\b", levels(x)) & levels(x) != "NS"]
    if (length(bad) > 0) data.frame(variable = var, bad_level = bad) else NULL
  }) %>%
    purrr::compact() %>%
    dplyr::bind_rows()
}

ns_issues <- find_bad_ns_levels(df_03)
ns_issues


anyNA(df_03)
names(df_03)[sapply(df_03, function(x) all(as.character(x) == "NS"))]




library(soc.ca)

names(df_03)
# Remove ID column if it exists
df_mca <- df_03 %>% select(-ID)
names(df_mca)

df_mca$q142_HJEMME_FREKVENS_2 <- fct_recode(df_mca$q142_HJEMME_FREKVENS_2, "Unknown" = "NS")



# check all factor
are.factor <- sapply(df_mca, is.factor)
df_mca[!are.factor]

# all vars have 2 or more values
which(sapply(df_mca, function(x) length(unique(x))) <= 1)

# library(FactoMineR)
# result <- MCA(df_mca, graph = TRUE)



# Run MCA on all variables
result <- soc.mca(active = df_mca)
result <- soc.mca(df_mca, passive = "NS")

result <- soc.mca(active = df_mca[, -1], identifier = df_mca$ID)

# View results
summary(result)
plot(result)



df_mca$q142_HJEMME_FREKVENS_2 <- factor(as.character(df_mca$q142_HJEMME_FREKVENS_2))
df_mca$q142_HJEMME_FREKVENS_2 <- droplevels(df_mca$q142_HJEMME_FREKVENS_2)

summary(df_mca$q142_HJEMME_FREKVENS_2)
levels(df_mca$q142_HJEMME_FREKVENS_2)



# Does soc.mca() run without crashing?
# Are "NS" values treated as passive (i.e., not appearing as active points)?
# Do variable clouds look well-formed, not collapsed?
# Do you spot very rare modalities that distort the space?



vars <- names(df_mca)
failing_vars <- c()

for (v in vars) {
  cat("\nTesting variable:", v, "\n")
  tryCatch({
    soc.mca(df_mca[, v, drop = FALSE], passive = "NS")
    cat("✅ Success\n")
  }, error = function(e) {
    cat("❌ Error:", e$message, "\n")
    failing_vars <<- c(failing_vars, v)
  })
}

# View all failing variables at the end
cat("\n\n=== Summary of Failing Variables ===\n")
print(failing_vars)


lapply(failing_vars, function(v) {
  x <- df_mca[[v]]
  used <- unique(as.character(x))
  list(
    n_levels = nlevels(x),
    used_levels = used,
    n_used = length(used),
    has_NS = any(used == "NS"),
    n_non_NS = sum(used != "NS"),
    colon_in_levels = any(grepl(":", levels(x))),
    level_names = levels(x)
  )
})

anyNA(df_mca$q142_HJEMME_FREKVENS_2)
sum(is.na(df_mca$q142_HJEMME_FREKVENS_2))

df_03$q142_HJEMME_FREKVENS_2
table(df_03$q142_HJEMME_FREKVENS_2, useNA = "always")
levels(df_03$q142_HJEMME_FREKVENS_2)
unique(df_03$q142_HJEMME_FREKVENS_2[df_03$q142_HJEMME_FREKVENS_2 == "NS"])



working_vars <- setdiff(names(df_mca), failing_vars)

working_with_colons <- working_vars[sapply(df_mca[working_vars], function(x) {
  is.factor(x) && any(grepl(":", levels(x)))
})]

working_with_colons

df_03$A1a_fkort

vars <- names(df_mca)
error_log <- list()

for (i in 1:(length(vars) - 1)) {
  for (j in (i + 1):length(vars)) {
    pair <- c(vars[i], vars[j])
    result <- tryCatch({
      soc.mca(df_mca[, pair])
      NULL
    }, error = function(e) {
      list(pair = pair, error = e$message)
    })
    
    if (!is.null(result)) {
      error_log[[length(error_log) + 1]] <- result
    }
  }
}

# Summarize failing variables
failing_vars <- unlist(lapply(error_log, function(e) e$pair))
failing_summary <- sort(table(failing_vars), decreasing = TRUE)

failing_summary


result <- soc.mca(df_mca, passive = "NS")






vars_with_only_one_active <- names(df_mca)[sapply(df_mca, function(x) {
  nlevels(x) > 1 && sum(levels(x) != "NS") == 1
})]
df_mca_clean <- df_mca[, !(names(df_mca) %in% vars_with_only_one_active)]
result <- soc.mca(df_mca_clean, passive = "NS")

vars_with_one_used_non_ns <- names(df_mca)[sapply(df_mca, function(x) {
  used_levels <- unique(as.character(x))
  sum(used_levels != "NS") == 1
})]

df_mca_clean2 <- df_mca[, !(names(df_mca) %in% vars_with_one_used_non_ns)]

result <- soc.mca(df_mca_clean2, passive = "NS")


df_mca_test <- df_03[, c("A1c_betale_1", "A1c_betale_2")]

table(df_03[, c("A1c_betale_1", "A1c_betale_2")], useNA = "always")


df_mca_test

df_mca_test$A1c_betale_1 <- factor(df_mca_test$A1c_betale_1,
                                   levels = c("NS", "DL: paied self", "DL: dummy"))
df_mca_test$A1c_betale_2 <- factor(df_mca_test$A1c_betale_2,
                                   levels = c("NS", "DL: family paid", "DL: dummy"))


table(df_mca_test$A1c_betale_1, useNA = "always")


lapply(df_mca_test, table)
unique(df_mca_test)
result <- soc.mca(df_mca_test)
result <- soc.mca(df_mca_test, passive = "NS")


ind_mat <- soc.ca::indicator(df_mca_test)
colnames(ind_mat)


result <- soc.mca(df_03$q142_HJEMME_FREKVENS_2)
result <- soc.mca(data.frame(q142_HJEMME_FREKVENS_2 = df_03$q142_HJEMME_FREKVENS_2), passive = "NS")

table(df_03$q142_HJEMME_FREKVENS_2, useNA = "always")

active_rows <- df_03$q142_HJEMME_FREKVENS_2 != "NS"
table(df_03$q142_HJEMME_FREKVENS_2[active_rows])


# For q141
ind_141 <- indicator(data.frame(df_03$q141_HJEM_STOPPESTED_d1))
colnames(ind_141)
colSums(ind_141)

# For q142
ind_142 <- indicator(data.frame(df_03$q142_HJEMME_FREKVENS_2))
colnames(ind_142)
colSums(ind_142)

result <- soc.mca(
  df_03[, c("A12b_Arbeid2_1", "A12b_Arbeid2_2", "A12b_Arbeid2_3", "A12b_Arbeid2_4"), drop = FALSE],
  passive = "NS"
)


# Select only the relevant variables for the MCA
mca_vars <- df_03[, names(arsak_labels_short)]
mca_vars
# Run a quick MCA using soc.mca (specific to binary nominal vars)
mca_test <- soc.mca(mca_vars, passive = "NS")



#################
table(df_02$q142_HJEMME_FREKVENS_2, useNA = "always")

# ✅ Define Mapping for Public Transport Frequency
frequency_mapping <- c(
  "12 ganger eller mer (5 min mellom avgangene)" = "PT Freq: 12+ per Hour",
  "8 ganger (7,5 min mellom avgangene)" = "PT Freq: 8 per Hour",
  "6 ganger (10 min mellom avgangene)" = "PT Freq: 6 per Hour",
  "4 ganger (15 min mellom avgangene)" = "PT Freq: 4 per Hour",
  "2 – 3 ganger per time" = "PT Freq: 2-3 per Hour",
  "1 gang per time" = "PT Freq: 1 per Hour",
  "hver annen time" = "PT Freq: Every Other Hour",
  "sjeldnere" = "PT Freq: Less Frequent",
  "NS" = "PT Freq: Unknown"
)


# ✅ Convert to Character Before Recoding
df_03 <- df_02 %>%
  mutate(q142_HJEMME_FREKVENS_2 = as.character(q142_HJEMME_FREKVENS_2))


df_03 <- df_02 %>%
  mutate(q142_HJEMME_FREKVENS_2 = recode(q142_HJEMME_FREKVENS_2, !!!frequency_mapping)) %>%
  mutate(q142_HJEMME_FREKVENS_2 = ifelse(is.na(q142_HJEMME_FREKVENS_2), "PT Freq: Unknown", q142_HJEMME_FREKVENS_2)) %>%
  mutate(q142_HJEMME_FREKVENS_2 = factor(q142_HJEMME_FREKVENS_2, 
                                         levels = unique(c(unname(frequency_mapping), "PT Freq: Unknown")),  # ✅ Ensure unique levels
                                         ordered = TRUE))  # ✅ Ensure ordered factor for MCA


table(df_03$q142_HJEMME_FREKVENS_2, useNA = "always")  # "PT Freq: Unknown" should match NA count


saveRDS(df_03, "df_cleaned_03.rds")



library(dplyr)
library(stringr)

# 1. Define the mapping
frequency_mapping <- c(
  "12 ganger eller mer (5 min mellom avgangene)" = "PT Freq: 12 per Hour",
  "8 ganger (7,5 min mellom avgangene)" = "PT Freq: 8 per Hour",
  "6 ganger (10 min mellom avgangene)" = "PT Freq: 6 per Hour",
  "4 ganger (15 min mellom avgangene)" = "PT Freq: 4 per Hour",
  "2 - 3 ganger per time" = "PT Freq: 2-3 per Hour",
  "1 gang per time" = "PT Freq: 1 per Hour",
  "hver annen time" = "PT Freq: Every Other Hour",
  "sjeldnere" = "PT Freq: Less Frequent",
  "NS" = "PT Freq: Unknown"
)

# 2. Clean and normalize input values, then recode
df_03 <- df_02 %>%
  mutate(q142_HJEMME_FREKVENS_2 = as.character(q142_HJEMME_FREKVENS_2)) %>%
  mutate(q142_HJEMME_FREKVENS_2 = str_squish(q142_HJEMME_FREKVENS_2)) %>%
  mutate(q142_HJEMME_FREKVENS_2 = str_replace_all(q142_HJEMME_FREKVENS_2, "–|−|—", "-")) %>%  # normalize dashes
  mutate(q142_HJEMME_FREKVENS_2 = recode(q142_HJEMME_FREKVENS_2, !!!frequency_mapping)) %>%
  mutate(q142_HJEMME_FREKVENS_2 = ifelse(is.na(q142_HJEMME_FREKVENS_2), "PT Freq: Unknown", q142_HJEMME_FREKVENS_2)) %>%
  mutate(q142_HJEMME_FREKVENS_2 = factor(q142_HJEMME_FREKVENS_2,
                                         levels = unique(c(unname(frequency_mapping), "PT Freq: Unknown")),
                                         ordered = TRUE))

# 3. Check the results
table(df_03$q142_HJEMME_FREKVENS_2, useNA = "always")




