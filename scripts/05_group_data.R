



# ─────────────────────────────────────────────────────────────
#  DEFINE VARIABLE GROUPS FOR MCA ANALYSIS
# ─────────────────────────────────────────────────────────────


# 1️⃣ SOCIO-DEMOGRAPHICS (with income included)
socio_demo_vars <- c(
  "alder",         # Age group
  "c_kjonn",           # Gender
  "utdanning",         # Education level
  "yrkesstatus",       # Employment status
  "hush_antpers",      # Household size
  "familietype",       # Family type
  "inntekt_pers",      # Personal income
  "q147_INNTEKT_HUSH",  # Household income
  "A1c_paid_merged",
  paste0("A2a_arsaker_", 1:10)  # Add all A2a_arsaker variables
)

# 2️⃣ TRANSPORT BEHAVIOR
transport_behavior_vars <- c(
  "A1a_fkort",                         # Driving license
  "A1c_paid_merged",                   # Payment consolidated
  "antall_biler", "drivstoff_bil1", "drivstoff_bil2",
  paste0("A5_bilbruk_grunnerN", setdiff(1:14, 8)),  # Reasons for car use (exclude 8 = "Other")
  "A4_transportmidler_arb",
  paste0("A4_transpmidler_somN", 1:6),
  paste0("A4_transpmidler_vinN", 1:6),
  "q141_HJEM_STOPPESTED_d1"          # Distance to stop
  # "q142_HJEMME_FREKVENS_2"            # Frequency
)

# 3️⃣ TRANSPORT ATTITUDES & ENVIRONMENTAL VALUES
transport_attitudes_vars <- c(
  paste0("A6_holdn_symbolskN", 1:8),
  paste0("A6_holdn_identitetN", 1:6),
  paste0("A6_holdn_opplN", 1:7),
  paste0("A6_holdn_kunnsk_intN", 1:4),
  paste0("A7_koll_opplN", 1:5),
  paste0("A7_koll_arsakN", setdiff(1:13, 7)),  # Exclude 7 = "Other"
  paste0("A9_holdn_polN", 1:10),
  paste0("A10_holdn_delingN", 1:3),
  paste0("A11_holdn_miljoN", c(1:5, 8:12))  # Skip missing 6 and 7
)

# 4️⃣ LIFESTYLE & ACTIVITIES
lifestyle_vars <- c(
  paste0("A12b_aktiviteter_uteN", 1:15),
  paste0("A12b_Arbeid2_", 1:4),
  "A12c_PermisjonN1"
)

# 5️⃣ POLITICAL AFFILIATION & VALUES
political_vars <- c(
  "A13_parti",
  paste0("A11_holdn_miljoN", c(1:5, 8:12)),  # Also used here
  paste0("A9_holdn_polN", 1:10),
  paste0("A12_inntektN", setdiff(1:5, 4))
)

# ─────────────────────────────────────────────────────────────
# ORGANIZE INTO NAMED LIST FOR EASY ACCESS
# ─────────────────────────────────────────────────────────────

variable_groups_mca <- list(
  socio_demo = socio_demo_vars, 
  transport_behavior = transport_behavior_vars,
  transport_attitudes = transport_attitudes_vars,
  lifestyle = lifestyle_vars,
  political = political_vars
)

# Preview the structure
str(variable_groups_mca)






get_unassigned_vars <- function(data, variable_groups) {
  # Flatten all group variables into one vector
  assigned_vars <- unique(unlist(variable_groups))
  
  # Find all column names in the dataset
  all_vars <- names(data)
  
  # Return variables that are NOT in any group
  unassigned_vars <- setdiff(all_vars, assigned_vars)
  
  return(unassigned_vars)
}
unassigned <- get_unassigned_vars(df_mca, variable_groups_mca)

# View or inspect the ungrouped variables
print(unassigned)
length(unassigned)




