## Organize vars into groups

# Define variable groups as named lists

# 1️⃣ Socio-Demographics
socio_demo_vars1 <- c("age_group", "c_kjonn", "utdanning", "yrkesstatus",
                     "hush_antpers", "familietype", "c_fylke_recode")

socio_demo_vars2 <- c("age_group", "c_kjonn", "utdanning", "yrkesstatus", 
                     "hush_antpers", "familietype")

# 2️⃣ Income & Economic Status
# income_vars <- c("inntekt_pers", "inntekt_hush", "q147_INNTEKT_HUSH") # the 2 last are similar

income_vars <- c("inntekt_pers", "q147_INNTEKT_HUSH")

# 3️⃣ Transport Accessibility & Public Transport
transport_access_vars <- c("q141_HJEM_STOPPESTED_d1", "q142_HJEMME_FREKVENS_2", 
                           "A4_transportmidler_arb", 
                           paste0("A4_transpmidler_somN", 1:6), 
                           paste0("A4_transpmidler_vinN", 1:6))

# 4️⃣ Car Ownership & Use
car_ownership_vars <- c(paste0("q15_BILEIE_", 1:7), "A1a_fkort", 
                        paste0("A1c_betale_", 1:5), "antall_biler", 
                        "drivstoff_bil1", "drivstoff_bil2", 
                        paste0("A5_bilbruk_grunnerN", 1:14))

# 4️⃣ Car Ownership & Use (Updated)
car_ownership_vars2 <- c(paste0("q15_BILEIE_", 1:7), "A1a_fkort", 
                        "A1c_betale_All",  # Consolidated variable
                        "antall_biler", 
                        "drivstoff_bil1", "drivstoff_bil2", 
                        paste0("A5_bilbruk_grunnerN", 1:14))


# 5️⃣ Attitudes Toward Transport & Environment
transport_attitudes_vars <- c(paste0("A6_holdn_symbolskN", 1:8), 
                              paste0("A6_holdn_identitetN", 1:6), 
                              paste0("A6_holdn_opplN", 1:7), 
                              paste0("A6_holdn_kunnsk_intN", 1:4), 
                              paste0("A7_koll_opplN", 1:5), 
                              paste0("A7_koll_arsakN", 1:13), 
                              paste0("A9_holdn_polN", 1:10), 
                              paste0("A10_holdn_delingN", 1:3), 
                              paste0("A11_holdn_miljoN", 1:12))

# Remove missing variables from the transport attitudes group
transport_attitudes_vars <- setdiff(transport_attitudes_vars, c("A11_holdn_miljoN6", "A11_holdn_miljoN7"))

# Update the variable_groups list
# variable_groups$transport_attitudes <- transport_attitudes_vars
# 

# 6️⃣ Activities & Lifestyle
activities_vars <- c(paste0("A12b_aktiviteter_uteN", 1:15), 
                     paste0("A12b_Arbeid2_", 1:4), 
                     "A12c_PermisjonN1")

# 7️⃣ Political Affiliation
political_vars <- c("A13_parti")


# 8️⃣ A2a Reasons (Multi-Select)
a2a_arsaker_vars <- c(paste0("A2a_arsaker_", 1:10))

# 8️⃣ A2a Reasons (Updated)
a2a_arsaker_vars2 <- c("DL_reason_need", "DL_reason_social", "DL_reason_personal")

# 9️⃣  Materia tangible assets 
a12_inntekt_vars <- c(paste0("A12_inntektN", c(1, 2, 3, 5)))



# Combine all variable groups
all_vars1 <- c(socio_demo_vars2, income_vars, transport_access_vars, 
              car_ownership_vars, transport_attitudes_vars, activities_vars, 
              political_vars, a2a_arsaker_vars, a12_inntekt_vars)

all_vars2 <- c(socio_demo_vars2, income_vars, transport_access_vars, 
               car_ownership_vars2, transport_attitudes_vars, activities_vars, 
               political_vars, a2a_arsaker_vars2, a12_inntekt_vars)



# Store all groups in a list for easy selection og groups
variable_groups1 <- list(
  socio_demo = socio_demo_vars2,
  income = income_vars,
  income_a12 = a12_inntekt_vars,
  transport_access = transport_access_vars,
  car_ownership = car_ownership_vars,
  transport_attitudes = transport_attitudes_vars,
  activities = activities_vars,
  political = political_vars,
  driving_licence_reason = a2a_arsaker_vars,
  tangible_assets = a12_inntekt_vars
)

# Store all groups in a list for easy selection og groups
variable_groups2 <- list(
  socio_demo = socio_demo_vars2,
  income = income_vars,
  income_a12 = a12_inntekt_vars,
  transport_access = transport_access_vars,
  car_ownership = car_ownership_vars2,
  transport_attitudes = transport_attitudes_vars,
  activities = activities_vars,
  political = political_vars,
  driving_licence_reason = a2a_arsaker_vars2,
  tangible_assets = a12_inntekt_vars
)

# Remove Vars with "Other" in the cat label
# Define the list of variables to remove
vars_to_remove <- c("q15_BILEIE_6", 
                    "A5_bilbruk_grunnerN8", 
                    "A7_koll_arsakN7", 
                    "A12b_aktiviteter_uteN3",
                    "A12b_aktiviteter_uteN7",
                    "A12b_aktiviteter_uteN15")


variable_groups3 <- lapply(variable_groups2, function(group) {
  setdiff(group, vars_to_remove)  # Keep only variables NOT in vars_to_remove
})




variable_groups = variable_groups2

# Check if all variables in `variable_groups` exist in `df_05` and opposite
all_vars <- unlist(variable_groups2)
df_vars <- names(df_06)
setdiff(all_vars, df_vars)
setdiff(df_vars, all_vars)




# ✅ Function to select a group for analysis
select_vars <- function(group_name) {
  if (group_name %in% names(variable_groups)) {
    return(variable_groups[[group_name]])
  } else {
    stop("Invalid group name. Choose from: ", paste(names(variable_groups), collapse = ", "))
  }
}

# ✅ Example: Select "socio_demo" group for analysis
selected_vars <- select_vars("socio_demo")
df_selected <- df_03[selected_vars]
selected_vars
table(df_selected$age_group)

# ✅ Check selected variables
str(df_selected)
