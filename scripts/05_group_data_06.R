

# Social Space variables
social_data <- df_06[, c("age_group", "c_kjonn", "familietype", "hush_antpers",
                         "q142_HJEMME_FREKVENS_2", "q141_HJEM_STOPPESTED_d1",
                         "q147_INNTEKT_HUSH", "inntekt_pers", "utdanning", "yrkesstatus", "A1c_betale_All")]


# Lifestyle variables
lifestyle_data <- df_06[, c("A12b_aktiviteter_uteN1", "A12b_aktiviteter_uteN2",  
                            "A12b_aktiviteter_uteN4", "A12b_aktiviteter_uteN5", "A12b_aktiviteter_uteN6", 
                             "A12b_aktiviteter_uteN8", "A12b_aktiviteter_uteN9", 
                            "A12b_aktiviteter_uteN10", "A12b_aktiviteter_uteN11", "A12b_aktiviteter_uteN12", 
                            "A12b_aktiviteter_uteN13", "A12b_aktiviteter_uteN14")]

# removed: A12b_aktiviteter_uteN3, A12b_aktiviteter_uteN7, A12b_aktiviteter_uteN15,



#  Political Opinions variables
political_opinions_data <- df_06[, c("A13_parti", "A12c_PermisjonN1", "A12b_Arbeid2_1", "A12b_Arbeid2_2", 
                                     "A12b_Arbeid2_3", "A12b_Arbeid2_4", "A12_inntektN1", "A12_inntektN2", 
                                     "A12_inntektN3", "A12_inntektN5", "A11_holdn_miljoN1", "A11_holdn_miljoN2", 
                                     "A11_holdn_miljoN3", "A11_holdn_miljoN4", "A11_holdn_miljoN5", "A11_holdn_miljoN8", 
                                     "A11_holdn_miljoN9", "A11_holdn_miljoN10", "A11_holdn_miljoN11", "A11_holdn_miljoN12", 
                                     "A10_holdn_delingN1", "A10_holdn_delingN2", "A10_holdn_delingN3", "A9_holdn_polN1", 
                                     "A9_holdn_polN2", "A9_holdn_polN3", "A9_holdn_polN4", "A9_holdn_polN5", "A9_holdn_polN6", 
                                     "A9_holdn_polN7", "A9_holdn_polN8", "A9_holdn_polN9", "A9_holdn_polN10")]





# Transport Behavior variables
transport_behavior_data <- df_06[, c("q15_BILEIE_1", "q15_BILEIE_2", "q15_BILEIE_3", "q15_BILEIE_4", "q15_BILEIE_5", 
                                      "q15_BILEIE_7", "A4_transpmidler_somN1", "A4_transpmidler_somN2", 
                                     "A4_transpmidler_somN3", "A4_transpmidler_somN4", "A4_transpmidler_somN5", "A4_transpmidler_somN6", 
                                     "A4_transpmidler_vinN1", "A4_transpmidler_vinN2", "A4_transpmidler_vinN3", "A4_transpmidler_vinN4", 
                                     "A4_transpmidler_vinN5", "A4_transpmidler_vinN6", "A4_transportmidler_arb", "drivstoff_bil1", 
                                     "drivstoff_bil2", "antall_biler", "A1a_fkort")]
# removed: q15_BILEIE_6

transport_behavior_data_test <- transport_behavior_data %>%
  mutate(across(where(is.factor), ~ factor(gsub("Not_Specified", "Other_Specified", as.character(.)))))




# Fix DL labels
transport_behavior_data$A1a_fkort <- factor(transport_behavior_data$A1a_fkort, 
                                            labels = paste0("A1a_fkort: ", levels(transport_behavior_data$A1a_fkort)))

# Fix Cars Owned labels
transport_behavior_data$antall_biler <- factor(transport_behavior_data$antall_biler, 
                                               labels = paste0("antall_biler: ", levels(transport_behavior_data$antall_biler)))
# remove ekstra prefix if ran multiple times
transport_behavior_data$antall_biler <- factor(gsub("antall_biler: ", "", transport_behavior_data$antall_biler))
transport_behavior_data$A1a_fkort <- factor(gsub("A1a_fkort: ", "", transport_behavior_data$A1a_fkort))

colnames(transport_behavior_data)
lapply(transport_behavior_data, levels)




# Define Transport Attitude variables
transport_attitude_data <- df_06[, c("A7_koll_opplN1", "A7_koll_opplN2", "A7_koll_opplN3", "A7_koll_opplN4", "A7_koll_opplN5", 
                                     "A7_koll_arsakN1", "A7_koll_arsakN2", "A7_koll_arsakN3", "A7_koll_arsakN4", "A7_koll_arsakN5", 
                                     "A7_koll_arsakN6", "A7_koll_arsakN8", "A7_koll_arsakN9", "A7_koll_arsakN10", 
                                     "A7_koll_arsakN11", "A7_koll_arsakN12", "A7_koll_arsakN13", "A6_holdn_symbolskN1", "A6_holdn_symbolskN2", 
                                     "A6_holdn_symbolskN3", "A6_holdn_symbolskN4", "A6_holdn_symbolskN5", "A6_holdn_symbolskN6", "A6_holdn_symbolskN7", 
                                     "A6_holdn_symbolskN8", "A6_holdn_identitetN1", "A6_holdn_identitetN2", "A6_holdn_identitetN3", "A6_holdn_identitetN4", 
                                     "A6_holdn_identitetN5", "A6_holdn_identitetN6", "A6_holdn_opplN1", "A6_holdn_opplN2", "A6_holdn_opplN3", "A6_holdn_opplN4", 
                                     "A6_holdn_opplN5", "A6_holdn_opplN6", "A6_holdn_opplN7", "A6_holdn_kunnsk_intN1", "A6_holdn_kunnsk_intN2", "A6_holdn_kunnsk_intN3", "A6_holdn_kunnsk_intN4", 
                                     "A5_bilbruk_grunnerN1", "A5_bilbruk_grunnerN2", "A5_bilbruk_grunnerN3", "A5_bilbruk_grunnerN4", "A5_bilbruk_grunnerN5", "A5_bilbruk_grunnerN6",  
                                      "A5_bilbruk_grunnerN9", "A5_bilbruk_grunnerN10", "A5_bilbruk_grunnerN11", "A5_bilbruk_grunnerN12", "A5_bilbruk_grunnerN13", "A5_bilbruk_grunnerN14")]

# removed: A7_koll_arsakN7, A5_bilbruk_grunnerN7, A5_bilbruk_grunnerN8, "A2a_arsaker_1", "A2a_arsaker_2", "A2a_arsaker_3", "A2a_arsaker_5", "A2a_arsaker_6", "A2a_arsaker_7"
# "A2a_arsaker_4", "A2a_arsaker_8", "A2a_arsaker_9", "A2a_arsaker_10", "A2a_arsaker_11"


# Transport-Lifestyle variables
transport_data <- df_06[, c("q15_BILEIE_1", "q15_BILEIE_2", "q15_BILEIE_3", 
                            "q15_BILEIE_4", "q15_BILEIE_5", "q15_BILEIE_7",
                            "A4_transpmidler_somN1", "A4_transpmidler_somN2", "A4_transpmidler_somN3",
                            "A4_transpmidler_somN4", "A4_transpmidler_somN5", "A4_transpmidler_somN6",
                            "A7_koll_opplN1", "A7_koll_opplN2", "A7_koll_opplN3", "A7_koll_opplN4",
                            "A7_koll_opplN5", "A11_holdn_miljoN1", "A11_holdn_miljoN2", "A11_holdn_miljoN3",
                            "A9_holdn_polN1", "A9_holdn_polN2", "A9_holdn_polN3")]


