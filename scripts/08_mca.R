




# Define supplementary variables
sup_vars <- df_mca[, variable_groups_mca$transport_behavior]

# Extract data subsets by group
socio_demo
social_data              <- df_mca[, variable_groups_mca$socio_demo]
lifestyle_data           <- df_mca[, variable_groups_mca$lifestyle]
transport_attitude_data  <- df_mca[, variable_groups_mca$transport_attitudes]
political_data           <- df_mca[, variable_groups_mca$political]
transport_behavior_data  <- df_mca[, variable_groups_mca$transport_behavior]

# Base MCAs
mca_social <- soc.mca(social_data, passive = "NS")
mca_lifestyle <- soc.mca(lifestyle_data, passive = "NS")
mca_transport_attitudes <- soc.mca(transport_attitude_data, passive = "NS")
mca_political <- soc.mca(political_data, passive = "NS")
mca_transport_behavior <- soc.mca(transport_behavior_data, passive = "NS")

# MCAs with supplementary variables (transport_behavior as sup)
mca_social_sup <- soc.mca(social_data, sup = sup_vars, passive = "NS")
mca_lifestyle_sup <- soc.mca(lifestyle_data, sup = sup_vars, passive = "NS")
mca_transport_attitudes_sup <- soc.mca(transport_attitude_data, sup = sup_vars, passive = "NS")
mca_political_sup <- soc.mca(political_data, sup = sup_vars, passive = "NS")






mca_social


mca_lifestyle
mca_transport_attitudes

mca_political_opinions <- soc.mca(political_opinions_data, passive = passive_vars)
mca_political_opinions

mca_transport_behavior <- soc.mca(transport_behavior_data, passive = passive_vars)
mca_transport_behavior

mca_transport_attitude_data <- soc.mca(transport_attitude_data, passive = passive_vars)
mca_transport_attitude_data


# Perform MCA with supplementary variables
mca_social_sup <- soc.mca(social_data, sup = transport_behavior_data_test, passive = passive_vars)
mca_social_sup

mca_lifestyle_sup <- soc.mca(lifestyle_data, sup = transport_behavior_data_test, passive = passive_vars)
mca_lifestyle_sup

mca_political_opinions_sup <- soc.mca(political_opinions_data, sup = transport_behavior_data_test, passive = passive_vars)
mca_political_opinions_sup

mca_transport_attitude_sup <- soc.mca(transport_attitude_data, sup = transport_behavior_data_test, passive = passive_vars)
mca_transport_attitude_sup
