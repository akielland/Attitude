


mca_social <- soc.mca(social_data, passive = passive_vars)
mca_social

mca_lifestyle <- soc.mca(lifestyle_data, passive = passive_vars)
mca_lifestyle

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
