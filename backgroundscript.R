library(tidyverse)


# b <- read_rds("240322_contacts_full.rds")
# 
# b_devon <- b|>
#   filter(derived_icb_reg=="QJK")
# 
# b_herts <- b|>
#   filter(derived_icb_reg=="QM7")
# 
# b_lancs <- b|>
#   filter(derived_icb_reg=="QE1")


# devon <- b_devon|>
#   group_by(OrgID_Provider,OrgIDICBRes,SourceOfReferral,TeamType,PrimaryReferralReason)|>
#   count(CareContactID)|>
#   group_by(OrgID_Provider,OrgIDICBRes,SourceOfReferral,TeamType,PrimaryReferralReason)|>
#   summarise(contactnum=sum(n))
# 
# herts <- b_herts|>
#   group_by(OrgID_Provider,OrgIDICBRes,SourceOfReferral,TeamType,PrimaryReferralReason)|>
#   count(CareContactID)|>
#   group_by(OrgID_Provider,OrgIDICBRes,SourceOfReferral,TeamType,PrimaryReferralReason)|>
#   summarise(contactnum=sum(n))
# 
# lancs <- b_lancs|>
#   group_by(OrgID_Provider,OrgIDICBRes,SourceOfReferral,TeamType,PrimaryReferralReason)|>
#   count(CareContactID)|>
#   group_by(OrgID_Provider,OrgIDICBRes,SourceOfReferral,TeamType,PrimaryReferralReason)|>
#   summarise(contactnum=sum(n))

#contacts <- read_rds("240319_contacts_commissioner_based.rds")
#providers_major <- read_rds("providers_major.rds")

# c. plot provider timeseries --------------------------------------------------

preplot_providers_major <- contacts %>%
  filter(OrgID_Provider %in% providers_major) %>%
  count(derived_icb_reg_name, OrgID_Provider, month) %>%
  # TO REMOVE ODD CROSS-COUNTRY VISITS:
  # filter(n >49) %>%
  filter(n >199) %>%
  # print(n=200) %>%
  identity()


plot_provider_timeseries <-preplot_providers_major %>%
  ggplot()+
  geom_line(aes(month, n, colour = OrgID_Provider, group = OrgID_Provider))+
  geom_point(aes(month, n, colour = OrgID_Provider, group = OrgID_Provider), size = 2, alpha = .2 )+
  theme(
    legend.position = "none",
    axis.title = element_blank()
  )+
  geom_blank(aes(y=0))+
  geom_text(
    data = preplot_providers_major %>%
      filter(month == "Aug"),
    aes(month, n, label = OrgID_Provider), colour = "grey50", size = 3, nudge_y = 500, nudge_x = -.5
  )+
  scale_y_continuous(label = scales::comma)+
  facet_wrap(vars(derived_icb_reg_name), scales = "free_y")
