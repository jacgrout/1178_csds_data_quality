# IDENTIFY SUBSET OF PROVIDERS AND PLOT TIMESERIES OF CONTACTS

# contacts <- read_rds("231116_contacts_commissioner_based.rds")
# contacts %>% colnames()


# a. tidy contacts --------------------------------------------------------

contacts <- contacts %>% 
  select(-OrgID_Provider, -n, -record) %>% 
  rename(
    derived_icb_reg = OrgIDICBRes.x,
    OrgIDICBRes = OrgIDICBRes.y,
    OrgID_Provider = provider
    )


contacts <- contacts %>% 
  mutate(derived_icb_reg_name = case_when(
    derived_icb_reg == "QHM" ~"North East and North Cumbria ICB",
    derived_icb_reg == "QT1" ~ "Nottingham and Nottinghamshire ICB",
    derived_icb_reg == "QXU" ~ "Surrey Heartlands ICB",
  TRUE ~ NA_character_
)) 


# b. identify subset of providers -----------------------------------------

# main providers (accounting for > 99% of contacts in each icb)
# 13 overall .982
# 19 overall .996
providers_major <-
  contacts %>% 
  count(derived_icb_reg_name, OrgID_Provider, sort= T) %>% 
  group_by(derived_icb_reg_name) %>%
  mutate(p = n/sum(n)) %>% 
  mutate(cs = cumsum(p)) %>% 
  ungroup %>% 
  arrange(derived_icb_reg_name) %>% 
  # filter(cs < .982 ) %>% 
  filter(cs < .996 ) %>%
  # print(n=200)
  pull(OrgID_Provider)
  
# providers_major %>%  saveRDS("providers_major.rds")

# c. plot provider timeseries --------------------------------------------------

preplot_providers_major <- contacts %>% 
  filter(OrgID_Provider %in% providers_major) %>% 
  count(derived_icb_reg_name, OrgID_Provider, month) %>%  
  # TO REMOVE ODD CROSS-COUNTRY VISITS:
  # filter(n >49) %>%
  filter(n >199) %>%
  # print(n=200) %>%
  identity()


preplot_providers_major %>% 
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
