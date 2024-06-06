
library(tidyverse)
library(readxl)
library(dplyr)
library(janitor)

primary_reason_referral <- read_excel("primary_reason_referral.xlsx")
primary_reason_referral<- primary_reason_referral |> mutate(PrimaryReferralReason=as.character(code))

b_devon <- readRDS("~/csds_data_quality/b_devon.rds")
b_lancs <- readRDS("~/csds_data_quality/b_lancs.rds")
b_herts <- readRDS("~/csds_data_quality/b_herts.rds")


devon <- b_devon |>
  group_by(OrgID_Provider,SourceOfReferral,TeamType,OrgID_GP,derived_icb_reg_name,CareContactID,ServiceRequestID,
           Person_ID,Contact_Date,AgeYr_Contact_Date,AttendanceStatus,ConsMechanism,Consultation_Type,Activity_LocationType,
           Days_Referral_to_CareContact,OrgID_Commissioner,month,derived_icb_reg,Gender,AgeYr_RP_StartDate,
           EthnicCategory,LSOA,OrgIDICBRes,OrgIDSubICBLocResidence)|>
  count(CareContactID)|>
  rename(contactnum=n)

herts <- b_herts |>
  group_by(OrgID_Provider,SourceOfReferral,TeamType,OrgID_GP,derived_icb_reg_name,CareContactID,ServiceRequestID,
           Person_ID,Contact_Date,AgeYr_Contact_Date,AttendanceStatus,ConsMechanism,Consultation_Type,Activity_LocationType,
           Days_Referral_to_CareContact,OrgID_Commissioner,month,derived_icb_reg,Gender,AgeYr_RP_StartDate,
           EthnicCategory,LSOA,OrgIDICBRes,OrgIDSubICBLocResidence)|>
  count(CareContactID)|>
  rename(contactnum=n)

lancs <- b_lancs |>
  group_by(OrgID_Provider,SourceOfReferral,TeamType,OrgID_GP,derived_icb_reg_name,CareContactID,ServiceRequestID,
           Person_ID,Contact_Date,AgeYr_Contact_Date,AttendanceStatus,ConsMechanism,Consultation_Type,Activity_LocationType,
           Days_Referral_to_CareContact,OrgID_Commissioner,month,derived_icb_reg,Gender,AgeYr_RP_StartDate,
           EthnicCategory,LSOA,OrgIDICBRes,OrgIDSubICBLocResidence)|>
  count(CareContactID)|>
  rename(contactnum=n)

options(scipen = 999)
devon |>
  ggplot(aes(x=SourceOfReferral, y=contactnum)) + 
  geom_bar(stat = "identity") +
  coord_flip()

devon |>
  ggplot(aes(x=TeamType, y=contactnum)) + 
  geom_bar(stat = "identity")+
  coord_flip()


herts |>
  ggplot(aes(x=SourceOfReferral, y=contactnum)) + 
  geom_bar(stat = "identity") +
  coord_flip()

herts |>
  ggplot(aes(x=TeamType, y=contactnum)) + 
  geom_bar(stat = "identity")+
  coord_flip()

lancs |>
  ggplot(aes(x=SourceOfReferral, y=contactnum)) + 
  geom_bar(stat = "identity") +
  coord_flip()

lancs |>
  ggplot(aes(x=TeamType, y=contactnum)) + 
  geom_bar(stat = "identity")+
  coord_flip()

#Do % of teamtype and source of ref maybe top 10?

d1 <- devon %>% 
  tabyl(month,show_na = TRUE) %>% 
 # adorn_title("combined") %>%
  adorn_totals(c("row")) %>%
  adorn_pct_formatting(rounding = "half up", digits = 2) %>%
#  adorn_ns() %>%
  rename(`Number of Contacts`=n,
         `Month`=month,
         `Percent`=percent) %>% 
  knitr::kable() %>% 
  kableExtra::kable_paper(bootstrap_options = "striped", full_width = F)

l1 <- lancs %>% 
  tabyl(month,show_na = TRUE)

h1 <- herts %>% 
  tabyl(month,show_na = TRUE) 

d2 <- devon %>% 
  tabyl(SourceOfReferral,show_na=TRUE) %>% 
  adorn_totals(c("row")) %>%
  adorn_pct_formatting(rounding = "half up", digits = 2) %>%
  #  adorn_ns() %>%
  order(desc(percent)) %>% 
  rename(`Number of Contacts`=n,
         `Referral Source`=SourceOfReferral,
         `Percent`=percent,
         `Percent (exc NA)`=valid_percent) %>% 
  knitr::kable() %>% 
  kableExtra::kable_styling(fixed_thead = T) %>% 
  kableExtra::kable_paper(bootstrap_options = "striped", full_width = F)

l2 <- lancs %>% 
  tabyl(SourceOfReferral,show_na = TRUE)

h2 <- herts %>% 
  tabyl(SourceOfReferral,show_na = TRUE)


get_tabyl <- function(df,columnname){
 t<- df %>% 
    tabyl(columnname,show_na = TRUE)
  return(t)
}

h3 <- get_tabyl(herts,"Gender")

# c. plot provider timeseries --------------------------------------------------


pilot_providers <- devon |>
  rbind(herts)|>
  rbind(lancs)|>
  count(derived_icb_reg_name, OrgID_Provider, month)|>

  identity()

pilot_providers |>  saveRDS("pilot_providers.rds")

pilot_providers <- readRDS("~/csds_data_quality/pilot_providers.rds")

preplot_providers_devon <- devon %>% 
#  filter(OrgID_Provider %in% providers_major) %>% 
  count(derived_icb_reg_name, OrgID_Provider, month) %>%  
  # TO REMOVE ODD CROSS-COUNTRY VISITS:
  # filter(n >49) %>%
#  filter(n >199) %>%
  # print(n=200) %>%
  identity()


p1 <- pilot_providers %>% 
  ggplot()+
  geom_line(aes(month, n, colour = OrgID_Provider, group = OrgID_Provider))+
  geom_point(aes(month, n, colour = OrgID_Provider, group = OrgID_Provider), size = 2, alpha = .2 )+
  theme(
    legend.position = "none",
    axis.title = element_blank()
  )+
  geom_blank(aes(y=0))+
  geom_text(
    data = pilot_providers %>% 
      filter(month == "Aug"),
    aes(month, n, label = OrgID_Provider), colour = "grey50", size = 3, nudge_y = 500, nudge_x = -.5
  )+
  scale_y_continuous(label = scales::comma)+
  facet_wrap(vars(derived_icb_reg_name), scales = "free_y")

p1 <- pilot_providers |>
  group_by(derived_icb_reg_name, OrgID_Provider, month)|>
  summarise(num=sum(n)) |>
  filter(num>199)|>
  ggplot()+
  geom_line(aes(month, num, colour = OrgID_Provider, group = OrgID_Provider))+
  geom_point(aes(month, num, colour = OrgID_Provider, group = OrgID_Provider), size = 2, alpha = .2 )+
  theme(
    legend.position = "none",
    axis.title = element_blank()
  )+
  geom_blank(aes(y=0))+
  scale_y_continuous(label = scales::comma)+
  facet_wrap(vars(derived_icb_reg_name), scales = "free_y")

attendance_status <-pilot_providers|>
  group_by(derived_icb_reg_name,AttendanceStatus)|>
  summarise(num=sum(n))|>
  mutate(dq=case_when(AttendanceStatus>=0 ~ 1,
                      .default = 0))|>
  group_by(derived_icb_reg_name,dq)|>
  summarise(num=sum(num)) |>
  group_by(derived_icb_reg_name)|>
  mutate(tot=sum(num))|>
  ungroup()|>
  mutate(percent=(num/tot)*100)

cons_mechanism <- pilot_providers|>
  group_by(derived_icb_reg_name,ConsMechanism)|>
  summarise(num=sum(n))|>
  mutate(dq=case_when(is.na(ConsMechanism)==TRUE ~ 0,
                      .default = 1))|>
  group_by(derived_icb_reg_name,dq)|>
  summarise(num=sum(num)) |>
  group_by(derived_icb_reg_name)|>
  mutate(tot=sum(num))|>
  ungroup()|>
  mutate(percent=(num/tot)*100)

consultation_type <- pilot_providers|>
  group_by(derived_icb_reg_name,Consultation_Type)|>
  summarise(num=sum(n))|>
  mutate(dq=case_when(Consultation_Type>=0 ~ 1,
                      .default = 0))|>
  group_by(derived_icb_reg_name,dq)|>
  summarise(num=sum(num)) |>
  group_by(derived_icb_reg_name)|>
  mutate(tot=sum(num))|>
  ungroup()|>
  mutate(percent=(num/tot)*100)


  