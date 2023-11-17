
tb_201_contact <- tbl(con_community, in_schema("csds", "PublishCYP201CareContact"))  

raw_contact <- tb_201_contact %>% 
  filter(
    (Der_Financial_Year == "2023/24" & Der_Financial_Month %in% local(str_c("0", 1:5))) #%>% 
    |(Der_Financial_Year == "2022/23" & Der_Financial_Month %in% c("12"))) %>%
  filter(AgeYr_Contact_Date > 64 & AgeYr_Contact_Date < 110) %>% # 
  # EVALATE PROV/CCG VECTORS LOCALLY BEFORE CONVERTING TO SQL:
  filter(
    OrgID_Commissioner %in% local(vec_ccg)
  ) %>%
  select(
    # Effective_From,
    RecordNumber,
    CareContactID,
    ServiceRequestID,
    Person_ID,
    Contact_Date,
    # Der_Financial_Year,
    # Der_Financial_Month,
    AgeYr_Contact_Date,
    # AttendOrNot, # ALL NA AS MOVE INTO 2023, REPLACED BY:
    AttendanceStatus,
    # Consultation_MediumUsed, # ALL NA AS MOVE INTO 2023, REPLACED BY:
    ConsMechanism,
    Consultation_Type,
    Activity_LocationType,
    Days_Referral_to_CareContact,
    OrgID_Commissioner,
    OrgID_Provider
  ) %>% 
  collect()

gc()

# BECAUSE SOME PROVIDERS SUBMIT IDENTICAL RECORDS 4 TIMES:
prep_contacts <- raw_contact %>% 
  mutate(month = month(Contact_Date, label = T)) %>% 
  tidytable::mutate(
    n = tidytable::row_number(),
    .by = c(month, OrgID_Provider, CareContactID)) %>%
  filter(n == 1) %>% 
  rename(provider = OrgID_Provider)

# TO SAVE MEMORY:
rm(raw_contact)
gc()

# TODO check commissioner codes from contacts are represented.
prep_contacts %>% count(OrgID_Commissioner, sort = T)
contacts %>% count(OrgID_Commissioner, sort = T)

prep_contacts <- prep_contacts %>% 
  mutate(OrgIDICBReg = case_when(
    OrgID_Commissioner %in% c("16C", "84H", "13T", "01H", "00L", "00P", "99C", "00N") ~ "QHM", # CUMB
    OrgID_Commissioner %in% c("52R", "02Q", "04E",
                               "04H",
                               "04L",
                               "04M",
                               "04N",
                               "04K") ~ "QT1", # NOTTS
    OrgID_Commissioner %in% c("92A", "09L",
                              "09N",
                              "09Y",
                              "99H" ) ~ "QXU",
    TRUE ~ NA_character_
  ))

# OrgIDICBRes == "QHM" ~   "North East and North Cumbria ICB",
# OrgIDICBRes == "QT1" ~   "Nottingham and Nottinghamshire ICB",
# OrgIDICBRes == "QXU" ~"Surrey Heartlands ICB",
# 

prep_contacts %>% 
  count(is.na(OrgIDICBRes))

method_commies <- prep_contacts %>% 
  count(month, OrgIDICBRes)

prep_contacts %>% saveRDS("231116prep_contacts.rds")


# b. Person ---------------------------------------------------------------
tb_001_mpi <- tbl(con_community, in_schema("csds", "PublishCYP001MPI"))  

# ALL PERSON RECORDS WITHIN CATCHMENT: 
raw_person <- tb_001_mpi %>%
  # colnames()
  filter(
    (Der_Financial_Year == "2023/24" & Der_Financial_Month %in% local(str_c("0", 1:5))) #%>% 
    |(Der_Financial_Year == "2022/23" & Der_Financial_Month %in% c("12"))) %>%
  # TO NARROW THE RESULTS:
  filter(AgeYr_RP_StartDate > 63 | is.na(AgeYr_RP_StartDate)) %>% # 
  # EVALATE PROV/CCG VECTORS LOCALLY BEFORE CONVERTING TO SQL:
  # filter(
  #   OrgID_CCG_Residence %in% local(vec_ccg)
  #   |
  #     is.na(OrgID_CCG_Residence)|
  #     OrgID_Provider %in% local(vec_providers)
  # ) %>%
  # count()
  select(
    Person_ID, 
    Gender,
    AgeYr_RP_StartDate,
    EthnicCategory,
    LanguagePreferred,
    OrgID_CCG_Residence,
    OrgID_Provider,
    OrgIDSubICBLocResidence,
    OrgIDICBRes,
    Der_Postcode_yr2011_LSOA,
    LSOA,
    # pseudo_nhs_number_ncdr,
    # Der_Financial_Year,
    RecordNumber,
    Effective_From,
    # DEATH VARIABLES:
    RiskOfUnexpectedDeath_Indicator,
    DateOfDeath,
    AgeYr_Death,
    DeathLocationPreferred_Type,
    DiscussedPreferredDeathLocation_Indicator,
    DeathLocationActual_Type,
    NotAtPreferredLocation_Reason
  ) %>% 
  collect

gc()

pid <- prep_contacts %>% distinct(Person_ID) %>% filter(!is.na(Person_ID))


# # SELECT PERSON RECORDS WITHIN CATCHMENT THAT MATCH PERSON IDS IN CONTACTS
prep_person <- raw_person %>%
  right_join(pid, by = "Person_ID")

rm(raw_person)
gc()

prep_person <- prep_person %>% 
   tidytable::mutate(
    record = tidytable::row_number(),
    .by = c(Person_ID, RecordNumber)) %>%
  filter(record == 1)


# c. join -----------------------------------------------------------------

prep <- prep_contacts %>%  
  left_join(
    prep_person, 
    by = c("Person_ID", "RecordNumber")
  )

prep %>% saveRDS("231116_contacts_commissioner_based.rds")
