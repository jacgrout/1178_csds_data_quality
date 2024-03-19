
tb_201_contact <- tbl(con_community, in_schema("csds", "PublishCYP201CareContact"))  

tb_101_referral <- tbl(con_community, in_schema("csds", "PublishCYP101Referral"))

tb_102_service <- tbl(con_community, in_schema("csds", "PublishCYP102ServiceTypeReferredTo"))

# referrals -----------------------------------------------------------------
raw_referral <- tb_101_referral |>
  filter(Person_ID=="0HC0162QETFFCI8") |>
  select(RecordNumber, Person_ID, Discharge_Date, AuditId,ServiceRequestID, LocalPatientID, ReferralRequest_ReceivedDate,
         Effective_From, UniqueSubmissionID) |>
  collect()

# services -----------------------------------------------------------------
raw_services <- tb_102_service  |>
  filter(ServiceRequestID=="33723994" | ServiceRequestID=="33724178") |>
  select(ServiceRequestID, AuditId, TeamType, TeamID_Local, Referral_ClosureDate, Referral_RejectionDate, Referral_ClosureReason,
         Referral_RejectionReason, Effective_From, RecordNumber, Person_ID) |>
  collect()

raw_services <- tb_102_service  |>
 # filter(OrgID_Provider=="RMP") |>
  select(ServiceRequestID, AuditId, TeamType, TeamID_Local, Referral_ClosureDate, Referral_RejectionDate, Referral_ClosureReason,
         Referral_RejectionReason, Effective_From, RecordNumber, Person_ID) |>
  collect()

temp_table_all <- raw_services |>
  group_by(TeamType) |>
  summarise(count_services = n()) 



# a. contacts ---------------------------------------------------------------

raw_contact <- tb_201_contact %>% 
  filter(Der_Financial_Year == "2023/24") %>%
 # filter(AgeYr_Contact_Date > 64 & AgeYr_Contact_Date < 110) %>% # 
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

#raw_contact2 <- raw_contact |> filter(Person_ID=="0HC0162QETFFCI8")


# BECAUSE SOME PROVIDERS SUBMIT SEEMINGLY IDENTICAL RECORDS 4 TIMES:
prep_contacts <- raw_contact %>% 
  mutate(month = month(Contact_Date, label = TRUE)) %>% 
  tidytable::mutate(
    n = tidytable::row_number(),
    .by = c(month, OrgID_Provider, CareContactID)) %>%
  filter(n == 1) %>% 
  rename(provider = OrgID_Provider)

temp <- prep_contacts |> filter(CareContactID=="EMC0005097728")

temp <- raw_contact |> filter(CareContactID="EMC0005097728")

raw_referral2 <- raw_referral |> filter(RecordNumber=="6513450000040163"|
                                         RecordNumber=="6757316000040172")

raw_referral3 <- raw_referral |> filter(RecordNumber=="6513027000040290")

# TO SAVE MEMORY:
rm(raw_contact)
gc()

# CREATE ICB REGISTERED FIELD BASED ON REGISTERED COMMISSIONER :
prep_contacts <- prep_contacts %>% 
  mutate(OrgIDICBReg = case_when(
    OrgID_Commissioner %in% c("15N") ~ "QJK", # DEVON
    OrgID_Commissioner %in% c("00Q", "00R", "01A",
                               "01K",
                               "00X",
                               "02M",
                               "01E",
                               "02G") ~ "QE1", # LANC SOUTH CUMB
    OrgID_Commissioner %in% c("18C", "07H",
                              "06K",
                              "06N" ) ~ "QM7",
    TRUE ~ NA_character_
  ))

prep_contacts_nhp <-prep_contacts %>% filter(OrgIDICBReg %in% c("QJK","QE1","QM7"))

# b. person details (MPI) ---------------------------------------------------------------

tb_001_mpi <- tbl(con_community, in_schema("csds", "PublishCYP001MPI"))  

# USING BRUTE FORCE APPROACH WHICH IS ONLY POSSIBLE GIVEN SHORT PERIOD OF INTEREST.

raw_person <- tb_001_mpi %>%
  # colnames()
  filter(Der_Financial_Year == "2023/24") %>%
  # TO NARROW THE RESULTS:
  #filter(AgeYr_RP_StartDate > 63 | is.na(AgeYr_RP_StartDate)) %>% # 
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

# DISTINCT PERSON IDs IN CONTACTS
pid <- prep_contacts_nhp %>% distinct(Person_ID) %>% filter(!is.na(Person_ID))


# # SELECT PERSON RECORDS WITHIN CATCHMENT THAT MATCH PERSON IDs IN CONTACTS
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

contacts <- prep_contacts %>%  
  left_join(
    prep_person, 
    by = c("Person_ID", "RecordNumber")
  )




