# PREPARE 8 TABLES FOR JOINING TO CONTACTS-MPI.

# FOLLOWING BRUTE FORCE APPROACH AGAIN. 
# APART FROM THE (MAJOR) CONTACTS-MPI JOIN, OTHER JOINS 
# ARE BASED ON THE SUBSET (99.6%) OF CONTACTS THAT HAPPEN 
# AT ONE OF THE DOMINANT PROVIDERS IDENTIFIED IN THE  
# 02_PROVIDERS.R SCRIPT. BASED ON PRACTICALITIES AND TIME / MEMORY 
# CONTRAINTS. THERE ARE PROBABLY MORE EFFICIENT WAYS TO DO THIS IN SQL OR R.

# providers_major <- read_rds("providers_major.rds")

# 1. gp -------------------------------------------------------------------

tb_002_gp <- tbl(con_community, in_schema("csds", "PublishCYP002GP"))  

raw_gp <- tb_002_gp %>% 
  # colnames()
  filter(Der_Financial_Year == "2023/24") %>% 
  filter(OrgID_Provider %in% local(providers_major)) %>% 
  select(
    OrgID_GP,
    RecordNumber,
    Person_ID
  ) %>% 
  collect

# 
# prep_gp <- raw_gp %>% 
#   count(RecordNumber, Person_ID, OrgID_GP, sort = T)

# TODO THERE ARE ODD THINGS HAPPENING HERE. 
# DON'T HAVE TIME TO FULLY INVESTIGATE BUT, IN FUTURE, IT MAY MAKE SENSE TO 
# TAKE CLOSEST GP PRACTICE.
# tb_002_gp %>% 
#   filter(Person_ID == "CTCBOQER9YEOTXY") %>% 
#   view()

# 21,644,891
prep_gp <- raw_gp %>% 
  tidytable::mutate(
    n = tidytable::row_number(),
    .by = c(RecordNumber, Person_ID)) %>%
  filter(n == 1) 
# 21,497,851

# 2.  ---------------------------------------------------------------------

tb_003_acc <- tbl(con_community, in_schema("csds", "PublishCYP003AccommType"))  

# tb_003_acc %>% colnames()

raw_acc <- tb_003_acc %>% 
  # colnames()
  filter(Der_Financial_Year == "2023/24") %>% 
  filter(OrgID_Provider %in% local(providers_major)) %>% 
  select(
    AccommStatus,
    RecordNumber,
    Person_ID
  ) %>% 
  collect
# 
# prep_acc <- raw_acc %>% 
#   distinct(RecordNumber, Person_ID, AccommStatus)

prep_acc <- prep_acc %>% 
  tidytable::mutate(
    n = tidytable::row_number(),
    .by = c(RecordNumber, Person_ID)) %>%
  filter(n == 1) 


# 3.  ---------------------------------------------------------------------

tb_006_soc <- tbl(con_community, in_schema("csds", "PublishCYP006SocPerCircumstances"))  

# tb_006_soc %>% colnames()

raw_soc <- tb_006_soc %>% 
  filter(Der_Financial_Year == "2023/24") %>% 
  filter(OrgID_Provider %in% local(providers_major)) %>% 
  select(
    SNOMED_ID,
    RecordNumber,
    Person_ID
  ) %>% 
  collect

# # prep_soc <- raw_soc %>% 
#   distinct(RecordNumber, Person_ID, SNOMED_ID)

prep_soc <- prep_soc %>% 
  tidytable::mutate(
    n = tidytable::row_number(),
    .by = c(RecordNumber, Person_ID)) %>%
  filter(n == 1) 


# 4.  ---------------------------------------------------------------------

tb_101_ref <- tbl(con_community, in_schema("csds", "PublishCYP101Referral"))  

tb_101_ref %>% colnames()

raw_ref <- tb_101_ref %>% 
  filter(Der_Financial_Year == "2023/24") %>% 
  filter(OrgID_Provider %in% local(providers_major)) %>% 
  # count
  select(
    SourceOfReferral,
    Referring_StaffGroup,
    PrimaryReferralReason,
    ServiceRequestID,
    RecordNumber,
    Person_ID
  ) %>% 
  collect

prep_ref <- raw_ref %>% 
  tidytable::mutate(
    recordn = tidytable::row_number(),
    .by = c(ServiceRequestID, RecordNumber, Person_ID)) %>%
  filter(recordn == 1) %>% 
  select(-recordn)

# prep_ref %>%  saveRDS("join_ref.rds")


# 5.  ---------------------------------------------------------------------

tb_102_ref_to <- tbl(con_community, in_schema("csds", "PublishCYP102ServiceTypeReferredTo"))  

tb_102_ref_to %>% colnames()

raw_ref_to <- tb_102_ref_to %>% 
  filter(Der_Financial_Year == "2023/24") %>% 
  filter(OrgID_Provider %in% local(providers_major)) %>% 
  # count
  select(
    TeamType,
    TeamID_Local,
    ServiceRequestID,
    RecordNumber,
    Person_ID
  ) %>% 
  collect

raw_ref_to %>% count(is.na(TeamID_Local))

prep_ref_to <- raw_ref_to %>% 
  tidytable::mutate(
    recordn = tidytable::row_number(),
    # NOTE THAT TEAM ID LOCAL DOESN'T GIVE VERY MUCH AND COULD POSSIBLY BE IGNORED.
    .by = c(TeamID_Local, ServiceRequestID, RecordNumber, Person_ID)) %>%
    # .by = c(ServiceRequestID, RecordNumber, Person_ID)) %>%
    # .by = c(TeamID_Local, RecordNumber, Person_ID)) %>%
    # .by = c(RecordNumber, Person_ID)) %>%
  filter(recordn == 1) %>% 
  select(-recordn)



# 6.  ---------------------------------------------------------------------

# TODO CHECK HEIGHT AND WEIGHT
tb_602_disability <- tbl(con_community, in_schema("csds", "PublishCYP602DisabilityType"))  

# tb_602_disability %>% colnames()

raw_disability <- tb_602_disability %>% 
  filter(Der_Financial_Year == "2023/24") %>% 
  filter(OrgID_Provider %in% local(providers_major)) %>% 
  # count
  select(
    Disability,
    DisabilityImpactPerception,
    RecordNumber,
    Person_ID
  ) %>% 
  collect

prep_disability <- raw_disability %>% 
  tidytable::mutate(
    recordn = tidytable::row_number(),
    .by = c(RecordNumber, Person_ID)) %>%
  filter(recordn == 1) %>% 
  select(-recordn)


# 7.  ---------------------------------------------------------------------

tb_607_diag01 <- tbl(con_community, in_schema("csds", "PublishCYP607PrimDiag"))  


# tb_607_diag01 %>% colnames()

raw_diag01 <- tb_607_diag01 %>% 
  filter(Der_Financial_Year == "2023/24") %>% 
  filter(OrgID_Provider %in% local(providers_major)) %>% 
  # count
  select(
    Diagnosis_Scheme,
    PrimaryDiagnosis,
    ServiceRequestID,
    RecordNumber,
    Person_ID
  ) %>% 
  collect


prep_diag01 <- raw_diag01 %>%
  # count(Diagnosis_Scheme)
  # distinct(Person_ID, RecordNumber, PrimaryDiagnosis, Diagnosis_Scheme) %>%
  tidytable::mutate(
    recordn = tidytable::row_number(),
    .by = c(ServiceRequestID, RecordNumber, Person_ID)) %>%
  filter(recordn == 1) %>% 
  select(-recordn) %>% 
  pivot_wider(
    names_from = Diagnosis_Scheme, values_from = PrimaryDiagnosis, names_prefix = "scheme_diag")
  



# 8.  ---------------------------------------------------------------------

tb_608_diag02 <- tbl(con_community, in_schema("csds", "PublishCYP608SecDiag"))  

# tb_608_diag02 %>% colnames()

raw_diag02 <- tb_608_diag02 %>% 
  filter(Der_Financial_Year == "2023/24") %>% 
  filter(OrgID_Provider %in% local(providers_major)) %>% 
  # count
  select(
    Diagnosis_Scheme,
    SecondaryDiagnosis,
    ServiceRequestID,
    RecordNumber,
    Person_ID
  ) %>% 
  collect


prep_diag02 <- raw_diag02 %>%
   tidytable::mutate(
    recordn = tidytable::row_number(),
    .by = c(ServiceRequestID, RecordNumber, Person_ID)) %>%
  filter(recordn == 1) %>% 
  select(-recordn) %>% 
  pivot_wider(
    names_from = Diagnosis_Scheme, values_from = SecondaryDiagnosis, names_prefix = "scheme_diag")


