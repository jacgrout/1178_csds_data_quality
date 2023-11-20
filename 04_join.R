
contacts_major <- contacts %>% 
  filter(OrgID_Provider %in% providers_major)

rm(contacts)
gc()

# 1. join gp --------------------------------------------------------------

prep_gp <- read_rds("join_gp.rds")

vctrs::vec_size(a)
prep_gp %>% count(RecordNumber, Person_ID, sort = T )

# 21,644,891
prep_gp <- prep_gp %>% 
  tidytable::mutate(
    n = tidytable::row_number(),
    .by = c(RecordNumber, Person_ID)) %>%
  filter(n == 1) 
# 21,497,851


# 3,858,523
a <- contacts_major %>% 
  left_join(prep_gp, by = c("Person_ID", "RecordNumber"))

rm(prep_gp)
gc()
# rm(a)


# 2. accommodation --------------------------------------------------------


prep_acc <- read_rds("join_acc.rds")

prep_acc %>% count(RecordNumber, Person_ID, sort = T)
prep_acc %>% count(is.na(AccommStatus))


prep_acc <- prep_acc %>% 
  tidytable::mutate(
    n = tidytable::row_number(),
    .by = c(RecordNumber, Person_ID)) %>%
  filter(n == 1) 
# 5,152,407


# 3,858,523
b <- a %>% 
  left_join(prep_acc, by = c("Person_ID", "RecordNumber"))

rm(prep_acc)
gc()

# 3. ----------------------------------------------------------------------

prep_soc <- read_rds("join_soc.rds")
# 1,866,840
prep_soc %>% count(is.na(SNOMED_ID))
# prep_soc %>% count(RecordNumber, Person_ID, sort = T)
# 1,489,404

prep_soc <- prep_soc %>% 
  tidytable::mutate(
    n = tidytable::row_number(),
    .by = c(RecordNumber, Person_ID)) %>%
  filter(n == 1) 

# b %>% count(is.na(AccommStatus))

# 3,858,523
a <- b %>% 
  left_join(prep_soc, by = c("Person_ID", "RecordNumber"))

rm(prep_soc)
gc()
a %>% colnames()
a %>% count(is.na(SNOMED_ID))

# 4. ----------------------------------------------------------------------

prep_ref  <- read_rds("join_ref.rds")


# b %>% count(is.na(AccommStatus))

# 3,858,523
b <- a %>% 
  left_join(prep_ref, by = c("Person_ID", "RecordNumber", "ServiceRequestID"))

rm(prep_ref)
gc()
a %>% colnames()
b %>% count(is.na(Referring_StaffGroup))

# 5. ----------------------------------------------------------------------
# TODO WE MANY NOT GET EXACT VALUES BY IGNORING TeamID_Local
# BUT AS NONE MISSING IN THE REFERENCE TABLES - WE SHOULD GET CORRECT MISSING %

prep_ref_to  <- read_rds("join_ref_to.rds")

prep_ref_to %>% count(is.na(TeamType))

# IT'S A MATTER OF 40 DUPLICATE RECORDS WHEN JOINING - SO PRETTY SMALL. 
prep_ref_to <- prep_ref_to %>% 
  tidytable::mutate(
    recordn = tidytable::row_number(),
    # .by = c(TeamID_Local, ServiceRequestID, RecordNumber, Person_ID)) %>%
  .by = c(ServiceRequestID, RecordNumber, Person_ID)) %>%
  filter(recordn == 1) %>% 
  select(-recordn)

# 3,858,523
a <- b %>% 
  left_join(prep_ref_to, by = c("Person_ID", "RecordNumber", "ServiceRequestID"))

a %>% vctrs::vec_size()
a %>% colnames()
a %>% count(is.na(TeamType))
rm(prep_ref_to)
gc()

# 6. ----------------------------------------------------------------------

prep_disability  <- read_rds("join_disability.rds")

prep_disability %>% count(is.na(DisabilityImpactPerception))

# 3,858,523
b <- a %>% 
  left_join(prep_disability, by = c("Person_ID", "RecordNumber"))

b %>% vctrs::vec_size()
b %>% colnames()
b %>% count(is.na(DisabilityImpactPerception))
rm(prep_disability)
gc()

# 7. ----------------------------------------------------------------------

prep_diag01 <- read_rds("join_diag01.rds")

# 3,858,523
a <- b %>% 
  left_join(prep_diag01, by = c("Person_ID", "RecordNumber", "ServiceRequestID"))

a %>% vctrs::vec_size()
b %>% colnames()
a %>% count(is.na(scheme_diag04))
rm(prep_disability)
gc()

a %>% saveRDS("join_one_to_go.rds")

a <- a %>% 
  rename(
    PrimDiag_sch04= scheme_diag04, 
    PrimDiag_sch05= scheme_diag05, 
    PrimDiag_sch06= scheme_diag06 
  )

# 8. ----------------------------------------------------------------------

prep_diag02 <- read_rds("join_diag02.rds")

# 3,858,523
b <- a %>% 
  left_join(prep_diag02, by = c("Person_ID", "RecordNumber", "ServiceRequestID"))

b %>% vctrs::vec_size()
b %>% colnames()
b %>% count(is.na(scheme_diag06))
rm(prep_disability)
gc()

b %>% saveRDS("231117_contacts_full.rds")
b <- read_rds("231117_contacts_full.rds")

# complete ----------------------------------------------------------------

b <- b %>% 
  rename(
    SecDiag_sch04 = scheme_diag04, 
    SecDiag_sch05 = scheme_diag05, 
    SecDiag_sch06 = scheme_diag06 
  )

b %>% 
  head(100) %>% 
  view("full")


b <- b %>%
  select(
    -c(
      # Person_ID, RecordNumber, ServiceRequestID, CareContactID,
      #  Effective_From, TeamID_Local,
       # starts_with("derived"),
       n.x, n.y, n))
  
# b %>% 
#   count(derived_icb_reg_name, is.na(AttendanceStatus)) %>%
#   group_by(derived_icb_reg_name) %>% 
#   mutate(p = n/sum(n))
