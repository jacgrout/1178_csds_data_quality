# JOIN ALL TABLES TO CREATE FULL TABLE. 
# (BASED ON THE SUBSET (99.6%) OF CONTACTS THAT HAPPEN 
#  AT ONE OF THE DOMINANT PROVIDERS IDENTIFIED IN THE  
#  02_PROVIDERS.R SCRIPT.)

contacts_major <- contacts %>% 
  filter(OrgID_Provider %in% providers_major)

rm(contacts)
gc()

# 1. join gp --------------------------------------------------------------

a <- contacts_major %>% 
  left_join(prep_gp, by = c("Person_ID", "RecordNumber"))

rm(prep_gp)
gc()
# rm(a)


# 2. accommodation --------------------------------------------------------

b <- a %>% 
  left_join(prep_acc, by = c("Person_ID", "RecordNumber"))

rm(prep_acc)
gc()

# 3. ----------------------------------------------------------------------

# 3,858,523
a <- b %>% 
  left_join(prep_soc, by = c("Person_ID", "RecordNumber"))

rm(prep_soc)
gc()

# 4. ----------------------------------------------------------------------

# 3,858,523
b <- a %>% 
  left_join(prep_ref, by = c("Person_ID", "RecordNumber", "ServiceRequestID"))

rm(prep_ref)
gc()

# 5. ----------------------------------------------------------------------
# NOTE: WE MAY NOT GET 100% CORRECT VALUES BY IGNORING TeamID_Local
# BUT AS NONE MISSING IN THE REFERENCE TABLES - WE WILL GET CORRECT MISSING %


# IN FACT, IT'S ONLY A MATTER OF 40 DUPLICATE RECORDS WHEN IGNORING TEAM ID 
# - SO PRETTY SMALL. 
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

rm(prep_ref_to)
gc()

# 6. ----------------------------------------------------------------------

#prep_disability  <- read_rds("join_disability.rds")


# 3,858,523
b <- a %>% 
  left_join(prep_disability, by = c("Person_ID", "RecordNumber"))

b %>% vctrs::vec_size()
# b %>% colnames()
# b %>% count(is.na(DisabilityImpactPerception))
rm(prep_disability)
gc()

# 7. ----------------------------------------------------------------------

#prep_diag01 <- read_rds("join_diag01.rds")

# 3,858,523
a <- b %>% 
  left_join(prep_diag01, by = c("Person_ID", "RecordNumber", "ServiceRequestID"))

# a %>% vctrs::vec_size()
# b %>% colnames()
# a %>% count(is.na(scheme_diag04))
rm(prep_disability)
gc()

a <- a %>% 
  rename(
    PrimDiag_sch04 = scheme_diag04, 
    PrimDiag_sch05 = scheme_diag05, 
    PrimDiag_sch06 = scheme_diag06 
  )

# 8. ----------------------------------------------------------------------

# 3,858,523
b <- a %>% 
  left_join(prep_diag02, by = c("Person_ID", "RecordNumber", "ServiceRequestID"))

# b %>% vctrs::vec_size()
# b %>% colnames()
# b %>% count(is.na(scheme_diag06))
rm(prep_disability)
gc()

# b <- read_rds("231117_contacts_full.rds")

# complete ----------------------------------------------------------------

b <- b %>% 
  rename(
    SecDiag_sch04 = scheme_diag04, 
    SecDiag_sch05 = scheme_diag05, 
    SecDiag_sch06 = scheme_diag06 
  )
# 
# b %>% 
#   head(100) %>% 
#   view("full")


b <- b %>%
  select(
    -c(
      # Person_ID, RecordNumber, ServiceRequestID, CareContactID,
      #  Effective_From, TeamID_Local,
       # starts_with("derived"),
       n.x, n.y, n))

saveRDS(b,"240322_contacts_full.rds")
