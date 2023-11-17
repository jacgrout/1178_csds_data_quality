# FROM CSDS SUBMISSION TRACKER (MASTER PROVIDER LIST) 

curl::curl_download(
  url = "https://digital.nhs.uk/binaries/content/assets/website-assets/data-and-information/data-sets/community-services/submission-tracker/csds-submission-tracker-jan-23-refresh.xlsx",
  destfile = "csds_submission_tracker.xlsx"
)

providers_dacha <- 
  readxl::read_excel(
    "csds_submission_tracker.xlsx", 
    # sheet = "Master Provider List", # "Monthly submissions"
    sheet = "Monthly submissions",
    skip = 14
    # skip = 16
  ) %>% 
  janitor::clean_names() 

providers_dacha <- providers_dacha %>% 
  filter(
    str_detect(icb_name, "NORTH CUMB")| str_detect(icb_name, "SURREY")|
    str_detect(icb_name, "NOTT")
      ) %>% 
  count(icb_name, provider, orgcode) %>% 
  # print(n=40)
  select(icb_name, provider_code = orgcode, provider_name = provider) %>% 
  # mutate(provider_name = str_to_title(provider_name))
  identity()

vec_providers <- providers_dacha %>% pull(provider_code)

# -------------------------------------------------------------------------

# 
# providers_dacha %>% 
#   select(starts_with("4")) %>% 
#   colnames() %>%  
#   as.numeric() %>%
#   janitor::excel_numeric_to_date()
# 
# providers_dacha %>% 
#   rename_with(~ janitor::excel_numeric_to_date(as.numeric(.x)) %>% 
#                 tsibble::yearmonth() %>% 
#                 as.character(), 
#               starts_with("4"))
#   # select((num_range("x", 1e3:1e6)))
#   rename_at(vars(num_range("x", 1e3:1e6)),
#                  ~ janitor::excel_numeric_to_date(as.numeric(.x))
#                    # tsibble::yearmonth() %>%
#                    as.character()
#                    )
# 
# 
# janitor::excel_numeric_to_date(44593) %>% 
#   tsibble::yearmonth() %>% 
#   as.character()
# enframe() %>%
#   # deframe() %>% 
#   pull(value)
# 
# # providers_dacha %>% view()
# 
# providers_dacha <- providers_dacha %>% 
#   # colnames
#   filter(region == "MIDLANDS COMMISSIONING REGION") %>% 
#   filter(current_status %in% c("Submitter", "Registered to submit")) %>% 
#   # view("provider_mids")
#   select(provider_code = ods_code, provider_name = org_name ) %>% 
#   mutate(provider_name = str_to_title(provider_name))
# 
# vec_providers <- provider_mids %>% pull(provider_code)



# LEGACY ------------------------------------------------------------------

# FROM CSDS SUBMISSION TRACKER (MASTER PROVIDER LIST) 

# providers_midlands <- tribble(
#   
#   ~provider_code,	~provider_name,
#   "503"	 , "LINCOLNSHIRE COUNTY COUNCIL",
#   "506AA", "PUBLIC HEALTH DERBYSHIRE COUNTY COUNCIL",
#   "8A260", "ST BARNABAS LINCOLNSHIRE HOSPICE (IPU)",
#   "8AW20", "ASHGATE HOSPICECARE",
#   "8HP67", "COVENTRY MYTON HOSPICE",
#   "ADN"	 , "PRIMARY INTEGRATED COMMUNITY SERVICES LTD",
#   "ATR"	 , "EVERYONE HEALTH",
#   "DQ502", "HEALTHY LIFESTYLES COVENTRY",
#   "DXK09", "LETS GET HEALTHY DUDLEY",
#   "NNJ"	 , "DHU HEALTH CARE C.I.C",
#   "NR3"	 , "NOTTINGHAM CITYCARE PARTNERSHIP",
#   "R1A"	 , "HEREFORDSHIRE AND WORCESTERSHIRE HEALTH AND CARE NHS TRUST",
#   "R1D"	 , "SHROPSHIRE COMMUNITY HEALTH NHS TRUST",
#   "RBK"	 , "WALSALL HEALTHCARE NHS TRUST",
#   "RHA"	 , "NOTTINGHAMSHIRE HEALTHCARE NHS FOUNDATION TRUST",
#   "RJC"	 , "SOUTH WARWICKSHIRE UNIVERSITY NHS FOUNDATION TRUST",
#   "RL4"	 , "THE ROYAL WOLVERHAMPTON NHS TRUST",
#   "RLQ"	 , "WYE VALLEY NHS TRUST",
#   "RNA"	 , "THE DUDLEY GROUP NHS FOUNDATION TRUST",
#   "RP1"	 , "NORTHAMPTONSHIRE HEALTHCARE NHS FOUNDATION TRUST",
#   "RRE"	 , "MIDLANDS PARTNERSHIP NHS FOUNDATION TRUST",
#   "RRK"	 , "UNIVERSITY HOSPITALS BIRMINGHAM NHS FOUNDATION TRUST",
#   "RT5"	 , "LEICESTERSHIRE PARTNERSHIP NHS TRUST",
#   "RXK"	 , "SANDWELL AND WEST BIRMINGHAM HOSPITALS NHS TRUST",
#   "RXM"	 , "DERBYSHIRE HEALTHCARE NHS FOUNDATION TRUST",
#   "RY5"	 , "LINCOLNSHIRE COMMUNITY HEALTH SERVICES NHS TRUST",
#   "RY8"	 , "DERBYSHIRE COMMUNITY HEALTH SERVICES NHS FOUNDATION TRUST",
#   "RYG"	 , "COVENTRY AND WARWICKSHIRE PARTNERSHIP NHS TRUST",
#   "RYK"	 , "DUDLEY INTEGRATED HEALTH AND CARE NHS TRUST",
#   "RYW"	 , "BIRMINGHAM COMMUNITY HEALTHCARE NHS FOUNDATION TRUST",
#   "TAJ"	 , "BLACK COUNTRY HEALTHCARE NHS FOUNDATION TRUST",
#   "Y9T7T", "SKY BLUES IN THE COMMUNITY",
#   "Z1T7T", "MILES BRAMWELL EXECUTIVE SERVICES T/A SLIMMING WORLD",
#   # PREVIOUS SUBMITTERS (ACCORDING TO SUBMISSION TRACKER NOV 22):
#   "404"  , "WARWICKSHIRE COUNTY COUNCIL",
#   "415"  , "HEREFORDSHIRE COUNCIL"
# )
# 
# vec_providers <- providers_midlands %>% pull(provider_code)
