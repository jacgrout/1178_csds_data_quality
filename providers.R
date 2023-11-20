# FROM CSDS SUBMISSION TRACKER (MASTER PROVIDER LIST) 

curl::curl_download(
  url = "https://digital.nhs.uk/binaries/content/assets/website-assets/data-and-information/data-sets/community-services/submission-tracker/csds-submission-tracker-jan-23-refresh.xlsx",
  destfile = "csds_submission_tracker.xlsx"
)

providers_dacha <- 
  readxl::read_excel(
    "csds_submission_tracker.xlsx", 
    sheet = "Monthly submissions",
    skip = 14
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
