
# curl::curl_download(
#   url = "https://opendata.arcgis.com/api/v3/datasets/423c8069710c4d488d5ff99475688101_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1",
#   destfile = "lsoa_sicbl_icb.csv"
# )

lkp_icb <- read_csv("lsoa_sicbl_icb.csv") %>% 
  janitor::clean_names()

lkp_icb <- lkp_icb %>% 
  select(
    lsoa11cd, 
    icb22cd, icb22cdh, icb22nm,
    sicbl22cdh, sicbl22nm,
    lad22cd, lad22nm) %>%
  mutate(suffix = as.numeric(str_sub(icb22cd, -2, -1))) %>%
  # count(icb22nm, suffix) %>% 
  # print(n=50)
  filter(suffix %in% c(50, 52, 60)) 


lkp_ccg_icb <- lkp_icb %>% 
  distinct(
    icb22nm,
    sicbl22nm,
    sicbl22cdh
  )

vec_ccg <- lkp_icb %>% count(sicbl22cdh, sort = T) %>% pull(sicbl22cdh)

# WORTH INCLUDING FORMER CCG CODES:
# (POSSIBLY LESS OF AN ISSUE IN 2023 (?) BUT IN PREVIOUS YEARS IT'S NECESSARY)
legacy_codes <- c(
  "04E", # notts                  
  "04H", # notts                  
  "04L", # notts                  
  "04M", # notts                  
  "04N", # notts                  
  "04K", # notts          
  "09L", # surrey          
  "09N", # surrey          
  "09Y", # surrey          
  "99H" # surrey          
)


vec_ccg <- c(vec_ccg, legacy_codes)

