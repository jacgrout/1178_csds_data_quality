
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

# lkp_icb %>%
#   view("icb")
# lkp_icb %>% count(lad22cd)


lkp_ccg_icb <- lkp_icb %>% 
  distinct(
    icb22nm,
    sicbl22nm,
    sicbl22cdh
  )

vec_ccg <- lkp_icb %>% count(sicbl22cdh, sort = T) %>% pull(sicbl22cdh)

# HOW WERE THESE FOUND :
# (TOP CCG CODES RECORDED BY PROVIDERS THAT WERE NOT IN LIST ABOVE )
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
  # "05L", # sandw
  # "508", # leics county c
  # "509", # leics city c
  # "05N", # shrops
  # "05X", # telf&wrekin
  # "D2P", # probs D2P2L black country  
  # "05A", # cov and rugby ccg
  # "5M2", # SHROP PCT                      
  # "05C", # dudley                    
  # "05Y", # wals            
  # "05R", # south warks                   
  # "06A", # wolves           
  # "510", # rutland cc                
  # "05H", # warks north                  
  # "5MK" # tel&wrekin pct                   
)

# THERE MAY BE OTHER ANOMALIES BUT THIS COVERS VAST MAJORITY

vec_ccg <- c(vec_ccg, legacy_codes)

# UPDATED ABOVE TO REFLECT 2022 CHANGES

# con_sandbox <- dbConnect(odbc(),
#                          Driver = "SQL Server",
#                          Server = "PRODNHSESQL101",
#                          Database = "NHSE_Sandbox_StrategyUnit",
#                          Trusted_Connection = "True")
# 
# tb_ics_lkp <- tbl(con_sandbox, in_schema("dbo", "LSOA(2011)_to_CCG_to_STP_(April_2021)"))
# 
# lkp_ics <- tb_ics_lkp %>% collect
# # 
# # lkp_ics <-
#   tb_ics_lkp %>%
#   mutate(suffix = as.numeric(str_sub(STP21CD, -2, -1))) %>%
#   filter(between(suffix, 10, 20)) %>%
#   select(LSOA11CD, CCG21CDH, CCG21NM, STP21CD, STP21NM, suffix) %>% 
#     count(STP21NM,
#                             STP21CD,
#                             CCG21NM,
#                             CCG21CDH,
#                             sort = T) %>% 
#   view("lkpold")
# 
# lkp_ics %>% count(STP21NM,
#                   # STP21CD, 
#                   CCG21NM,
#                   # CCG21CDH,
#                   sort = T)
# 
# vec_ccg <- lkp_ics %>% count(CCG21CDH, sort = T) %>% pull(CCG21CDH)
# 
# lkp_ics <- lkp_ics %>% 
#   janitor::clean_names() 
