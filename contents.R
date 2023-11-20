#    ___  __    ___  __      ___  _   _____  _    
#   / __\/ _\  /   \/ _\    /   \/_\ /__   \/_\   
#  / /   \ \  / /\ /\ \    / /\ //_\\  / /\//_\\  
# / /___ _\ \/ /_// _\ \  / /_//  _  \/ / /  _  \ 
# \____/ \__/___,'  \__/ /___,'\_/ \_/\/  \_/ \_/ 
#    ____         _      __   _____  _____      
#   /___ \/\ /\  /_\    / /   \_   \/__   \/\_/\
#  //  / / / \ \//_\\  / /     / /\/  / /\/\_ _/
# / \_/ /\ \_/ /  _  \/ /___/\/ /_   / /    / \ 
# \___,_\ \___/\_/ \_/\____/\____/   \/     \_/   

library("tidyverse")
library("lubridate")
library("dbplyr")
library("odbc")
library("DBI")

theme_set(theme_bw())


# a. SETUP ----------------------------------------------------------------

# CODES FOR 3 DACHA ICBs:
source("icbs_v2.r")

# ULTIMATELY NOT USED BUT MAY BE USEFUL AT SOME STAGE:
# source("providers.r")

# source("1178_db_connections.r")
# CONNECTIONS SCRIPT NOT INCLUDED FOR REASONS OF SECURITY 
# BUT ONLY CREATES A DB CONNECTION OF THE FORM:
# con_community <- dbConnect(
#   odbc(),
#   Driver = "ODBC Driver 17 for SQL Server",
#   Server = "XXX",
#   Database = "XXX",
#   Trusted_Connection = "yes"
# )


# b. ANALYSIS -------------------------------------------------------------





