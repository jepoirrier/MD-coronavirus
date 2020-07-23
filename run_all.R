# run_all() just source all other code
# TODO move it to src too
# but for the moment, it starts in ..

# ORIGINAL code (manual data)
source("MD-coronavirus-cases.R")
source("MD-coronavirus-counties.R")
#source("MD-coronavirus-races.R") # now replaced below, can be deleted when sure API stable
source("MD-coronavirus-Stage1.R")

# NEW VERSION of code (API data)
# Note: pay attention to last data in data sets. Sometimes, re-downloading give the latest data
setwd("src/")
source("deaths-confirmed.R")
source("age-analysis.R")
source("gender-analysis.R") # sometimes needs to be re-run a 2nd time to get today's data
source("races-analysis.R")
source("zipcode-analysis.R")
source("congregate-facility-settings.R")
