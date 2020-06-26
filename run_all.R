# run_all() just source all other code
# TODO move it to src too
# but for the moment, it starts in ..

# ORIGINAL code (manual data)
source("MD-coronavirus-cases.R")
source("MD-coronavirus-counties.R")
#source("MD-coronavirus-races.R") # now replaced below, can be deleted when sure API stable
source("MD-coronavirus-Stage1.R")

# NEW VERSION of code (API data)
setwd("src/")
source("zipcode-analysis.R")
source("deaths-confirmed.R")
source("races-analysis.R")
source("congregate-facility-settings.R")
