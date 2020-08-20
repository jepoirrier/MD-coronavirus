# run_all() just source all other code

# NEW VERSION of code (API data)
# Note: pay attention to last data in data sets. Sometimes, re-downloading give the latest data
source("cases-analysis.R")
source("hospit-analysis.R")
source("deaths-confirmed.R")
source("age-analysis.R")
source("gender-analysis.R") # sometimes needs to be re-run a 2nd time to get today's data
source("races-analysis.R")
source("zipcode-analysis.R")
source("county-analysis.R")
source("contacttracing-analysis.R")
source("congregate-facility-settings.R")
