# run_all() just source all other code
# TODO move it to src too
# but for the moment, it starts in ..

source("MD-coronavirus-cases.R")
source("MD-coronavirus-counties.R")
source("MD-coronavirus-races.R")
source("MD-coronavirus-Stage1.R")

setwd("src/")
source("zipcode-analysis.R")