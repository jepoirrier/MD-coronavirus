# Produce a simple graph of Coronavirus cases in Maryland State (USA) since the 1st cases detected
# Source of data: https://coronavirus.maryland.gov/ (updated daily at ~ 10am - 12pm)
# Rationale for doing this: no history of past cases and removal of some data (see below)

library(ggplot2)
library(tidyr)

MDCasesFile = 'MD-coronavirus-cases.txt'
# Space-delimited file with 11 fields:
# Date: date in month/day/year format
# TotalNegative: number of total negative cases (was not shown after March 11, 2020)
# TotalPositive: number of total positive cases
# AnneArundel: number of positive cases in Anne Arundel County (started on March 15, 2020)
# BaltimoreCity: number of positive cases in Baltimore City
# BaltimoreCounty: number of positive cases in Baltimore County
# Carroll: number of positive cases in Carroll County
# Charles: number of positive cases in Charles County
# Harford: number of positive cases in Harford County
# Howard: number of positive cases in Howard County
# PrinceGeorges: number of positive cases in Prince George's County
# Montgomery: number of positive cases in Montgomery County
# Talbot: number of positive cases in Talbot County

dat <- read.csv(MDCasesFile, sep = " ", colClasses = c(rep("numeric",27)))
#dat$DateTime <- as.POSIXct(dat$Date, format = "%m/%d/%y")

cols2pivot <- colnames(dat) # headers of columns to pivot
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "County", values_to = "Tests", values_drop_na = TRUE)

p <- ggplot(dt, aes(x = Date, y = Tests, group = County)) +
  geom_line(aes(color = County)) +
  geom_point(aes(color = County)) +
  labs(title = "Evolution of Coronavirus testing in Maryland, USA (2020)",
       x = "Date",
       y = "Tests counts",
       caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))
p # optional
ggsave("MD-coronavirus-cases.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")

