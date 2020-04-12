# Produce a simple graph of Coronavirus cases in Maryland State (USA) since the 1st cases detected
# Source of data: https://coronavirus.maryland.gov/ (updated daily at ~ 10am)
# Rationale for doing this: no history of past cases and removal of some data (see below)
# Explanations: https://jepoirrier.org (maybe search for "coronavirus")
# Data repository: https://github.com/jepoirrier/MD-coronavirus

library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)

source("fnPlots.R")

# Read the data with total cases
MDCasesFile = 'MD-coronavirus-cases2.txt'
MDCasesNCols <- 6
# Space-delimited file with fields:
# Date: date in YearMonthDay format
# Negative: number of total negative cases (gap in data published between March 11 and March 28, 2020)
# Positive: number of total positive cases
# Death: number of total deaths linked with positive cases
# Hospitalizations: number of people ever hospitalized
# Released: number of people released from hospital
datCases <- read.csv(MDCasesFile, sep = " ", colClasses = c(rep("numeric", MDCasesNCols)))
# Get last date with cases
lastDateCases <- as.Date(sprintf("%d", max(datCases$Date, na.rm = TRUE)), "%y%m%d")
# Get the maximum number of positive cases
lastMaxPositiveCases = max(datCases$Positive, na.rm = TRUE)

# Read the data from the counties
MDCountiesFile <- 'MD-coronavirus-counties.txt'
MDCountiesNCols <- 25
# Space-delimited file with fields:
# Date: date in YearMonthDay format
# AnneArundel: number of positive cases in Anne Arundel County (started on March 15, 2020)
# then all the other counties
datCounty <- read.csv(MDCountiesFile, sep = " ", colClasses = c(rep("numeric", MDCountiesNCols)))
lastDateCounty <- as.Date(sprintf("%d",max(datCounty$Date)), "%y%m%d")
lastMaxCounty = max(datCounty$PrinceGeorges) # TODO manually find the county with max cases - TODO change this behavior

# Read the data from the deaths in counties
MDCountiesDeathsFile <- 'MD-coronavirus-counties-deaths.txt'
MDCountiesDeathsNCols <- 25
# Space-delimited file with fields:
# Date: date in YearMonthDay format
# AnneArundel: number of positive cases in Anne Arundel County (started on March 15, 2020)
# then all the other counties
datCountyDeaths <- read.csv(MDCountiesDeathsFile, sep = " ", colClasses = c(rep("numeric", MDCountiesDeathsNCols)))
lastDateCountyDeaths <- as.Date(sprintf("%d",max(datCountyDeaths$Date)), "%y%m%d")
lastMaxCountyDeaths = max(datCountyDeaths$PrinceGeorges) # TODO manually find the county with max deaths - TODO change this behavior

# Read the data from the age groups: cases (new version since March 27) & deaths (included since April 9)
MDAgeFile = 'MD-coronavirus-byage3.txt' # Switch for 3rd version of age groups presented by MDH
MDAgeNCols <- 4
# Space-delimited file with fields:
# Date: date in YearMonthDate format
# 0-9: number of positive cases aged 0 to 9 years old
# 10-19: number of positive cases aged 10 to 19years old
# 20-29: number of positive cases aged 20 to 29 years old
# 30-39: number of positive cases aged 30 to 39 years old
# 40-49: number of positive cases aged 40 to 49 years old
# 50-59: number of positive cases aged 50 to 59 years old
# 60-69: number of positive cases aged 60 to 69 years old
# 70-79: number of positive cases aged 70 to 79 years old
# 80+: number of positive cases aged 80 years old and over
# D...: number of deaths in the age category
datAge <- read.csv(MDAgeFile, sep = " ", colClasses = c("numeric", "character", "character", "numeric"))

# Read data by gender
MDGenderFile = 'MD-coronavirus-bygender.txt'
MDGenderCols <- 5
# Space-delimited file with fields:
# Date: date in YearMonthDate format
# Female: number of female positive cases
# Male: number of male positive cases
# DFemale: number of female deaths
# DMale: number of male deaths
datGender <- read.csv(MDGenderFile, sep = " ", colClasses = c(rep("numeric", MDGenderCols)))

# Read data for race distribution
MDRaceFile = 'MD-coronavirus-byrace.txt'
MDRaceNCols <- 11
# Space-delimited file with fields:
# Date: date in YearMonthDate format
# AfricanAmerican: number of cases in African-Americans
# Asian: number of cases in Asians
# White: number of cases in Whites
# Other: number of cases in other races
# DataNotAvailable: number of cases for which data is not available
# D...: number of deaths for that community
datRace <- read.csv(MDRaceFile, sep = " ", colClasses = c(rep("numeric", MDRaceNCols)))

# Plot things now
p <- plotTotalCasesOverTime(datCases, lastDateCases, lastMaxPositiveCases, logScale = TRUE)
p
p <- plotDailyCasesOverTime(datCases, logScale = FALSE)
p
p <- plotPositiveTestPc(datCases, lastDateCases, lastMaxPositiveCases)
p
p <- plotCountyCasesOverTime(datCounty)
p
p <- plotCountyDeathsOverTime(datCountyDeaths)
p
p <- plotAgeGroupsCases(datAge)
p
p <- plotAgeGroupsDeaths(datAge)
p
p <- plotAgeGroupsSection(datAge)
p
p <- plotGenderSection(datGender)
p
p <- plotRaceCasesOverTime(datRace)
p
p <- plotRaceSection(datRace)
p
