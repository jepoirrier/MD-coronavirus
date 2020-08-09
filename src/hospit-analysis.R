# Hospitalizations statewide analysis for Maryland
# Using data from the MD Data Respository: https://data.imap.maryland.gov/

# MDCOVID19 TotalHospitalizations
# https://data.imap.maryland.gov/datasets/mdcovid19-totalhospitalizations
#
# MDCOVID19 TotalCurrentlyHospitalizedAcuteAndICU
# https://data.imap.maryland.gov/datasets/mdcovid19-totalcurrentlyhospitalizedacuteandicu


library(dplyr)
library(gghighlight)
library(ggplot2)
library(ggpubr) # for ggarrange
library(ggseas) # for decompose
library(ini)
library(scales) # for x-axis ticks
library(tidyr)
library(MMWRweek) # to read CDC data on hospitalization and age

print("Entering hospit-analysis.R")

plotWidth <- 12
plotHeight <- 7 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong <- 10 # for multiple graphs: 9 (= 3 times 3) + 1
preventMultipleDownload <- FALSE

logScale <- TRUE
logWarning <- ""
if(logScale)
  logWarning = " (log scale!)"

# Read an .ini file with point data in it
iniFile <- "../data-other-sources/pointData.ini"
pointData <- read.ini(iniFile)

# cumulative number of COVID-19 positive Maryland residents who have been hospitalized
HTURL <- "https://opendata.arcgis.com/datasets/9a33ce5efe0e458db636f215433765bd_0.csv"
HTFile <- "../data/hospit-total.csv"
# HT = hospitalization total

# daily occupancy number of COVID-19 designated hospital beds in Maryland
HCURL <- "https://opendata.arcgis.com/datasets/bf3f201b056b4c488b5dac3441b7ac20_0.csv"
HCFile <- "../data/hospit-current.csv"
# HC = hospitalization current


# Download and import the data 1/2
if((as.Date(file.info(HTFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(HTURL, HTFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as HTFile already downloaded today")
}
if(file.exists(HTFile)) {
  print(paste("HTFile found, created on:", file.info(HTFile)$ctime))
} else {
  stop(HTFile, " does not exist") # not the best way to stop (if it even stops!)
}
datHT <- read.csv(HTFile, sep = ",", colClasses = c("integer", "Date", "integer"))
datHT$DATE <- as.Date(datHT$DATE)
print(paste("Latest data point in HTFile:", max(datHT$DATE)))

# Download and import the data 2/2
if((as.Date(file.info(HCFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(HCURL, HCFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as HCFile already downloaded today")
}
if(file.exists(HCFile)) {
  print(paste("HCFile found, created on:", file.info(HCFile)$ctime))
} else {
  stop(HCFile, " does not exist") # not the best way to stop (if it even stops!)
}
datHC <- read.csv(HCFile, sep = ",", colClasses = c("integer", "Date", "integer", "integer", "integer"))
datHC$DATE <- as.Date(datHC$DATE)
print(paste("Latest data point in HCFile:", max(datHC$DATE)))



# Clean the data
# we don't need columna OBJECTID
datHT$OBJECTID <- NULL
datHC$OBJECTID <- NULL
# rename headers
colnames(datHT) <- c("Date", "Total")
colnames(datHC) <- c("Date", "AcuteCare", "ICU", "Total")



# Graph cumulative hostpitalizations

datHTlast <- tail(datHT, n = 1)

p <- ggplot(datHT, aes(x = Date, y = Total)) +
  {if(logScale) scale_y_log10()} +
  {if(logScale) annotation_logticks()} +
  geom_line(lwd = 1) +
  theme_linedraw() +
  annotate("text", label = paste("Latest data:", datHTlast$Date, "\n",
                                 format(datHTlast$Total, scientific = FALSE, big.mark = ","), "hospitalizations cumulative"),
           x = datHTlast$Date - 20,
           y = 1000,
           size = 4, fontface = "italic") +
  labs(title = "Cumulative number of COVID-19 patients ever hospitalized in Maryland, USA (2020)",
       x = "Date",
       y = paste("Total # of patients", logWarning))
#p

dtT <- datHT
dtT <- dtT %>% mutate(Delta = Total - lag(Total, default = 0))

dtT <- subset(dtT, select = c("Date", "Delta"))

dtTlast <- tail(dtT, n = 1)

q <- ggplot(dtT, aes(x = Date, y = Delta)) +
  geom_line(lwd = 1) +
  geom_smooth(mapping = aes(x = Date, y = Delta)) +
  theme_linedraw() +
  annotate("text", label = paste("Latest date:", dtTlast$Date, "\n",
                                 format(dtTlast$Delta, scientific = FALSE, big.mark = ","), "hospitalizations added\nsince day before"),
           x = datHTlast$Date - 20,
           y = 200,
           size = 4, fontface = "italic") +
  labs(title = "Daily variation of COVID-19 patients hospitalized in Maryland, USA (2020)",
       x = "Date",
       y = "Daily delta patients",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
#q

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
#r

ggsave("../figures/hospit-total.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")


# Graph daily utilization (CSP)

cols2pivot <- colnames(datHC)
cols2pivot <- cols2pivot[2:4] # we don't need "Date"

datHClast <- tail(datHC, n = 1)

dtC <- pivot_longer(data = datHC, cols = cols2pivot, names_to = "Hospitalization Type", values_to = "Count")

s <- ggplot(dtC, aes(x = Date, y = Count, group = `Hospitalization Type`)) +
  geom_line(aes(color = `Hospitalization Type`), lwd = 1) +
  theme_linedraw() +
  annotate("text", label = paste("On", datHClast$Date, ":\n",
                                 format(datHClast$AcuteCare, scientific = FALSE, big.mark = ","), "patients in acute care\n",
                                 format(datHClast$ICU, scientific = FALSE, big.mark = ","), "patients in ICU\n",
                                 format(datHClast$Total, scientific = FALSE, big.mark = ","), "patients hospitalized\n"),
           x = datHTlast$Date - 20,
           y = 1000,
           size = 4, fontface = "italic") +
  labs(title = "Daily COVID-19 occupancy in hospitals in Maryland, USA (2020)",
       x = "Date",
       y = "# of patients in hospital beds",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
#s
ggsave("../figures/hospit-CSP.png", plot = s, device = "png", width = plotWidth, height = plotHeight, units = "in")



# calculate + GRAPH  daily delta from the cumulative count (HC)

dtD <- datHC

dtD <- dtD %>% mutate(dailyTotal = Total - lag(Total, default = 0))
dtD <- dtD %>% mutate(dailyAcuteCare = AcuteCare - lag(AcuteCare, default = 0))
dtD <- dtD %>% mutate(dailyICU = ICU - lag(ICU, default = 0))

dtD <- subset(dtD, select = c("Date", "dailyTotal", "dailyAcuteCare", "dailyICU")) # we just need those
colnames(dtD) <- c("Date", "Total", "AcuteCare", "ICU")

dtDlast <- tail(dtD, n = 1)

cols2pivot <- colnames(dtD)
cols2pivot <- cols2pivot[2:4] # we don't need "Date"

dt <- pivot_longer(data = dtD, cols = cols2pivot, names_to = "Type", values_to = "Daily")

t <- ggplot(dt, aes(x = Date, y = Daily, group = Type["Total"])) +
  geom_line() +
  geom_smooth() +
  theme_linedraw() +
  annotate("text", label = paste("On", dtDlast$Date, ":", format(dtDlast$Total, scientific = FALSE, big.mark = ",")),
           x = dtDlast$Date - 15,
           y = 100,
           size = 4, fontface = "italic") +
  labs(title = "Daily number of new hospitalized COVID-19 patients in Maryland, USA (2020)",
       x = "",
       y = "daily delta patients")
#t

u <- ggplot(dt, aes(x = Date, y = Daily, group = Type["AcuteCare"])) +
  geom_line() +
  geom_smooth() +
  theme_linedraw() +
  annotate("text", label = paste("On", dtDlast$Date, ":", format(dtDlast$AcuteCare, scientific = FALSE, big.mark = ",")),
           x = dtDlast$Date - 15,
           y = 100,
           size = 4, fontface = "italic") +
  labs(title = "Daily number of new COVID-19 patients in acute care",
       x = "",
       y = "daily delta patients")
#u

v <- ggplot(dt, aes(x = Date, y = Daily, group = Type["ICU"])) +
  geom_line() +
  geom_smooth() +
  theme_linedraw() +
  annotate("text", label = paste("On", dtDlast$Date, ":", format(dtDlast$ICU, scientific = FALSE, big.mark = ",")),
           x = dtDlast$Date - 15,
           y = 100,
           size = 4, fontface = "italic") +
  labs(title = "Daily number of new COVID-19 patients in ICU",
       x = "Date",
       y = "daily delta patients",
       caption = paste("Negative values: more patients were discharged than admitted on that day\nExplanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
#v


w <- ggarrange(t, u, v, heights = c(1, 1), 
               ncol = 1, nrow = 3, align = "v")
#w
ggsave("../figures/hospit-CSPDaily.png", plot = w, device = "png", width = plotWidth, height = plotHeight, units = "in")



### CDC gives hospitalization by age group from 100% coverage of Maryland:
# Article: https://www.cdc.gov/coronavirus/2019-ncov/covid-data/covid-net/purpose-methods.html
# Interactive graph: https://gis.cdc.gov/grasp/COVIDNet/COVID19_3.html
# I didn't find how to automate data retrieval --> save it in data-other-sources/CDC-hospit-age.csv
# Then delete the 2 first lines + disclaimer @ bottom and name this CDC-hospit-age2.csv

CDCDataHospAgeUpdated = "2020-08-09" # Manually update this date when re-downloading the data above

HAFile <- "../data-other-sources/CDC-hospit-age2.csv"
# HA = hospitalization age

if(file.exists(HAFile)) {
  print(paste("HAFile found, created on:", file.info(HAFile)$ctime))
} else {
  stop(HAFile, " does not exist") # not the best way to stop (if it even stops!)
}

datHA <- read.csv(HAFile, sep = ",", na.strings = "null", colClasses = c("character", "character", "character", "integer", "integer", "character", "numeric", "numeric"))

# Note we keep MMWR year/week to reconstruct the date, we drop cumulative rate and all other columns:
datHA <- subset(datHA, CATCHMENT == "Maryland", select = c("CATCHMENT", "MMWR.YEAR", "MMWR.WEEK", "AGE.CATEGORY", "WEEKLY.RATE"))
datHA$CATCHMENT <- NULL

datHA <- cbind(datHA, mapply(function(x, y) MMWRweek2Date(x, y), datHA$MMWR.YEAR, datHA$MMWR.WEEK))
datHA$MMWR.YEAR <- NULL
datHA$MMWR.WEEK <- NULL
colnames(datHA) <- c("Age Group", "Weekly Rate", "Date")
datHA$Date <- as.Date(datHA$Date, origin = "1970-01-01")

# Removing NAs
datHA <- datHA[!is.na(datHA$`Weekly Rate`),]
# Removing age categories too broad:
datHA <- datHA[!(datHA$`Age Group` == "18-49 yr" | datHA$`Age Group` == "65+ yr" | datHA$`Age Group` == "Overall"),]
# Re-order the age levels
datHA$`Age Group` <- factor(datHA$`Age Group`, levels = c("0-4 yr", "5-17 yr", "18-29 yr", "30-39 yr", "40-49 yr", "50-64 yr", "65-74 yr", "75-84 yr", "85"))
# And rename "85" to "85+"
levels(datHA$`Age Group`)[levels(datHA$`Age Group`) == "85"] <- "85+"


x <- ggplot(datHA, aes(x = Date, y = `Weekly Rate`, group = `Age Group`)) +
  geom_line(aes(color = `Age Group`), lwd = 1) +
  geom_point(aes(color = `Age Group`, shape = `Age Group`)) +
  theme_linedraw() +
  labs(title = "Weekly hospitalization rates by age group in Maryland, USA (2020)",
       x = "Date",
       y = "Rate (per 100,000 population)",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from CDC ; last update:", CDCDataHospAgeUpdated))
x

ggsave("../figures/hospit-CDCAge.png", plot = x, device = "png", width = plotWidth, height = plotHeight, units = "in")

