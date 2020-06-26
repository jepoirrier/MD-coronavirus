# Cases & Deaths by congregate facility settings - analysis of data from Maryland
# Using data from the MD Data Respository: https://data.imap.maryland.gov/

# WORK IN PROGRESS!

# MD COVID-19 - Total Cases in Congregate Facility Settings
# https://data.imap.maryland.gov/datasets/md-covid-19-total-cases-in-congregate-facility-settings
# Download URL to check if not moving every day
#
# MD COVID-19 - Total Deaths in Congregate Facility Settings
# https://data.imap.maryland.gov/datasets/md-covid-19-total-deaths-in-congregate-facility-settings
# Download URL to check if not moving every day

print("Entering congregate-facility-settings.R")

library(dplyr)
library(gghighlight)
library(ggplot2)
library(ggseas) # for decompose
library(ini)
library(scales) # for x-axis ticks
library(tidyr)

plotWidth <- 12
plotHeight <- 7 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong <- 10 # for multiple graphs: 9 (= 3 times 3) + 1
Nbreaks <- 10 # default number of breaks for trend decomposition
preventMultipleDownload <- TRUE

logWarning <- ""
logScale <- FALSE
if (logScale)
  logWarning <- " (log scale!)"
  

# CUMULATIVE CURRENT cases confirmed
# CFC = congregate facility settings cases
CFCURL <- "https://opendata.arcgis.com/datasets/10626dfb579244fca496460e3bd1c6e4_0.csv"
CFCFile <- "../data/cfs-cases.csv"

# Download the data
if((as.Date(file.info(CFCFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(CFCURL, CFCFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as CFCFile already downloaded today")
}
if(file.exists(CFCFile)) {
  print(paste("CFCFile found, created on:", file.info(CFCFile)$ctime))
} else {
  stop(CFCFile, " does not exist") # not the best way to stop (if it even stops!)
}

# Import the case data
datCFC <- read.csv(CFCFile, sep = ",", colClasses = c("character", "integer", "integer", "integer", "integer", "integer", "integer", "integer"))
datCFC$DATE <- as.Date(sprintf("%s", datCFC$DATE), "%m/%d/%y")
print(paste("Latest data point in RCFile:", max(datCFC$DATE)))

# Clean the data
# we don't need column OBJECTID
datCFC$OBJECTID <- NULL
# rename headers
colnames(datCFC) <- c("Date", "Nursing Staff", "Nursing Residents", "State/Local Staff", "State/Local Patients", "State/Local Inmates", "State/Local Youth")
# create a Total
datCFC$Total <- datCFC$`Nursing Staff` + datCFC$`Nursing Residents` +
  datCFC$`State/Local Staff` + datCFC$`State/Local Patients` + datCFC$`State/Local Patients` +
  datCFC$`State/Local Inmates` + datCFC$`State/Local Youth`

### Graph CASES

cols2pivot <- colnames(datCFC)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtCFC <- pivot_longer(data = datCFC, cols = cols2pivot, names_to = "Categories", values_to = "Cases", values_drop_na = TRUE)

p <- ggplot(dtCFC, aes(x = Date, y = Cases, group = Categories)) +
  geom_line(aes(color = Categories), lwd = 1) +
  geom_point(aes(color = Categories, shape = Categories)) +
  {if(logScale) scale_y_log10()} +
  {if(logScale) annotation_logticks()} +
  theme_linedraw() +
  labs(title = "Evolution of total current* cases in Congregate Facility Settings in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative current* cases")
p



# CUMULATIVE CURRENT deaths confirmed
# CFD = congregate facility settingsdeaths
CFDURL <- "https://opendata.arcgis.com/datasets/ee4bf79e9d6348b580899c1ec5dbc2eb_0.csv?outSR=4326"
CFDFile <- "../data/cfs-deaths.csv"

# Download the data
if((as.Date(file.info(CFDFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(CFDURL, CFDFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as CFDFile already downloaded today")
}
if(file.exists(CFDFile)) {
  print(paste("CFDFile found, created on:", file.info(CFDFile)$ctime))
} else {
  stop(CFDFile, " does not exist") # not the best way to stop (if it even stops!)
}

# Import the case data
datCFD <- read.csv(CFDFile, sep = ",", colClasses = c("character", "integer", "integer", "integer", "integer", "integer", "integer", "integer"))
datCFD$DATE <- as.Date(sprintf("%s", datCFD$DATE), "%m/%d/%y")
print(paste("Latest data point in RDFile:", max(datCFD$DATE)))

# Clean the data
# we don't need column OBJECTID
datCFD$OBJECTID <- NULL
# rename headers
colnames(datCFD) <- c("Date", "Nursing Staff", "Nursing Residents", "State/Local Staff", "State/Local Patients", "State/Local Inmates", "State/Local Youth")
# create a Total
datCFD$Total <- datCFD$`Nursing Staff` + datCFD$`Nursing Residents` +
  datCFD$`State/Local Staff` + datCFD$`State/Local Patients` + datCFD$`State/Local Patients` +
  datCFD$`State/Local Inmates` + datCFD$`State/Local Youth`

### Graph DEATHS

cols2pivot <- colnames(datCFD)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtCFD <- pivot_longer(data = datCFD, cols = cols2pivot, names_to = "Categories", values_to = "Deaths", values_drop_na = TRUE)

q <- ggplot(dtCFD, aes(x = Date, y = Deaths, group = Categories)) +
  geom_line(aes(color = Categories), lwd = 1) +
  geom_point(aes(color = Categories, shape = Categories)) +
  {if(logScale) scale_y_log10()} +
  {if(logScale) annotation_logticks()} +
  theme_linedraw() +
  labs(title = "Evolution of total current* deaths in Congregate Facility Settings in Maryland, USA (2020)",
       x = "Date",
       y = paste("Cumulative number* of deaths", logWarning),
       caption = paste("* Facilities are removed from the list when health officials determine 14 days have passed with no new cases/deaths and no tests pending\nExplanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last data update:", format(max(datCFC$Date), "%b %d, %Y")))
q


# Group cases and deaths
r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
r
ggsave("../figures/cfs.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")

### Now illustrate why it's bad to remove facilities from the list:
# - you can't have a reliable proportion of % cases in CFS because total cases is really cumulative
# - you don't know the real toll in CFS (and the other datasets on CFS are also removing data)

# CUMULATIVE cases confirmed (in CFS and other settings)
# C = cases
CURL <- "https://opendata.arcgis.com/datasets/18582de727934249b92c52542395a3bf_0.csv"
CFile <- "../data/state-totalcases.csv"

# Download the data
if((as.Date(file.info(CFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(CURL, CFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as CFile already downloaded today")
}
if(file.exists(CFile)) {
  print(paste("CFile found, created on:", file.info(CFile)$ctime))
} else {
  stop(CFile, " does not exist") # not the best way to stop (if it even stops!)
}

# Import the State total case data
datC <- read.csv(CFile, sep = ",", colClasses = c("integer", "Date", "integer"))
print(paste("Latest data point in CFile:", max(datC$DATE)))

# Clean the data
# we don't need column OBJECTID
datC$OBJECTID <- NULL
# rename headers
colnames(datC) <- c("Date", "Cases")
# join the total cases from CFS
X <- data.frame(datCFC$Date, datCFC$Total)
colnames(X) <- c("Date", "Total")
datRel <- merge(datC, X, by.x = "Date")
colnames(datRel) <- c("Date", "Total", "in CFS")

### Graph both CASES

cols2pivot <- colnames(datRel)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtRel <- pivot_longer(data = datRel, cols = cols2pivot, names_to = "Location", values_to = "Cases", values_drop_na = TRUE)

s <- ggplot(dtRel, aes(x = Date, y = Cases, group = Location)) +
  geom_line(aes(color = Location), lwd = 1) +
  geom_point(aes(color = Location, shape = Location)) +
  {if(logScale) scale_y_log10()} +
  {if(logScale) annotation_logticks()} +
  theme_linedraw() +
  labs(title = "Evolution of total/CFS cases in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative* cases")
s

### Graph CFS relative to Total

datRel$Rel <- datRel$`in CFS` / datRel$Total * 100

t <- ggplot(datRel, aes(x = Date, y = Rel)) +
  geom_line(lwd = 1) +
  geom_point() +
  theme_linedraw() +
  labs(title = "Evolution of proportion of cases in congragate facility settings in Maryland, USA (2020)",
       x = "Date",
       y = "% cases in CFS compared to total # cases in State",
       caption = paste("* Facilities are removed from the list when health officials determine 14 days have passed with no new cases and no tests pending\nExplanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last data update:", format(max(datCFC$Date), "%b %d, %Y"))) +
  annotate("text", label = "Wrong chart: number of cases in congregate facility settings is not cumulative\n and this shows an artificial decrease of proportion",
           x = min(datRel$Date) + 25, y = 13,
           size = 4, fontface = "italic")
t

# Group totals and relative
u <- ggarrange(s, t, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
u
ggsave("../figures/cfs-relative.png", plot = u, device = "png", width = plotWidth, height = plotHeightLong, units = "in")

### Maybe we can try to aggregate all numbers by facility and date and get the real cumulative total

# MD COVID-19 - Number of Cases by Affected Congregate Facility
# CF = cases in facilities
CFURL <- "https://opendata.arcgis.com/datasets/614189852ee74bb98278c5f6fdae0b7a_0.csv"
CFFile <- "../data/cfs-facilities.csv"

# Download the data
if((as.Date(file.info(CFFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(CFURL, CFFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as CFFile already downloaded today")
}
if(file.exists(CFFile)) {
  print(paste("CFFile found, created on:", file.info(CFFile)$ctime))
} else {
  stop(CFFile, " does not exist") # not the best way to stop (if it even stops!)
}

# Import the State total case data
datCF <- read.csv(CFFile, sep = ",", na.strings = "N/A", colClasses = c("character", "character", "character", "integer", "integer", "integer", "integer", "integer", "integer", "integer"))
datCF$DATE <- as.Date(sprintf("%s", datCF$DATE), "%m/%d/%y")
print(paste("Latest data point in CFFile:", max(datCF$DATE)))

# Clean the data
# we don't need column OBJECTID
datCF$OBJECTID <- NULL
# we don't need the County here
datCF$COUNTY <- NULL
# rename headers
colnames(datCF) <- c("Date", "Facility", "Nursing Staff", "Nursing Residents", "State/Local Staff", "State/Local Patients", "State/Local Inmates", "State/Local Youth")

# Small test on June 25: try a facility that initially reported then stopped reporting
# This graph can be removed if needed, just for illustration purpose (and blog post)
tmp <- datCF[datCF$Facility == "Cumberland Healthcare Center", ] # 1st in dataset but reported until the end
tmp <- datCF[datCF$Facility == "Sterling Care Frostburg Village", ] # 2nd in dataset and stopped reporting on 6/10
tmp$`State/Local Staff` <- NULL
tmp$`State/Local Patients` <- NULL
tmp$`State/Local Inmates` <- NULL
tmp$`State/Local Youth` <- NULL
tmp$Facility <- NULL

cols2pivot <- colnames(tmp)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtRel <- pivot_longer(data = tmp, cols = cols2pivot, names_to = "Category", values_to = "Cases", values_drop_na = TRUE)

z <- ggplot(dtRel, aes(x = Date, y = Cases, group = Category)) +
  expand_limits(x = c(as.Date("04/25/20", "%m/%d/%y"), as.Date("06/25/20", "%m/%d/%y"))) +
  geom_line(aes(color = Category), lwd = 1) +
  geom_point(aes(color = Category, shape = Category)) +
  theme_linedraw() +
  labs(title = "Evolution of COVID-19 cases in Sterling Care Frostburg Village",
       x = "Date",
       y = "Cumulative cases",
       caption = paste("Graph created to illustrate the removal of facilities in MD data when reporting stop for > 14 days\nExplanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last data update:", format(max(datCF$Date), "%b %d, %Y")))
z
ggsave("../figures/cfs-SterlingCare.png", plot = z, device = "png", width = plotWidth, height = plotHeight, units = "in")
# End of test

### ARRIVED HERE but I need to go to sleep
###
### The example with z below doesn't work because it is impacted by facilities that are removed at later date(s)
###
### The idea would be to keep the datCF$Facility and ...
# for the 1st day, make a list of all facilities and their numbers
# for each subsequent day, compare facilities in this list to facilities in this day:
# either 1) the facility is still there -> update for this day with (latest) number
# or 2) the facility is not there anymore -> take the latest numbers (from previous day) and paste them here for this facility
# with this new dataframe, then we can summarise_all() like I'm currently doing for z below
### After that, display p, q, r, s, t, u with new cumulative cases
### and delete MD-coronavirus-nursing.txt and associated figures

# Continuing to display new cumulative numbers
# Now we don't need facilities anymore
datCF$Facility <- NULL
# Change colnames to sth easy to manipulate for now
colnames(datCF) <- c("Date", "NS", "NR", "SLS", "SLP", "SLI", "SLY")
Z <- group_by(datCF, Date) %>% summarise_all(list(sum), na.rm = TRUE)
# create a Total
datCFD$Total <- datCFD$`Nursing Staff` + datCFD$`Nursing Residents` +
  datCFD$`State/Local Staff` + datCFD$`State/Local Patients` + datCFD$`State/Local Patients` +
  datCFD$`State/Local Inmates` + datCFD$`State/Local Youth`

# rewriting the correct colnames (do not forget the "Cumulative" at the end)
colnames(datCF) <- c("Date", "Nursing Staff", "Nursing Residents", "State/Local Staff", "State/Local Patients", "State/Local Inmates", "State/Local Youth", "Cumulative")



# join the total cases from CFS
X <- data.frame(datCFC$Date, datCFC$Total)
colnames(X) <- c("Date", "Total")
datRel <- merge(datC, X, by.x = "Date")
colnames(datRel) <- c("Date", "Total", "in CFS")

### Graph both CASES

cols2pivot <- colnames(datRel)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtRel <- pivot_longer(data = datRel, cols = cols2pivot, names_to = "Location", values_to = "Cases", values_drop_na = TRUE)

v <- ggplot(dtRel, aes(x = Date, y = Cases, group = Location)) +
  geom_line(aes(color = Location), lwd = 1) +
  geom_point(aes(color = Location, shape = Location)) +
  {if(logScale) scale_y_log10()} +
  {if(logScale) annotation_logticks()} +
  theme_linedraw() +
  labs(title = "Evolution of total/CFS cases in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative* cases")
v

### Graph CFS relative to Total

datRel$Rel <- datRel$`in CFS` / datRel$Total * 100

w <- ggplot(datRel, aes(x = Date, y = Rel)) +
  geom_line(lwd = 1) +
  geom_point() +
  theme_linedraw() +
  labs(title = "Evolution of proportion of cases in congragate facility settings in Maryland, USA (2020)",
       x = "Date",
       y = "% cases in CFS compared to total # cases in State",
       caption = paste("* Facilities are removed from the list when health officials determine 14 days have passed with no new cases and no tests pending\nExplanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last data update:", format(max(datCFC$Date), "%b %d, %Y"))) +
  annotate("text", label = "Wrong chart: number of cases in congregate facility settings is not cumulative\n and this shows an artificial decrease of proportion",
           x = min(datRel$Date) + 25, y = 13,
           size = 4, fontface = "italic")
w

# Group totals and relative
x <- ggarrange(v, w, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
x
ggsave("../figures/cfs-cumulative.png", plot = x, device = "png", width = plotWidth, height = plotHeightLong, units = "in")

