# Cases and deaths for counties in Maryland
# Using data from the MD Data Respository: https://data.imap.maryland.gov/

# dataset 1 - MDCOVID19 TotalPopulationTestedByCounty
# https://data.imap.maryland.gov/datasets/mdcovid19-totalpopulationtestedbycounty
#
# dataset 2 - MDCOVID19 DailyTestingVolumeByCounty
# https://data.imap.maryland.gov/datasets/mdcovid19-dailytestingvolumebycounty
# 
# dataset 3 - MDCOVID19 TotalTestingVolumeByCounty
# https://data.imap.maryland.gov/datasets/mdcovid19-totaltestingvolumebycounty
# 
# dataset 4 - MDCOVID19 CasesByCounty
# https://data.imap.maryland.gov/datasets/mdcovid19-casesbycounty
# 
# dataset 5 - MDCOVID19 PosPercentByJursidiction
# https://data.imap.maryland.gov/datasets/mdcovid19-pospercentbyjursidiction
# 
# dataset 6 - MDCOVID19 ConfirmedDeathsByCounty
# https://data.imap.maryland.gov/datasets/mdcovid19-confirmeddeathsbycounty

library(dplyr)
library(gghighlight)
library(ggplot2)
library(ggpubr) # for ggarrange
library(ggseas) # for decompose
library(ini)
library(scales) # for x-axis ticks
library(tidyr)

print("Entering county-analysis.R")

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

# cumulative number of residents tested at least once - from dataset 1 above
C1URL <- "https://opendata.arcgis.com/datasets/736d3e5a280840a7916ab309b6ac8908_0.csv"
C1File <- "../data/counties-residentsonce.csv"


# Download and import the data
if((as.Date(file.info(C1File)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(C1URL, C1File, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as C1File already downloaded today")
}
if(file.exists(C1File)) {
  print(paste("C1File found, created on:", file.info(C1File)$ctime))
} else {
  stop(C1File, " does not exist") # not the best way to stop (if it even stops!)
}
datC1 <- read.csv(C1File, sep = ",")

# Clean the data
# we don't need column OBJECTID
datC1$OBJECTID <- NULL
C1cols <- colnames(datC1)
# remove 'F' if string starts with 'd_'
C1cols <- gsub('d_', '', C1cols)
# remove 'F' if string starts with 'd'
C1cols <- gsub('d', '', C1cols)

colnames(datC1) <- C1cols

cols2pivot <- C1cols[2:length(C1cols)] # we don't need the county here
dt <- pivot_longer(data = datC1, cols = cols2pivot, names_to = "Date", values_to = "Tests", values_drop_na = TRUE)
dt$Date <- as.character(dt$Date)
dt$Date <- as.Date(sprintf("%s",dt$Date), "%m_%d_%Y")
print(paste("Latest data point in C1File:", max(dt$Date)))

# CHART ALL COUNTIES TESTS over time

a <- ggplot(dt, aes(x = Date, y = Tests, group = County)) +
  geom_line(aes(color = County)) +
  geom_point(aes(color = County, shape = County)) +
  #gghighlight(County == "Somerset") +
  theme_linedraw() +
  labs(title = "Evolution of Coronavirus tests in Maryland counties, USA (2020)",
       y = "Total residents tested at least once")
a

# CHART ALL COUNTIES TESTS per 100,000 over time

# Get county projections from the MD State
MDCountiesPopFile <- '../data-other-sources/TotalPopProj.dat'
MDCountiesPopNCols <- 24
datCountyPop <- read.csv(MDCountiesPopFile, sep = " ", colClasses = c(rep("numeric", MDCountiesPopNCols)))
# This is dangerous and needs to make sure it's in the same order - otherwise replace individually
colnames(datCountyPop) <- datC1$County
dtCountyPop <- pivot_longer(data = datCountyPop, cols = colnames(datCountyPop), names_to = "County", values_to = "Pop", values_drop_na = TRUE)

dt2 <- merge(dt, dtCountyPop, by = "County") # yes, redo this, TODO fix this
# Calculates tests %:
dt2$TPC <- dt2$Tests / dt2$Pop * 100
# some cleanup:
dt2$Tests <- NULL
dt2$Pop <- NULL

b <- ggplot(dt2, aes(x = Date, y = TPC, group = County)) +
  geom_line(aes(color = County)) +
  geom_point(aes(color = County, shape = County)) +
  #gghighlight(County == "Somerset") +
  annotate("text", label = paste("(issue with Somerset in dataset)"),
           x = as.Date("2020-06-18") + 7,
           y = 30,
           size = 3, fontface = "italic") +
  theme_linedraw() +
  labs(title = "Evolution of percentage of population in Maryland counties ever tested",
       y = "% residents tested at least once",
       caption = paste("Total number of residents who have been administered at least one COVID-19 test in each Maryland jurisdiction\nExplanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last data update:", format(max(dt$Date), "%b %d, %Y")))
b

c <- ggarrange(a, b, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
c
ggsave("../figures/counties-residentsonce.png", plot = c, device = "png", width = plotWidth, height = plotHeightLong, units = "in")

# CHART ALL COUNTIES TESTS relative to population
# We don't really use this - but just in case

dt3 <- merge(dt, dtCountyPop, by = "County")
# Calculates tests per hundred thousands:
dt3$TPHT <- dt3$Tests / dt3$Pop * 100000
# some cleanup:
dt3$Tests <- NULL
dt3$Pop <- NULL

d <- ggplot(dt3, aes(x = Date, y = TPHT, group = County)) +
  geom_line(aes(color = County)) +
  geom_point(aes(color = County, shape = County)) +
  #gghighlight(County == "Somerset") +
  annotate("text", label = paste("(issue with Somerset in dataset)"),
           x = as.Date("2020-06-18") + 7,
           y = 30000,
           size = 3, fontface = "italic") +
  theme_linedraw() +
  labs(title = "Evolution of Coronavirus tests relative to pop in Maryland counties, USA (2020)",
       y = "Total residents tested at least once (per 100,000)",
       caption = paste("Total number of residents who have been administered at least one COVID-19 test in each Maryland jurisdiction\nExplanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last data update:", format(max(dt$Date), "%b %d, %Y")))
d
ggsave("../figures/counties-residentsonceRel.png", plot = c, device = "png", width = plotWidth, height = plotHeight, units = "in")

