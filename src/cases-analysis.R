# Cases & Deaths statewide analysis for Maryland
# Using data from the MD Data Respository: https://data.imap.maryland.gov/

# MDCOVID19 TotalCasesStatewide
# https://data.imap.maryland.gov/datasets/mdcovid19-totalcasesstatewide
#
# MDCOVID19 NumberOfPersonsTestedNegative
# https://data.imap.maryland.gov/datasets/mdcovid19-numberofpersonstestednegative
#
# MDCOVID19 TestingVolume
# https://data.imap.maryland.gov/datasets/mdcovid19-testingvolume

library(dplyr)
library(gghighlight)
library(ggplot2)
library(ggpubr) # for ggarrange
library(ggseas) # for decompose
library(ini)
library(scales) # for x-axis ticks
library(tidyr)

print("Entering cases-analysis.R")

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

# statewide positive COVID-19 test results that have been reported each day by each local health department via the ESSENCE system
CPURL <- "https://opendata.arcgis.com/datasets/18582de727934249b92c52542395a3bf_0.csv"
CPFile <- "../data/cases-positives.csv"
# CP = cases positive

# number of people statewide who have tested negative for COVID-19 reported each day by each local health department via the NEDSS system
CNURL <- "https://opendata.arcgis.com/datasets/67c49f40064c45f9aadfcc9298cba9e6_0.csv"
CNFile <- "../data/cases-negatives.csv"

# static daily total of PCR COVID-19 tests electronically reported for Maryland residents
# !!! this count does not include test results submitted by labs and other clinical facilities through non-electronic means
# The percent positive rate is a five-day rolling average of positive results as a percentage of all tests.
CTURL <- "https://opendata.arcgis.com/datasets/e5d044bda3a44bfa8ba224145617cda0_0.csv"
CTFile <- "../data/cases-total.csv"

# Download and import the data 1/3
if((as.Date(file.info(CPFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(CPURL, CPFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as CPFile already downloaded today")
}
if(file.exists(CPFile)) {
  print(paste("CPFile found, created on:", file.info(CPFile)$ctime))
} else {
  stop(CPFile, " does not exist") # not the best way to stop (if it even stops!)
}
datCP <- read.csv(CPFile, sep = ",", colClasses = c("integer", "Date", "integer"))
datCP$DATE <- as.Date(datCP$DATE)
print(paste("Latest data point in CPFile:", max(datCP$DATE)))

# Download and import the data 2/3
if((as.Date(file.info(CNFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(CNURL, CNFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as CNFile already downloaded today")
}
if(file.exists(CNFile)) {
  print(paste("CNFile found, created on:", file.info(CNFile)$ctime))
} else {
  stop(CNFile, " does not exist") # not the best way to stop (if it even stops!)
}
datCN <- read.csv(CNFile, sep = ",", colClasses = c("integer", "Date", "integer"))
datCN$DATE <- as.Date(datCN$ReportDate)
print(paste("Latest data point in CNFile:", max(datCN$DATE)))

# Download and import the data 3/3
if((as.Date(file.info(CTFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(CTURL, CTFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as CTFile already downloaded today")
}
if(file.exists(CTFile)) {
  print(paste("CTFile found, created on:", file.info(CTFile)$ctime))
} else {
  stop(CTFile, " does not exist") # not the best way to stop (if it even stops!)
}
datCT <- read.csv(CTFile, sep = ",", colClasses = c("integer", "Date", "integer", "integer", "numeric", "numeric"))
datCT$DATE <- as.Date(datCT$date)
print(paste("Latest data point in CNFile:", max(datCT$DATE)))



# Clean the data
# we don't need columna OBJECTID nor the old date field
datCP$OBJECTID <- NULL
datCN$OBJECTID <- NULL
datCN$ReportDate <- NULL
datCT$OBJECTID <- NULL
datCT$date <- NULL
# rename headers
colnames(datCP) <- c("Date", "Positives")
colnames(datCN) <- c("Negatives", "Date")
colnames(datCT) <- c("EDTotal", "EDPositives", "EDPcPositive", "EDPcPositiveAverage", "Date")

# rebuild CT: daily negatives + from daily numbers to cumulative
datCT$EDNegatives <- datCT$EDTotal - datCT$EDPositives

datCT$ETotal <- cumsum(datCT$EDTotal)
datCT$EPositives <- cumsum(datCT$EDPositives)
datCT$ENegatives <- cumsum(datCT$EDNegatives)

# merge Positives and Negatives; calculate total and % positive for total reported
df <- merge(datCP, datCN, by = "Date")
df$Total <- df$Positives + df$Negatives
df$TPcPositive <- df$Positives / df$Total * 100 # % positivity on the total reported
# calculate daily delta from the cumulative count
deltaF <- function(x, na.rm = FALSE) (x - lag(x, default = 0))
dfX <- merge(datCP, datCN, by = "Date") # redoing it - maybe improved (but not dfX <- df)
dfX <- mutate_all(dfX[2:length(dfX)], ~ deltaF(.))
dfX <- cbind(Date = df$Date, dfX) # this is risky (desync?) but adds Date
dfX$Total <- dfX$Positives + dfX$Negatives
colnames(dfX) <- c("Date", "DPositives", "DNegatives", "DTotal")
dfX$DPcPositive <- dfX$DPositives / dfX$DTotal * 100 # % positivity on a daily basis on the total
df <- merge(df, dfX, by = "Date")

df <- merge(df, datCT, by = "Date")

# Graph A1 - simply the cumulative cases

dfC <- df[, c("Date", "Positives", "Negatives", "EPositives", "ENegatives")] # C = Cumulative
colnames(dfC) <- c("Date", "Reported positive (ESSENCE)", "Reported negative (NEDSS)", "Electronic positive", "Electronic negative")

cols2pivot <- colnames(dfC)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dfCp <- pivot_longer(data = dfC, cols = cols2pivot, names_to = "Tests", values_to = "Cumulative Count", values_drop_na = TRUE)
# dfCp$`Reporting Type`[dfCp$Tests == "Reported Positive"] <- "ESSENCE"
# dfCp$`Reporting Type`[dfCp$Tests == "Reported Negative"] <- "NEDSS"
# dfCp$`Reporting Type`[dfCp$Tests == "Electronic Positive"] <- "Electronic"
# dfCp$`Reporting Type`[dfCp$Tests == "Electronic Negative"] <- "Electronic"
# dfCp$Result[dfCp$Tests == "Reported Positive"] <- "Positive"
# dfCp$Result[dfCp$Tests == "Reported Negative"] <- "Negative"
# dfCp$Result[dfCp$Tests == "Electronic Positive"] <- "Positive"
# dfCp$Result[dfCp$Tests == "Electronic Negative"] <- "Negative"

p <- ggplot(dfCp, aes(x = Date, y = `Cumulative Count`, group = Tests)) +
  {if(logScale) scale_y_log10()} +
  {if(logScale) annotation_logticks()} +
  geom_line(aes(color = Tests), lwd = 1) +
  #geom_point(aes(color = Tests, shape = `Reporting Type`)) +
  theme_linedraw() +
  theme(legend.position = "bottom") + 
  labs(title = "Evolution of total Coronavirus cases by test result in Maryland, USA (2020)",
       x = "Date",
       y = paste("Cumulative cases", logWarning))
#p

# Graph A2 - simply the daily positive cases

dfC <- df[, c("Date", "DPositives", "EDPositives")] # recycling C
colnames(dfC) <- c("Date", "Reported positive (ESSENCE)", "Electronic positive")

cols2pivot <- colnames(dfC)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dfCp <- pivot_longer(data = dfC, cols = cols2pivot, names_to = "Type", values_to = "Daily Count", values_drop_na = TRUE)

q <- ggplot(dfCp, aes(x = Date, y = `Daily Count`, group = Type)) +
  geom_line(aes(color = Type), lwd = 1) +
  geom_point(aes(color = Type, shape = Type)) +
  theme_linedraw() +
  theme(legend.position = "bottom") + 
  labs(title = "Evolution of daily Coronavirus positive cases in Maryland, USA (2020)",
       x = "Date",
       y = "# positive cases reported",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
#q

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
#r
ggsave("../figures/cases.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")


# Graph B - percentage positivity

dfC <- df[, c("Date", "TPcPositive", "DPcPositive", "EDPcPositive", "EDPcPositiveAverage")] # recycling C
colnames(dfC) <- c("Date", "Cumulative", "Daily reported (ESSENCE)", "Daily electronic", "5-day rolling average electronic")

# small digression: keeping the last row for the annotations
dfClast <- tail(dfC, n = 1)

cols2pivot <- colnames(dfC)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dfCp <- pivot_longer(data = dfC, cols = cols2pivot, names_to = "Type", values_to = "Percent Positivity", values_drop_na = TRUE)

s <- ggplot(dfCp, aes(x = Date, y = `Percent Positivity`, group = Type)) +
  geom_line(aes(color = Type), lwd = 1) +
  #geom_point(aes(color = Tests, shape = `Reporting Type`)) +
  theme_linedraw() +
  theme(legend.position = "bottom") + 
  annotate("text", label = paste("Latest data:", dfClast$Date, "\n",
                                 "% cumulative:", format(round(dfClast$Cumulative, 2), nsmall = 2), "%\n",
                                 "% daily reported:", format(round(dfClast$`Daily reported (ESSENCE)`, 2), nsmall = 2), "%\n",
                                 "% daily electr.:", format(round(dfClast$`Daily electronic`, 2), nsmall = 2), "%\n",
                                 "% 5day av. electr.:", format(round(dfClast$`5-day rolling average electronic`, 2), nsmall = 2), "%\n"),
           x = dfClast$Date - 30,
           y = 30,
           size = 3, fontface = "italic") +
  labs(title = "Evolution of Coronavirus percentage of positivity in Maryland, USA (2020)",
       x = "Date",
       y = "% Positivity",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
#s
ggsave("../figures/cases-pcpositive.png", plot = s, device = "png", width = plotWidth, height = plotHeight, units = "in")

