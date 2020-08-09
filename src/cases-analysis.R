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
#
# MDCOVID19 TotalNumberReleasedFromIsolation
# https://data.imap.maryland.gov/datasets/mdcovid19-totalnumberreleasedfromisolation


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

dfClast <- tail(dfC, n = 1)

cols2pivot <- colnames(dfC)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dfCp <- pivot_longer(data = dfC, cols = cols2pivot, names_to = "Tests", values_to = "Cumulative Count", values_drop_na = TRUE)

p <- ggplot(dfCp, aes(x = Date, y = `Cumulative Count`, group = Tests)) +
  {if(logScale) scale_y_log10()} +
  {if(logScale) annotation_logticks()} +
  geom_line(aes(color = Tests), lwd = 1) +
  #geom_point(aes(color = Tests, shape = `Reporting Type`)) +
  theme_linedraw() +
  theme(legend.position = "bottom") + 
  annotate("text", label = paste("On", dfClast$Date, ":\n",
                                 format(dfClast$`Reported positive (ESSENCE)`, scientific = FALSE, big.mark = ","), "reported positive (ESSENCE),\n",
                                 format(dfClast$`Electronic positive`, scientific = FALSE, big.mark = ","), "electronic positive,\n",
                                 format(dfClast$`Reported negative (NEDSS)`, scientific = FALSE, big.mark = ","), "reported negative (NEDSS),\n",
                                 format(dfClast$`Electronic negative`, scientific = FALSE, big.mark = ","), "electronic negative"),
           x = dfClast$Date - 20,
           y = 5000,
           size = 4, fontface = "italic") +
  labs(title = "Evolution of total Coronavirus cases by test result in Maryland, USA (2020)",
       x = "Date",
       y = paste("Cumulative cases", logWarning))
#p

# Graph A2 - simply the daily positive cases

dfD <- df[, c("Date", "DPositives", "EDPositives")]
colnames(dfD) <- c("Date", "Reported positive (ESSENCE)", "Electronic positive")

dfDLast <- tail(dfD, n = 1)

cols2pivot <- colnames(dfD)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dfDp <- pivot_longer(data = dfD, cols = cols2pivot, names_to = "Type", values_to = "Daily Count", values_drop_na = TRUE)

q <- ggplot(dfDp, aes(x = Date, y = `Daily Count`, group = Type)) +
  geom_line(aes(color = Type), lwd = 1) +
  geom_point(aes(color = Type, shape = Type)) +
  theme_linedraw() +
  theme(legend.position = "bottom") + 
  # Annotation for broadening tests
  annotate("segment", x = as.Date("200519", "%y%m%d"), y = 200,
           xend = as.Date("200519", "%y%m%d"), yend = 0,
           size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
  annotate("text", label = "Testing broadening\nMay 19, 2020",
           x = as.Date("200519", "%y%m%d"), y = 250,
           size = 2, fontface = "italic") +
  annotate("text", label = paste("On", dfDLast$Date, ":\n",
                                 format(dfDLast$`Reported positive (ESSENCE)`, scientific = FALSE, big.mark = ","), "reported positive (ESSENCE),\n",
                                 format(dfDLast$`Electronic positive`, scientific = FALSE, big.mark = ","), "electronic positive"),
           x = dfDLast$Date - 20,
           y = 1750,
           size = 4, fontface = "italic") +
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

dfP <- df[, c("Date", "TPcPositive", "DPcPositive", "EDPcPositive", "EDPcPositiveAverage")]
colnames(dfP) <- c("Date", "Cumulative", "Daily reported (ESSENCE)", "Daily electronic", "5-day rolling average electronic")

# small digression: keeping the last row for the annotations
dfPlast <- tail(dfP, n = 1)

cols2pivot <- colnames(dfP)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dfPp <- pivot_longer(data = dfP, cols = cols2pivot, names_to = "Type", values_to = "Percent Positivity", values_drop_na = TRUE)

s <- ggplot(dfPp, aes(x = Date, y = `Percent Positivity`, group = Type)) +
  geom_line(aes(color = Type), lwd = 1) +
  theme_linedraw() +
  theme(legend.position = "bottom") + 
  annotate("text", label = paste("Latest data:", dfPlast$Date, "\n",
                                 "% cumulative:", format(round(dfPlast$Cumulative, 2), nsmall = 2), "%\n",
                                 "% daily reported:", format(round(dfPlast$`Daily reported (ESSENCE)`, 2), nsmall = 2), "%\n",
                                 "% daily electr.:", format(round(dfPlast$`Daily electronic`, 2), nsmall = 2), "%\n",
                                 "% 5day av. electr.:", format(round(dfPlast$`5-day rolling average electronic`, 2), nsmall = 2), "%\n"),
           x = dfClast$Date - 30,
           y = 30,
           size = 3, fontface = "italic") +
  # Annotation for broadening tests
  annotate("segment", x = as.Date("200519", "%y%m%d"), y = 5,
           xend = as.Date("200519", "%y%m%d"), yend = 1,
           size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
  annotate("text", label = "Testing broadening\nMay 19, 2020",
           x = as.Date("200519", "%y%m%d"), y = 7,
           size = 4, fontface = "italic") +
  labs(title = "Evolution of Coronavirus percentage of positivity in Maryland, USA (2020)",
       x = "Date",
       y = "% Positivity",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
#s
ggsave("../figures/cases-pcpositive.png", plot = s, device = "png", width = plotWidth, height = plotHeight, units = "in")


# Release from isolation is a special kind of data. I put it here because it relates more to cases than hospitalizations, in fact

# cumulative total of individuals who tested positive for COVID-19 that have been reported each day by each local health department via the ESSENCE system as having been released from home isolation
RIURL <- "https://opendata.arcgis.com/datasets/02cc59cfe5144cdc9844859615ecc412_0.csv"
RIFile <- "../data/cases-isolation-released.csv"
# RI = Released from Isolation

# Download and import the data
if((as.Date(file.info(RIFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(RIURL, RIFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as RIFile already downloaded today")
}
if(file.exists(RIFile)) {
  print(paste("RIFile found, created on:", file.info(RIFile)$ctime))
} else {
  stop(RIFile, " does not exist") # not the best way to stop (if it even stops!)
}
datRI <- read.csv(RIFile, sep = ",", colClasses = c("integer", "Date", "integer"))
datRI$DATE <- as.Date(datRI$DATE)
print(paste("Latest data point in RIFile:", max(datRI$DATE)))

# Cleanup
datRI$OBJECTID <- NULL
colnames(datRI) = c("Date", "Patients")

# small digression: keeping the last row for the annotations
datRIlast <- tail(datRI, n = 1)

t <- ggplot(datRI, aes(x = Date, y = Patients)) +
  geom_line(lwd = 1) +
  theme_linedraw() +
  theme(legend.position = "bottom") + 
  annotate("text", label = paste("Latest data:", datRIlast$Date, "\n",
                                 format(datRIlast$Patients, scientific = FALSE, big.mark = ","), "patients"),
           x = datRIlast$Date - 10,
           y = datRIlast$Patients / 3 * 2,
           size = 3, fontface = "italic") +
  labs(title = "Evolution of patients released from home isolation in Maryland, USA (2020)",
       x = "Date",
       y = "Number of patients released from home isolation")
#t

# Is there a slow down or an acceleration of released from isolation?

deltaF <- function(x, na.rm = FALSE) (x - lag(x, default = 0))
dt <- datRI
dt <- mutate_all(dt[2:length(dt)], ~ deltaF(.))
dt <- cbind(dt, Date = datRI$Date) # this is risky (desync?) but adds Date as the last column

# small digression: keeping the last row for the annotations
dtlast <- tail(dt, n = 1)

u <- ggplot(dt, aes(x = Date, y = Patients)) +
  geom_line() +
  theme_linedraw() +
  geom_smooth(mapping = aes(x = Date, y = Patients)) +
  theme(legend.position = "bottom") + 
  annotate("text", label = paste("Latest date:", dtlast$Date, "\n",
                                 format(round(dtlast$Patients, 0), nsmall = 0), "patients"),
           x = dtlast$Date - 10,
           y = max(dt$Patients) + 5,
           size = 3, fontface = "italic") +
  labs(title = "Evolution of daily patients released from home isolation in Maryland, USA (2020)",
       x = "Date",
       y = "Number of patients released from home isolation",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
#u

v <- ggarrange(t, u, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
#v
ggsave("../figures/cases-released.png", plot = v, device = "png", width = plotWidth, height = plotHeightLong, units = "in")

# Does daily cases and releases match or are delayed?

dtC <- merge(dt, dfD[, c("Date", "Reported positive (ESSENCE)")] , by = "Date")
colnames(dtC) <- c("Date", "Patients released", "Patients tested positive")

cols2pivot <- colnames(dtC)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtCp <- pivot_longer(data = dtC, cols = cols2pivot, names_to = "Type", values_to = "Count", values_drop_na = TRUE)

w <- ggplot(dtCp, aes(x = Date, y = Count)) +
  geom_line(aes(color = Type), lwd = 1) +
  #geom_point(aes(color = Tests, shape = `Reporting Type`)) +
  theme_linedraw() +
  theme(legend.position = "bottom") + 
  labs(title = "Daily positive cases & released from home isolation in Maryland, USA (2020)",
       x = "Date",
       y = "Number of patients",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
#w
ggsave("../figures/cases-pos-rel-delay.png", plot = w, device = "png", width = plotWidth, height = plotHeight, units = "in")
