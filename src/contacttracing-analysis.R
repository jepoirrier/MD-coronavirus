# Cases & Contacts reported to be contacted for Maryland
# Using data from the MD Data Respository: https://data.imap.maryland.gov/

# MD COVID 19 ContactTracing CasesReachedAndInterviewed
# https://data.imap.maryland.gov/datasets/md-covid-19-contacttracing-casesreachedandinterviewed
#
# MD COVID 19 ContactTracing ContactReachedAndInterviewed
# https://data.imap.maryland.gov/datasets/md-covid-19-contacttracing-contactreachedandinterviewed

library(dplyr)
library(gghighlight)
library(ggplot2)
library(ggpubr) # for ggarrange
library(ggseas) # for decompose
library(ini)
library(scales) # for x-axis ticks
library(tidyr)

print("Entering contacttracing-analysis.R")

plotWidth <- 12
plotHeight <- 7 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong <- 10 # for multiple graphs: 9 (= 3 times 3) + 1
Nbreaks <- 10 # default number of breaks for trend decomposition
preventMultipleDownload <- FALSE

# Read an .ini file with point data in it
iniFile <- "../data-other-sources/pointData.ini"
pointData <- read.ini(iniFile)

# Cases contacted

CCURL <- "https://opendata.arcgis.com/datasets/b4408624b02b4d0ea07c8b68540f485b_0.csv"
CCFile <- "../data/contacttracing-cases.csv"
# CC = cases contacted

# Download the data
if((as.Date(file.info(CCFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(CCURL, CCFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as CCFile already downloaded today")
}
if(file.exists(CCFile)) {
  print(paste("CCFile found, created on:", file.info(CCFile)$ctime))
} else {
  stop(CCFile, " does not exist") # not the best way to stop (if it even stops!)
}

# Import the data
datCC <- read.csv(CCFile, sep = ",", colClasses = c("integer", "character", "character", "character", "integer", "integer", "integer", "integer", "integer"))
datCC$ReportDate <- as.Date(datCC$ReportDate)
datCC$WeekEnd <- as.Date(datCC$WeekEnd)
datCC$WeekStart <- as.Date(datCC$WeekStart)
print(paste("Latest reported date in CCFile:", max(datCC$ReportDate), "(should be last Wednesday)"))
# I display the last reported date but then will use the WeekEnd as the date (as it relates more to the end of the week of counts than reporting itself)

# Clean the data
# we don't need columna OBJECTID nor age_unknown (assumption here!)
datCC$OBJECTID <- NULL

# Graph A - simply the cumulative cases contacted, i.e. just the data

datCC1 <- datCC
datCC1$ReportDate <- NULL
datCC1$WeekStart <- NULL
#datCC1$totalrecords <- NULL

colnames(datCC1) <- c("WeekEnd", "1. total cases entered", "2. cases with phone #", "3. cases successfully reached", "4. cases interviewed", "4. collected 1+ contacts names")
cols2pivot <- colnames(datCC1)
cols2pivot <- cols2pivot[2:6] # we don't need "Date"
dtCC1 <- pivot_longer(data = datCC1, cols = cols2pivot, names_to = "Type", values_to = "Count")

p <- ggplot(dtCC1, aes(x = WeekEnd, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  #theme_linedraw() +
  #geom_text(aes(label=format(round(Count,0), scientific = FALSE, big.mark = ",")), vjust=-1.6, color="black", size=1) +
  labs(title = "Cumulative total of confirmed COVID-19 cases contacted in Maryland, USA (2020)",
       x = "Date (week end)",
       y = paste("Cumulative cases"),
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
p
ggsave("../figures/contacttracing-cases.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")

# Graph B - % of reached compared to total cases
# Needs: MDCOVID19 TotalCasesStatewide (maybe had downloaded it already) -->
preventMultipleDownload <- TRUE
# URL: https://data.imap.maryland.gov/datasets/mdcovid19-totalcasesstatewide/data?page=16&selectedAttribute=DATE
CPURL <- "https://opendata.arcgis.com/datasets/18582de727934249b92c52542395a3bf_0.csv"
CPFile <- "../data/cases-positives.csv"
# CP = cases positive

# Download the data
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

# Import the data
datCP <- read.csv(CPFile, sep = ",", colClasses = c("integer", "Date", "integer"))
datCP$DATE <- as.Date(datCP$DATE)
print(paste("Latest data point in CPFile:", max(datCP$DATE)))
datCP$OBJECTID <- NULL
colnames(datCP) <- c("Date", "PositiveCases")

# Partial merge by weekEnd / date
datCC2 <- merge(datCC, datCP, by.x = "WeekEnd", by.y = "Date")
datCC2$ReportDate <- NULL
datCC2$WeekStart <- NULL
datCC2$phonenumber <- NULL
datCC2$namedcontact <- NULL

# Calculate percentages
datCC2$PcRecorded <- datCC2$totalrecords / datCC2$PositiveCases * 100
datCC2$PcReached <- datCC2$reached / datCC2$PositiveCases * 100
datCC2$PcInterviewed <- datCC2$interviewed / datCC2$PositiveCases * 100

# Some cleanup of unwanted columns and renaming columns
datCC2$totalrecords <- NULL
datCC2$reached <- NULL
datCC2$interviewed <- NULL
datCC2$PositiveCases <- NULL
colnames(datCC2) <- c("WeekEnd", "Cases recorded", "Cases reached", "Cases interviewed")

# small digression: keeping the last row for the annotations
datCC2last <- tail(datCC2, n = 1)

# graph it
cols2pivot <- colnames(datCC2)
cols2pivot <- cols2pivot[2:4] # we don't need "Date"
dtCC2 <- pivot_longer(data = datCC2, cols = cols2pivot, names_to = "Type", values_to = "Percentage")

q <- ggplot(dtCC2, aes(x = WeekEnd, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  annotate("text", label = paste("Latest data:", datCC2last$WeekEnd, "\n",
                                 "% positive cases recorded:", format(round(datCC2last$`Cases recorded`, 2), nsmall = 2), "%\n",
                                 "% positive cases reached:", format(round(datCC2last$`Cases reached`, 2), nsmall = 2), "%\n",
                                 "% positive cases interviewed:", format(round(datCC2last$`Cases interviewed`, 2), nsmall = 2), "%"),
           x = datCC2last$WeekEnd - 15,
           y = 20,
           size = 3, fontface = "italic") +
  labs(title = "Cumulative metrics for successful contact tracing in Maryland, USA (2020)",
       x = "Date (week end)",
       y = paste("Percentage of positive cases"),
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
q
ggsave("../figures/contacttracing-casespc.png", plot = q, device = "png", width = plotWidth, height = plotHeight, units = "in")

# Set back some flags to normal
preventMultipleDownload <- FALSE

# Now about contacts

COURL <- "https://opendata.arcgis.com/datasets/6c5c65bb86ed43778e61653f37db12d0_0.csv"
COFile <- "../data/contacttracing-contacts.csv"
# CO = COntacts

# Download the data
if((as.Date(file.info(COFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(COURL, COFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as COFile already downloaded today")
}
if(file.exists(COFile)) {
  print(paste("COFile found, created on:", file.info(COFile)$ctime))
} else {
  stop(COFile, " does not exist") # not the best way to stop (if it even stops!)
}

# Import the data
datCO <- read.csv(COFile, sep = ",", colClasses = c("integer", "character", "character", "character", "integer", "integer", "integer", "integer"))
datCO$ReportDate <- as.Date(datCO$ReportDate)
datCO$WeekEnd <- as.Date(datCO$WeekEnd)
datCO$WeekStart <- as.Date(datCO$WeekStart)
print(paste("Latest reported date in COFile:", max(datCO$ReportDate), "(should be last Wednesday)"))
# I display the last reported date but then will use the WeekEnd as the date (as it relates more to the end of the week of counts than reporting itself)

# Clean the data
# we don't need columna OBJECTID nor age_unknown (assumption here!)
datCO$OBJECTID <- NULL

# Graph C - simply the cumulative contacts, i.e. just the data

datCO1 <- datCO
datCO1$ReportDate <- NULL
datCO1$WeekStart <- NULL

colnames(datCO1) <- c("WeekEnd", "1. total contacts recorded", "2. contacts with phone #", "3. contacts successfully reached", "4. contacts interviewed")
cols2pivot <- colnames(datCO1)
cols2pivot <- cols2pivot[2:5] # we don't need "Date"
dtCO1 <- pivot_longer(data = datCO1, cols = cols2pivot, names_to = "Type", values_to = "Count")

r <- ggplot(dtCO1, aes(x = WeekEnd, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Cumulative total of contacts of COVID-19 cases in Maryland, USA (2020)",
       x = "Date (week end)",
       y = paste("Cumulative contacts"),
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
r
ggsave("../figures/contacttracing-contacts.png", plot = r, device = "png", width = plotWidth, height = plotHeight, units = "in")

# Graph D - N times contacts (interviewed) per case recorded

# Partial merge by weekEnd / date
datCO2 <- merge(datCO, datCC, by.x = "WeekEnd", by.y = "WeekEnd", suffixes = c("_contacts", "_cases"))
datCO2$ReportDate_contacts <- NULL
datCO2$WeekStart_contacts <- NULL
datCO2$phonenumber_contacts <- NULL
datCO2$reached_contacts <- NULL
datCO2$ReportDate_cases <- NULL
datCO2$WeekStart_cases <- NULL
datCO2$phonenumber_cases <- NULL
datCO2$reached_cases <- NULL
datCO2$interviewed_cases <- NULL

# Calculate percentages
datCO2$AverageContactPerCase <- datCO2$totalrecords_contacts / datCO2$namedcontact
datCO2$AverageContactInterviewPerCase <- datCO2$interviewed_contacts / datCO2$namedcontact

# Some cleanup of unwanted columns and renaming columns
datCO2$totalrecords_contacts <- NULL
datCO2$interviewed_contacts <- NULL
datCO2$totalrecords_cases <- NULL
datCO2$namedcontact <- NULL
colnames(datCO2) <- c("WeekEnd", "Average contacts per case", "Average contacts interviewed per case")

# small digression: keeping the last row for the annotations
datCO2last <- tail(datCO2, n = 1)

# graph it
cols2pivot <- colnames(datCO2)
cols2pivot <- cols2pivot[2:3] # we don't need "Date"
dtCO2 <- pivot_longer(data = datCO2, cols = cols2pivot, names_to = "Type", values_to = "N")

s <- ggplot(dtCO2, aes(x = WeekEnd, y = N, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  annotate("text", label = paste("Latest data:", datCO2last$WeekEnd, "\n",
                                 "Average", format(round(datCO2last$`Average contacts per case`, 2), nsmall = 2), "contacts given per case\n",
                                 "Average", format(round(datCO2last$`Average contacts interviewed per case`, 2), nsmall = 2), "contacts interviewed per case"),
           x = datCO2last$WeekEnd - 7,
           y = max(datCO2last$`Average contacts per case` + 1),
           size = 3, fontface = "italic") +
  labs(title = "# Contacts per COVID-19 cases who gave contacts in Maryland, USA (2020)",
       x = "Date (week end)",
       y = paste("N per case who gave contacts"),
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
s
ggsave("../figures/contacttracing-contactspc.png", plot = s, device = "png", width = plotWidth, height = plotHeight, units = "in")
