# Cases & Deaths by race and ethnicity analysis of data from Maryland
# Using data from the MD Data Respository: https://data.imap.maryland.gov/

# WORK IN PROGRESS!

# MDCOVID19 CasesByRaceAndEthnicityDistribution
# https://data.imap.maryland.gov/datasets/mdcovid19-casesbyraceandethnicitydistribution
# Download URL to check if not moving every day
#
# MDCOVID19 ConfirmedDeathsByRaceAndEthnicityDistribution
# https://data.imap.maryland.gov/datasets/mdcovid19-confirmeddeathsbyraceandethnicitydistribution
# Download URL to check if not moving every day

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

# Read an .ini file with point data in it
iniFile <- "../data-other-sources/pointData.ini"
pointData <- read.ini(iniFile)

# CUMULATIVE cases confirmed
# RC = race cases
RCURL <- "https://opendata.arcgis.com/datasets/2362c46380aa48f9a18f88571d2090bb_0.csv"
RCFile <- "../data/races-cases.csv"

# Download the data
if((as.Date(file.info(RCFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(RCURL, RCFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as RCFile already downloaded today")
}
if(file.exists(RCFile)) {
  print(paste("RCFile found, created on:", file.info(RCFile)$ctime))
} else {
  stop(RCFile, " does not exist") # not the best way to stop (if it even stops!)
}

# Import the case data
datRC <- read.csv(RCFile, sep = ",", colClasses = c("integer", "Date", "integer", "integer", "integer", "integer", "integer", "integer"))
print(paste("Latest data point in RCFile:", max(datRC$DATE)))

# Clean the data
# we don't need column OBJECTID
datRC$OBJECTID <- NULL
# rename headers
colnames(datRC) <- c("Date", "African American", "White", "Hispanic", "Asian", "Others", "Not Available")

# Now get the total population and get the relative cases
# Look at cases (positive tests) per 100,000 for each race

# Get race counts from the MD State (arrange from the ini file -> dataframe)

raceTotal <- data.frame("Date" = as.double(1), "African American" = as.double(pointData$MDRaces$AfricanAmerican),
                        "White" = as.double(pointData$MDRaces$White),
                        "Hispanic" = as.double(pointData$MDRaces$Hispanic),
                        "Asian" = as.double(pointData$MDRaces$Asian),
                        "Others" = as.double(pointData$MDRaces$Other),
                        "Not Available" = as.double(1000000000)) # NAs will be at ~ 0

dateRacerDataUpdate <- pointData$MDRaces$DateAccessed

# Date is in Date, datTmp will get it in number
datTmp <- datRC
datTmp$Date <- as.integer(datTmp$Date)

matR <- as.matrix(datTmp) # transform to matrix for processing
matT <- as.matrix(raceTotal)

matX <- sweep(matR, 2, matT, '/') # simple processing
matX <- matX * 100000
matX[,1] <- matX[,1] / 100000 # well, shouldn't have multiplied for Date ...

datX <- as.data.frame(matX) # get back to dataframe
datX$Date <- as.Date(datX$Date, origin = "1970-01-01")

### Graph CASES

# 1 - crude cases 

cols2pivot <- colnames(datRC)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtRC <- pivot_longer(data = datRC, cols = cols2pivot, names_to = "Races", values_to = "Tests", values_drop_na = TRUE)

p <- ggplot(dtRC, aes(x = Date, y = Tests, group = Races)) +
  geom_line(aes(color = Races), lwd = 1) +
  geom_point(aes(color = Races, shape = Races)) +
  theme_linedraw() +
  labs(title = "Evolution of cumulative Coronavirus cases by race in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative cases")
p

# 2 - relative cases

cols2pivot <- colnames(datX)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtX <- pivot_longer(data = datX, cols = cols2pivot, names_to = "Races", values_to = "Tests", values_drop_na = TRUE)

q <- ggplot(dtX, aes(x = Date, y = Tests, group = Races)) +
  geom_line(aes(color = Races), lwd = 1) +
  geom_point(aes(color = Races, shape = Races)) +
  theme_linedraw() +
  labs(title = "Evolution of COVID-19 cases relative by race in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative cases / 100,000 pop",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(max(datRC$Date), "%b %d, %Y")))
q
r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")

ggsave("../figures/races-cases.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")




# CUMULATIVE deaths confirmed
# RD = race deaths
RDURL <- "https://opendata.arcgis.com/datasets/312715a843064ef18879eb726f64c63a_0.csv"
RDFile <- "../data/races-deaths.csv"

# Download the data
if((as.Date(file.info(RDFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(RDURL, RDFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as RDFile already downloaded today")
}
if(file.exists(RDFile)) {
  print(paste("RDFile found, created on:", file.info(RDFile)$ctime))
} else {
  stop(RDFile, " does not exist") # not the best way to stop (if it even stops!)
}

# Import the case data
datRD <- read.csv(RDFile, sep = ",", colClasses = c("integer", "Date", "integer", "integer", "integer", "integer", "integer", "integer"))
print(paste("Latest data point in RDFile:", max(datRD$DATE)))

# Clean the data
# we don't need column OBJECTID
datRD$OBJECTID <- NULL
# rename headers
colnames(datRD) <- c("Date", "African American", "White", "Hispanic", "Asian", "Others", "Not Available")

# Now look at deaths per 100,000 for each race

# Date is in Date, datTmp will get it in number
datTmp <- datRD
datTmp$Date <- as.integer(datTmp$Date)

matR <- as.matrix(datTmp) # transform to matrix for processing
matT <- as.matrix(raceTotal)

matX <- sweep(matR, 2, matT, '/') # simple processing
matX <- matX * 100000
matX[,1] <- matX[,1] / 100000 # well, shouldn't have multiplied for Date ...

datX <- as.data.frame(matX) # get back to dataframe
datX$Date <- as.Date(datX$Date, origin = "1970-01-01")

### Graph DEATHS

# 1 - crude deaths 

cols2pivot <- colnames(datRD)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtRD <- pivot_longer(data = datRD, cols = cols2pivot, names_to = "Races", values_to = "Deaths", values_drop_na = TRUE)

r <- ggplot(dtRD, aes(x = Date, y = Deaths, group = Races)) +
  geom_line(aes(color = Races), lwd = 1) +
  geom_point(aes(color = Races, shape = Races)) +
  theme_linedraw() +
  labs(title = "Evolution of cumulative Coronavirus deaths by race in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative number of deaths")
r

# 2 - relative deaths

cols2pivot <- colnames(datX)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtX <- pivot_longer(data = datX, cols = cols2pivot, names_to = "Races", values_to = "Deaths", values_drop_na = TRUE)

s <- ggplot(dtX, aes(x = Date, y = Deaths, group = Races)) +
  geom_line(aes(color = Races), lwd = 1) +
  geom_point(aes(color = Races, shape = Races)) +
  theme_linedraw() +
  labs(title = "Evolution of COVID-19-specific death rate by race in Maryland, USA (2020)",
       x = "Date",
       y = "COVID-19-specific death rate\n(# deaths / 100,000 pop)",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(max(datRD$Date), "%b %d, %Y")))
s
t <- ggarrange(r, s, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")

ggsave("../figures/races-deaths.png", plot = t, device = "png", width = plotWidth, height = plotHeightLong, units = "in")



### section w/ cumulative data

dtRCS <- dtRC[(nrow(dtRC)-5):(nrow(dtRC)),]
dtRCS$Races <- factor(dtRCS$Races, levels = c("African American", "White", "Hispanic", "Asian", "Other", "Not Available"))

p <- ggplot(dtRCS, aes(x = Races, y = Tests)) +
  geom_bar(stat="identity", fill="orange") +
  theme_linedraw() +
  geom_text(aes(label=Tests), vjust=1.6, color="black", size=3.5) +
  labs(title = "Race distribution of Coronavirus cases in Maryland, USA (2020)",
       x = "Races",
       y = "Total cases")
p

dtRDS <- dtRD[(nrow(dtRD)-5):(nrow(dtRD)),]
dtRDS$Races <- factor(dtRDS$Races, levels = c("African American", "White", "Hispanic", "Asian", "Other", "Not Available"))

q <- ggplot(dtRDS, aes(x = Races, y = Deaths)) +
  geom_bar(stat="identity", fill="#CCCCCC") +
  theme_linedraw() +
  geom_text(aes(label=Deaths), vjust=1.6, color="black", size=3.5) +
  labs(title = "Race distribution of Coronavirus confirmed deaths in Maryland, USA (2020)",
       x = "Races",
       y = "Total deaths",
  caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(max(datRD$Date), "%b %d, %Y")))
q

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2)

ggsave("../figures/races-section.png", plot = r, device = "png", width = plotWidth, height = plotHeight, units = "in")
