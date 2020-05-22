# Produce graphs of Coronavirus cases in Maryland Counties (USA) - relative for Races
# Source of data: https://coronavirus.maryland.gov/ (updated daily at ~ 10am)
# Rationale for doing this: have some fun
# Explanations: https://jepoirrier.org/mdcovid19/ 
# Data repository: https://github.com/jepoirrier/MD-coronavirus

library(dplyr)
library(gghighlight)
library(ggplot2)
library(ggpubr)
library(ini)
library(tidyr)
#library(viridis)

plotWidth <- 12
plotHeight <- 7 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong <- 10 # for multiple graphs: 9 (= 3 times 3) + 1

# Read an .ini file with point data in it
iniFile <- "data-other-sources/pointData.ini"
pointData <- read.ini(iniFile)

# *********** RELATIVE CASES **********

# Read data for race distribution
MDRaceFile <- 'MD-coronavirus-byrace.txt'
MDRaceNCols <- 19
datRace <- read.csv(MDRaceFile, sep = " ", colClasses = c(rep("numeric", MDRaceNCols)))

# Look at cases (positive tests) per 100,000 for each race

# Get race counts from the MD State (arrange from the ini file -> dataframe)

raceTotal <- data.frame("Date" = as.double(1), "AfricanAmerican" = as.double(pointData$MDRaces$AfricanAmerican),
                        "Asian" = as.double(pointData$MDRaces$Asian),
                        "White" = as.double(pointData$MDRaces$White),
                        "Hispanic" = as.double(pointData$MDRaces$Hispanic),
                        "Other" = as.double(pointData$MDRaces$Other),
                        "DataNotAvailable" = as.double(1),
                        "DAfricanAmerican" = as.double(pointData$MDRaces$AfricanAmerican),
                        "DAsian" = as.double(pointData$MDRaces$Asian),
                        "DWhite" = as.double(pointData$MDRaces$White),
                        "DHispanic" = as.double(pointData$MDRaces$Hispanic),
                        "DOther" = as.double(pointData$MDRaces$Other),
                        "DDataNotAvailable" = as.double(1),
                        "PDAfricanAmerican" = as.double(pointData$MDRaces$AfricanAmerican), # I'm not using probably deaths but just in case
                        "PDAsian" = as.double(pointData$MDRaces$Asian),
                        "PDWhite" = as.double(pointData$MDRaces$White),
                        "PDHispanic" = as.double(pointData$MDRaces$Hispanic),
                        "PDOther" = as.double(pointData$MDRaces$Other),
                        "PDDataNotAvailable" = as.double(1))

dateRacerDataUpdate <- pointData$MDRaces$DateAccessed

matR <- as.matrix(datRace) # transform to matrix for processing
matT <- as.matrix(raceTotal)

matX <- sweep(matR, 2, matT, '/') # simple processing
matX <- matX * 100000
matX[,1] <- matX[,1] / 100000 # well, shouldn't have multiplied for Date ...

datX <- as.data.frame(matX) # get back to dataframe

### Graph CASES

# 1 - crude cases 

limitCasesCol <- 6 # just cases, no NA, no deaths, no probable deaths --> TODO
dat <- datRace[1:limitCasesCol]

cols2pivot <- colnames(dat)
cols2pivot <- c("Date", "African American", "Asian", "White", "Hispanic", "Other")
colnames(dat) <- cols2pivot
cols2pivot <- cols2pivot[2:limitCasesCol] # we don't need "Date"

#cols2pivot <- colnames(dat)
dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "Races", values_to = "Tests", values_drop_na = TRUE)
dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")

p <- ggplot(dt, aes(x = Date, y = Tests, group = Races)) +
  geom_line(aes(color = Races), lwd = 1) +
  geom_point(aes(color = Races, shape = Races)) +
  theme_linedraw() +
  labs(title = "Evolution of cumulative Coronavirus cases by race in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative cases")

# 2 - relative cases

limitCasesCol <- 6 # just cases, no deaths, no probable deaths --> TODO
dat <- datX[1:limitCasesCol]

cols2pivot <- colnames(dat)
cols2pivot <- c("Date", "African American", "Asian", "White", "Hispanic", "Other")
colnames(dat) <- cols2pivot
cols2pivot <- cols2pivot[2:limitCasesCol] # we don't need "Date"

#cols2pivot <- colnames(dat)
dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "Races", values_to = "Tests", values_drop_na = TRUE)
dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")

q <- ggplot(dt, aes(x = Date, y = Tests, group = Races)) +
  geom_line(aes(color = Races), lwd = 1) +
  geom_point(aes(color = Races, shape = Races)) +
  theme_linedraw() +
  labs(title = "Evolution of COVID-19 cases relative by race in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative cases / 100,000 pop",
       caption = paste("Races not available are not on the graph\nData from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y")))

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")

ggsave("figures/MD-COVID19-race-casesRel.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")


### Graph DEATHS

# 1 - crude deaths

cols2keep <- c(1, 8:12)
dat <- datRace[, cols2keep]

cols2pivot <- colnames(dat)
cols2pivot <- c("Date", "African American", "Asian", "White", "Hispanic", "Other")
colnames(dat) <- cols2pivot
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

#cols2pivot <- colnames(dat)
dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "Races", values_to = "Deaths", values_drop_na = TRUE)
dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")

p <- ggplot(dt, aes(x = Date, y = Deaths, group = Races)) +
  geom_line(aes(color = Races), lwd = 1) +
  geom_point(aes(color = Races, shape = Races)) +
  theme_linedraw() +
  labs(title = "Evolution of cumulative Coronavirus deaths by race in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative number of deaths")

# Then build the graph for the cases / 100,000 per race

cols2keep <- c(1, 8:12)
dat <- datX[, cols2keep]

cols2pivot <- colnames(dat)
cols2pivot <- c("Date", "African American", "Asian", "White", "Hispanic", "Other")
colnames(dat) <- cols2pivot
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

#cols2pivot <- colnames(dat)
dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "Races", values_to = "Deaths", values_drop_na = TRUE)
dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")

q <- ggplot(dt, aes(x = Date, y = Deaths, group = Races)) +
  geom_line(aes(color = Races), lwd = 1) +
  geom_point(aes(color = Races, shape = Races)) +
  theme_linedraw() +
  labs(title = "Evolution of COVID-19-specific death rate by race in Maryland, USA (2020)",
       x = "Date",
       y = "COVID-19-specific death rate\n(# deaths / 100,000 pop)",
       caption = paste("Races not available are not on graph\nData from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y")))

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")

ggsave("figures/MD-COVID19-race-deathsRel.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")
