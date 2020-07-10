# Cases & Deaths by age statewide analysis for Maryland
# Using data from the MD Data Respository: https://data.imap.maryland.gov/

# MDCOVID19 CasesByAgeDistribution
# https://data.imap.maryland.gov/datasets/mdcovid19-casesbyagedistribution
#
# MDCOVID19 ConfirmedDeathsByAgeDistribution
# https://data.imap.maryland.gov/datasets/mdcovid19-confirmeddeathsbyagedistribution
#
# not used: ProbableDeathsByAgeDistribution - https://data.imap.maryland.gov/datasets/mdcovid19-probabledeathsbyagedistribution

library(dplyr)
library(gghighlight)
library(ggplot2)
library(ggpubr) # for ggarrange
library(ggseas) # for decompose
library(ini)
library(scales) # for x-axis ticks
library(tidyr)

print("Entering age-analysis.R")

plotWidth <- 12
plotHeight <- 7 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong <- 10 # for multiple graphs: 9 (= 3 times 3) + 1
Nbreaks <- 10 # default number of breaks for trend decomposition
preventMultipleDownload <- FALSE

# Read an .ini file with point data in it
iniFile <- "../data-other-sources/pointData.ini"
pointData <- read.ini(iniFile)

# Cases by age distribution

ACURL <- "https://opendata.arcgis.com/datasets/68fbe34617cd450aa423e27692f503b0_0.csv"
ACFile <- "../data/age-cases.csv"
# AC = age cases

# Download the data
if((as.Date(file.info(ACFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(ACURL, ACFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as ACFile already downloaded today")
}
if(file.exists(ACFile)) {
  print(paste("ACFile found, created on:", file.info(ACFile)$ctime))
} else {
  stop(ACFile, " does not exist") # not the best way to stop (if it even stops!)
}

# Import the data
datAC <- read.csv(ACFile, sep = ",", colClasses = c("integer", "character", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer"))
datAC$DATE <- as.Date(datAC$DATE)
print(paste("Latest data point in ACFile:", max(datAC$DATE)))

# Clean the data
# we don't need columna OBJECTID nor age_unknown (assumption here!)
datAC$OBJECTID <- NULL
datAC$Age_Unknown <- NULL
# rename headers
colnames(datAC) <- c("Date", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

# Graph A1 - simply the cumulative cases, i.e. just the data

cols2pivot <- colnames(datAC)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtAC <- pivot_longer(data = datAC, cols = cols2pivot, names_to = "AgeGroups", values_to = "Cases", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtAC$AgeGroups <- factor(dtAC$AgeGroups , levels = c("80+", "70-79", "60-69", "50-59", "40-49", "30-39", "20-29", "10-19", "0-9"))

p <- ggplot(dtAC, aes(x = Date, y = Cases, group = AgeGroups)) +
  geom_line(aes(color = AgeGroups), lwd = 1) +
  geom_point(aes(color = AgeGroups, shape = AgeGroups)) +
  theme_linedraw() +
  labs(title = "Evolution of Coronavirus cases by age group in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative cases")
#p

# Graph A2 - number of new daily cases

# Calculate daily delta from the cumulative count
deltaF <- function(x, na.rm = FALSE) (x - lag(x, default = 0))
datAC2 <- datAC
datAC2 <- mutate_all(datAC2[2:length(datAC2)], ~ deltaF(.))
datAC2 <- cbind(datAC2, Date = datAC$Date) # this is risky (desync?) but adds Date as the last column

cols2pivot <- colnames(datAC2)
cols2pivot <- cols2pivot[1:length(cols2pivot)-1] # we don't need "Date" (but here it's at the end, thanks to cbind)

dtAC2 <- pivot_longer(data = datAC2, cols = cols2pivot, names_to = "AgeGroups", values_to = "NewCases", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtAC2$AgeGroups <- factor(dtAC2$AgeGroups , levels = c("80+", "70-79", "60-69", "50-59", "40-49", "30-39", "20-29", "10-19", "0-9"))

q <- ggplot(dtAC2, aes(x = Date, y = NewCases, group = AgeGroups)) +
  geom_line(aes(color = AgeGroups), lwd = 1) +
  geom_point(aes(color = AgeGroups, shape = AgeGroups)) +
  theme_linedraw() +
  labs(title = "Evolution of daily new Coronavirus cases by age group in Maryland, USA (2020)",
       x = "Date",
       y = "New cases each day",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
#q

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
#r
ggsave("../figures/age-cases.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")

# Graph B - section cases of the last date
# Now donw at E below

# Graph C1 - cases evolution of each age category relative to its own population

# Get the total population and get the relative cases
# Look at cases (positive tests) per 100,000 for each age category

# Get age categoris counts from the MD State (arrange from the ini file -> dataframe)

ageTotal <- data.frame("Date" = as.double(1), "0-9" = as.double(pointData[["MDAges"]][["0-9"]]),
                       "10-19" = as.double(pointData[["MDAges"]][["10-19"]]),
                       "20-29" = as.double(pointData[["MDAges"]][["20-29"]]),
                       "30-39" = as.double(pointData[["MDAges"]][["30-39"]]),
                       "40-49" = as.double(pointData[["MDAges"]][["40-49"]]),
                       "50-59" = as.double(pointData[["MDAges"]][["50-59"]]),
                       "60-69" = as.double(pointData[["MDAges"]][["60-69"]]),
                       "70-79" = as.double(pointData[["MDAges"]][["70-79"]]),
                       "80+" = as.double(pointData[["MDAges"]][["80+"]]))

dateAgeDataUpdate <- pointData$MDAges$DateAccessed

# Date is in Date, datTmp will get it in number
datTmp <- datAC
datTmp$Date <- as.integer(datTmp$Date)

matR <- as.matrix(datTmp) # transform to matrix for processing
matT <- as.matrix(ageTotal)

matX <- sweep(matR, 2, matT, '/') # simple processing
matX <- matX * 100000
matX[,1] <- matX[,1] / 100000 # well, shouldn't have multiplied for Date ...

datX <- as.data.frame(matX) # get back to dataframe
datX$Date <- as.Date(datX$Date, origin = "1970-01-01")

cols2pivot <- colnames(datX)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtX <- pivot_longer(data = datX, cols = cols2pivot, names_to = "Ages", values_to = "Cases", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtX$Ages <- factor(dtX$Ages , levels = c("80+", "70-79", "60-69", "50-59", "40-49", "30-39", "20-29", "10-19", "0-9"))

p <- ggplot(dtX, aes(x = Date, y = Cases, group = Ages)) +
  geom_line(aes(color = Ages), lwd = 1) +
  geom_point(aes(color = Ages, shape = Ages)) +
  theme_linedraw() +
  labs(title = "Evolution of COVID-19 cases relative by age in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative cases / 100,000 pop")
#p

# Graph C2 - cases evolution of each age category (contribution to 100% - not relative ot pop)

datTT <- datAC
datTT$Total <- datTT$`0-9` + datTT$`10-19` + datTT$`20-29` + datTT$`30-39` + datTT$`40-49` + datTT$`50-59` + datTT$`60-69` + datTT$`70-79` + datTT$`80+`
datTT$`Pc0-9` <- datTT$`0-9` / datTT$Total * 100
datTT$`Pc10-19` <- datTT$`10-19` / datTT$Total * 100
datTT$`Pc20-29` <- datTT$`20-29` / datTT$Total * 100
datTT$`Pc30-39` <- datTT$`30-39` / datTT$Total * 100
datTT$`Pc40-49` <- datTT$`40-49` / datTT$Total * 100
datTT$`Pc50-59` <- datTT$`50-59` / datTT$Total * 100
datTT$`Pc60-69` <- datTT$`60-69` / datTT$Total * 100
datTT$`Pc70-79` <- datTT$`70-79` / datTT$Total * 100
datTT$`Pc80+` <- datTT$`80+` / datTT$Total * 100

cols2pivot <- colnames(datTT)
# removing raw numbers
datTT[,(2:11)] <- NULL
# renaming headers
colnames(datTT) <- cols2pivot[1:10]
# small digression: keeping the last row for the annotations
datTTlast <- tail(datTT, n = 1)
# pivoting (back to the flow)
cols2pivot <- colnames(datTT)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtTT <- pivot_longer(data = datTT, cols = cols2pivot, names_to = "Ages", values_to = "Percentage", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtTT$Ages <- factor(dtTT$Ages , levels = c("80+", "70-79", "60-69", "50-59", "40-49", "30-39", "20-29", "10-19", "0-9"))

q <- ggplot(dtTT, aes(x = Date, y = Percentage, fill = Ages)) + 
  geom_area()  +
  theme_linedraw() +
  annotate("text", label = paste("20-29:", format(datTTlast$`20-29`, digits = 0), "%"),
           x = datTTlast$Date - 5,
           y = datTTlast$`0-9` + datTTlast$`10-19` + 5,
           size = 3, fontface = "italic") +
  annotate("text", label = paste("30-39:", format(datTTlast$`30-39`, digits = 0), "%"),
           x = datTTlast$Date - 5,
           y = datTTlast$`0-9` + datTTlast$`10-19` + datTTlast$`20-29` + 5,
           size = 3, fontface = "italic") +
  annotate("text", label = paste("40-49:", format(datTTlast$`40-49`, digits = 0), "%"),
           x = datTTlast$Date - 5,
           y = datTTlast$`0-9` + datTTlast$`10-19` + datTTlast$`20-29` + datTTlast$`30-39` + 5,
           size = 3, fontface = "italic") +
  annotate("text", label = paste("50-59:", format(datTTlast$`50-59`, digits = 0), "%"),
           x = datTTlast$Date - 5,
           y = datTTlast$`0-9` + datTTlast$`10-19` + datTTlast$`20-29` + datTTlast$`30-39` + datTTlast$`40-49` + 5,
           size = 3, fontface = "italic") +
  annotate("text", label = paste("60-69:", format(datTTlast$`60-69`, digits = 0), "%"),
           x = datTTlast$Date - 5,
           y = datTTlast$`0-9` + datTTlast$`10-19` + datTTlast$`20-29` + datTTlast$`30-39` + datTTlast$`40-49` + datTTlast$`50-59` + 5,
           size = 3, fontface = "italic") +
  annotate("text", label = paste("70-79:", format(datTTlast$`70-79`, digits = 0), "%"),
           x = datTTlast$Date - 5,
           y = datTTlast$`0-9` + datTTlast$`10-19` + datTTlast$`20-29` + datTTlast$`30-39` + datTTlast$`40-49` + datTTlast$`50-59` + datTTlast$`60-69` + 3,
           size = 3, fontface = "italic") +
  annotate("text", label = paste("80+:", format(datTTlast$`80+`, digits = 0), "%"),
           x = datTTlast$Date - 5,
           y = datTTlast$`0-9` + datTTlast$`10-19` + datTTlast$`20-29` + datTTlast$`30-39` + datTTlast$`40-49` + datTTlast$`50-59` + datTTlast$`60-69` + datTTlast$`70-79` + 2,
           size = 3, fontface = "italic") +
  labs(title = "Evolution of COVID-19 cases by age, relative to total in Maryland, USA (2020)",
       x = "Date",
       y = "Percentage of cases by age category",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last data update:", format(max(datAC$Date), "%b %d, %Y")))
#q

#as.Date(as.Date(sprintf("%d", max(datX$Date))
#, scientific = FALSE, big.mark = ","
#lastMaxTotalCases = max(datTT$TotalTests, na.rm = TRUE)
#lastPositivePC = if (is.na(datTT$PositivePC[nrow(datTT)])) (datTT$PositivePC[nrow(datTT) - 1]) else (datTT$PositivePC[nrow(datTT)]) # needed because sometimes last line is NA
#datTT$Date <- as.Date(sprintf("%d", datTT$Date), "%y%m%d")

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
#r
ggsave("../figures/age-cases-relative.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")






### Deaths now ...

ADURL <- "https://opendata.arcgis.com/datasets/b51451a222cd4c3d89a58c3393212cc7_0.csv"
ADFile <- "../data/age-deaths.csv"
# AD = age deaths

# Download the data
if((as.Date(file.info(ADFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(ADURL, ADFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as ADFile already downloaded today")
}
if(file.exists(ADFile)) {
  print(paste("ADFile found, created on:", file.info(ADFile)$ctime))
} else {
  stop(ADFile, " does not exist") # not the best way to stop (if it even stops!)
}

# Import the data
datAD <- read.csv(ADFile, sep = ",", colClasses = c("integer", "character", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer"))
datAD$DATE <- as.Date(datAD$DATE)
print(paste("Latest data point in ADFile:", max(datAD$DATE)))

# Clean the data
# we don't need columna OBJECTID nor age_unknown (assumption here!)
datAD$OBJECTID <- NULL
datAD$Age_Unknown <- NULL
# rename headers
colnames(datAD) <- c("Date", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

# Graph D1 - simply the cumulative deaths, i.e. just the data

cols2pivot <- colnames(datAD)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtAD <- pivot_longer(data = datAD, cols = cols2pivot, names_to = "AgeGroups", values_to = "Deaths", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtAD$AgeGroups <- factor(dtAD$AgeGroups , levels = c("80+", "70-79", "60-69", "50-59", "40-49", "30-39", "20-29", "10-19", "0-9"))

p <- ggplot(dtAD, aes(x = Date, y = Deaths, group = AgeGroups)) +
  geom_line(aes(color = AgeGroups), lwd = 1) +
  geom_point(aes(color = AgeGroups, shape = AgeGroups)) +
  theme_linedraw() +
  labs(title = "Evolution of confirmed Coronavirus deaths by age group in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative deaths")
#p

# Graph D2 - number of new daily deaths

# Calculate daily delta from the cumulative count
deltaF <- function(x, na.rm = FALSE) (x - lag(x, default = 0))
datAD2 <- datAD
datAD2 <- mutate_all(datAD2[2:length(datAD2)], ~ deltaF(.))
datAD2 <- cbind(datAD2, Date = datAD$Date) # this is risky (desync?) but adds Date as the last column

cols2pivot <- colnames(datAD2)
cols2pivot <- cols2pivot[1:length(cols2pivot)-1] # we don't need "Date" (but here it's at the end, thanks to cbind)

dtAD2 <- pivot_longer(data = datAD2, cols = cols2pivot, names_to = "AgeGroups", values_to = "NewDeaths", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtAD2$AgeGroups <- factor(dtAD2$AgeGroups , levels = c("80+", "70-79", "60-69", "50-59", "40-49", "30-39", "20-29", "10-19", "0-9"))

q <- ggplot(dtAD2, aes(x = Date, y = NewDeaths, group = AgeGroups)) +
  geom_line(aes(color = AgeGroups), lwd = 1) +
  geom_point(aes(color = AgeGroups, shape = AgeGroups)) +
  theme_linedraw() +
  labs(title = "Evolution of daily new confirmed Coronavirus deaths by age group in Maryland, USA (2020)",
       x = "Date",
       y = "New confirmed deaths each day",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
#q

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
#r
ggsave("../figures/age-deaths.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")



# Graph E - section deaths of the last date

# Graph E/1 - section CASES of the last date

datAC3 <- tail(datAC, n = 1) # assumes datAC ordered by Date
cols2pivot <- colnames(datAC3)
cols2pivot <- cols2pivot[2:length(cols2pivot)]

dtAC3 <- pivot_longer(data = datAC3, cols = cols2pivot, names_to = "AgeGroups", values_to = "Cases", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtAC3$AgeGroups <- factor(dtAC3$AgeGroups , levels = c("80+", "70-79", "60-69", "50-59", "40-49", "30-39", "20-29", "10-19", "0-9"))

p <- ggplot(dtAC3, aes(x = AgeGroups, y = Cases)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  #scale_fill_manual(name = "Legend", labels = c("Confirmed deaths", "Probable deaths", "Cases"), values = c("#000000", "#999999", "steelblue")) +
  theme_linedraw() +
  #geom_text(aes(label=Count), vjust=1.6, color="white", size=3.5)+
  labs(title = "Age distribution of all Coronavirus cases in Maryland, USA (2020)",
       color = "Legend", 
       x = "Age groups (years)",
       y = "All cases")
#p

# Deaths now ...

datAD3 <- tail(datAD, n = 1) # assumes datAD ordered by Date
cols2pivot <- colnames(datAD3)
cols2pivot <- cols2pivot[2:length(cols2pivot)]

dtAD3 <- pivot_longer(data = datAD3, cols = cols2pivot, names_to = "AgeGroups", values_to = "Deaths", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtAD3$AgeGroups <- factor(dtAD3$AgeGroups , levels = c("80+", "70-79", "60-69", "50-59", "40-49", "30-39", "20-29", "10-19", "0-9"))

q <- ggplot(dtAD3, aes(x = AgeGroups, y = Deaths)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  #scale_fill_manual(name = "Legend", labels = c("Confirmed deaths", "Probable deaths", "Cases"), values = c("#000000", "#999999", "steelblue")) +
  theme_linedraw() +
  #geom_text(aes(label=Count), vjust=1.6, color="white", size=3.5)+
  labs(title = "Age distribution of all confirmed Coronavirus deaths in Maryland, USA (2020)",
       color = "Legend", 
       x = "Age groups (years)",
       y = "All deaths",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
#q

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
#r
ggsave("../figures/age-section.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")



# Graph F1 - deaths evolution of each age category relative to its own population

# Get the total population and get the relative cases
# Look at deaths per 100,000 for each age category

# Get age categories counts from the MD State (arrange from the ini file -> dataframe)
# Already done above: use variable ageTotal
# Already know dateAgeDataUpdate <- pointData$MDAges$DateAccessed

# Date is in Date, datTmp will get it in number
datTmp <- datAD
datTmp$Date <- as.integer(datTmp$Date)

matR <- as.matrix(datTmp) # transform to matrix for processing
matT <- as.matrix(ageTotal)

matX <- sweep(matR, 2, matT, '/') # simple processing
matX <- matX * 100000
matX[,1] <- matX[,1] / 100000 # well, shouldn't have multiplied for Date ...

datX <- as.data.frame(matX) # get back to dataframe
datX$Date <- as.Date(datX$Date, origin = "1970-01-01")

cols2pivot <- colnames(datX)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtX <- pivot_longer(data = datX, cols = cols2pivot, names_to = "Ages", values_to = "Deaths", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtX$Ages <- factor(dtX$Ages , levels = c("80+", "70-79", "60-69", "50-59", "40-49", "30-39", "20-29", "10-19", "0-9"))

p <- ggplot(dtX, aes(x = Date, y = Deaths, group = Ages)) +
  geom_line(aes(color = Ages), lwd = 1) +
  geom_point(aes(color = Ages, shape = Ages)) +
  theme_linedraw() +
  labs(title = "Evolution of confirmed COVID-19 deaths relative by age in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative deaths / 100,000 pop")
#p


# Graph F2 - deaths evolution of each age category (contribution to 100% - not relative ot pop)

datTT <- datAD
datTT$Total <- datTT$`0-9` + datTT$`10-19` + datTT$`20-29` + datTT$`30-39` + datTT$`40-49` + datTT$`50-59` + datTT$`60-69` + datTT$`70-79` + datTT$`80+`
datTT$`Pc0-9` <- datTT$`0-9` / datTT$Total * 100
datTT$`Pc10-19` <- datTT$`10-19` / datTT$Total * 100
datTT$`Pc20-29` <- datTT$`20-29` / datTT$Total * 100
datTT$`Pc30-39` <- datTT$`30-39` / datTT$Total * 100
datTT$`Pc40-49` <- datTT$`40-49` / datTT$Total * 100
datTT$`Pc50-59` <- datTT$`50-59` / datTT$Total * 100
datTT$`Pc60-69` <- datTT$`60-69` / datTT$Total * 100
datTT$`Pc70-79` <- datTT$`70-79` / datTT$Total * 100
datTT$`Pc80+` <- datTT$`80+` / datTT$Total * 100

cols2pivot <- colnames(datTT)
# removing raw numbers
datTT[,(2:11)] <- NULL
# renaming headers
colnames(datTT) <- cols2pivot[1:10]
# small digression: keeping the last row for the annotations
datTTlast <- tail(datTT, n = 1)
# pivoting (back to the flow)
cols2pivot <- colnames(datTT)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtTT <- pivot_longer(data = datTT, cols = cols2pivot, names_to = "Ages", values_to = "Percentage", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtTT$Ages <- factor(dtTT$Ages , levels = c("80+", "70-79", "60-69", "50-59", "40-49", "30-39", "20-29", "10-19", "0-9"))

q <- ggplot(dtTT, aes(x = Date, y = Percentage, fill = Ages)) + 
  geom_area()  +
  theme_linedraw() +
  # annotate("text", label = paste("20-29:", format(datTTlast$`20-29`, digits = 0), "%"),
  #          x = datTTlast$Date - 5,
  #          y = datTTlast$`0-9` + datTTlast$`10-19` + 5,
  #          size = 3, fontface = "italic") +
  # annotate("text", label = paste("30-39:", format(datTTlast$`30-39`, digits = 0), "%"),
  #          x = datTTlast$Date - 5,
  #          y = datTTlast$`0-9` + datTTlast$`10-19` + datTTlast$`20-29` + 5,
  #          size = 3, fontface = "italic") +
  # annotate("text", label = paste("40-49:", format(datTTlast$`40-49`, digits = 0), "%"),
  #          x = datTTlast$Date - 5,
  #          y = datTTlast$`0-9` + datTTlast$`10-19` + datTTlast$`20-29` + datTTlast$`30-39` + 5,
  #          size = 3, fontface = "italic") +
  annotate("text", label = paste("50-59:", format(datTTlast$`50-59`, digits = 0), "%"),
           x = datTTlast$Date - 5,
           y = datTTlast$`0-9` + datTTlast$`10-19` + datTTlast$`20-29` + datTTlast$`30-39` + datTTlast$`40-49` + 2,
           size = 3, fontface = "italic") +
  annotate("text", label = paste("60-69:", format(datTTlast$`60-69`, digits = 0), "%"),
           x = datTTlast$Date - 5,
           y = datTTlast$`0-9` + datTTlast$`10-19` + datTTlast$`20-29` + datTTlast$`30-39` + datTTlast$`40-49` + datTTlast$`50-59` + 3,
           size = 3, fontface = "italic") +
  annotate("text", label = paste("70-79:", format(datTTlast$`70-79`, digits = 0), "%"),
           x = datTTlast$Date - 5,
           y = datTTlast$`0-9` + datTTlast$`10-19` + datTTlast$`20-29` + datTTlast$`30-39` + datTTlast$`40-49` + datTTlast$`50-59` + datTTlast$`60-69` + 5,
           size = 3, fontface = "italic") +
  annotate("text", label = paste("80+:", format(datTTlast$`80+`, digits = 0), "%"),
           x = datTTlast$Date - 5,
           y = datTTlast$`0-9` + datTTlast$`10-19` + datTTlast$`20-29` + datTTlast$`30-39` + datTTlast$`40-49` + datTTlast$`50-59` + datTTlast$`60-69` + datTTlast$`70-79` + 5,
           size = 3, fontface = "italic") +
  labs(title = "Evolution of confirmed COVID-19 deaths by age, relative to total in Maryland, USA (2020)",
       x = "Date",
       y = "Percentage of deaths by age category",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last data update:", format(max(datAC$Date), "%b %d, %Y")))
#q

#as.Date(as.Date(sprintf("%d", max(datX$Date))
#, scientific = FALSE, big.mark = ","
#lastMaxTotalCases = max(datTT$TotalTests, na.rm = TRUE)
#lastPositivePC = if (is.na(datTT$PositivePC[nrow(datTT)])) (datTT$PositivePC[nrow(datTT) - 1]) else (datTT$PositivePC[nrow(datTT)]) # needed because sometimes last line is NA
#datTT$Date <- as.Date(sprintf("%d", datTT$Date), "%y%m%d")

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
#r
ggsave("../figures/age-deaths-relative.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")
