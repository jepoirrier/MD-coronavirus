# Cases & Deaths by gender statewide analysis for Maryland
# Using data from the MD Data Respository: https://data.imap.maryland.gov/

# MDCOVID19 CasesByGenderDistribution
# https://data.imap.maryland.gov/datasets/mdcovid19-casesbygenderdistribution
#
# MDCOVID19 ConfirmedDeathsByGenderDistribution
# https://data.imap.maryland.gov/datasets/mdcovid19-confirmeddeathsbygenderdistribution

library(dplyr)
library(gghighlight)
library(ggplot2)
library(ggpubr) # for ggarrange
library(ggseas) # for decompose
library(ini)
library(scales) # for x-axis ticks
library(tidyr)

print("Entering gender-analysis.R")

plotWidth <- 12
plotHeight <- 7 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong <- 10 # for multiple graphs: 9 (= 3 times 3) + 1
Nbreaks <- 10 # default number of breaks for trend decomposition
preventMultipleDownload <- FALSE

# Read an .ini file with point data in it
iniFile <- "../data-other-sources/pointData.ini"
pointData <- read.ini(iniFile)

# Cases by age distribution

GCURL <- "https://opendata.arcgis.com/datasets/4719f38f874c4e4fb16b7955317f7f9c_0.csv"
GCFile <- "../data/gender-cases.csv"
# GC = gender cases

# Download the data
if((as.Date(file.info(GCFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(GCURL, GCFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as GCFile already downloaded today")
}
if(file.exists(GCFile)) {
  print(paste("GCFile found, created on:", file.info(GCFile)$ctime))
} else {
  stop(GCFile, " does not exist") # not the best way to stop (if it even stops!)
}

# Import the data
datGC <- read.csv(GCFile, sep = ",", colClasses = c("integer", "Date", "integer", "integer"))
datGC$DATE <- as.Date(datGC$DATE)
print(paste("Latest data point in GCFile:", max(datGC$DATE)))

# Clean the data
# we don't need columna OBJECTID nor age_unknown (assumption here!)
datGC$OBJECTID <- NULL
datGC$Unknown <- NULL
# rename headers
colnames(datGC) <- c("Date", "Male", "Female")

# Graph A1 - simply the cumulative cases, i.e. just the data

cols2pivot <- colnames(datGC)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtGC <- pivot_longer(data = datGC, cols = cols2pivot, names_to = "Gender", values_to = "Cases", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtGC$Gender <- factor(dtGC$Gender , levels = c("Female", "Male"))

p <- ggplot(dtGC, aes(x = Date, y = Cases, group = Gender)) +
  geom_line(aes(color = Gender), lwd = 1) +
  geom_point(aes(color = Gender, shape = Gender)) +
  theme_linedraw() +
  labs(title = "Evolution of Coronavirus cases by gender in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative cases")
#p

# Graph A2 - number of new daily cases

# Calculate daily delta from the cumulative count
deltaF <- function(x, na.rm = FALSE) (x - lag(x, default = 0))
datGC2 <- datGC
datGC2 <- mutate_all(datGC2[2:length(datGC2)], ~ deltaF(.))
datGC2 <- cbind(datGC2, Date = datGC$Date) # this is risky (desync?) but adds Date as the last column

cols2pivot <- colnames(datGC2)
cols2pivot <- cols2pivot[1:length(cols2pivot)-1] # we don't need "Date" (but here it's at the end, thanks to cbind)

dtGC2 <- pivot_longer(data = datGC2, cols = cols2pivot, names_to = "Gender", values_to = "NewCases", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtGC2$Gender <- factor(dtGC2$Gender , levels = c("Female", "Male"))

q <- ggplot(dtGC2, aes(x = Date, y = NewCases, group = Gender)) +
  geom_line(aes(color = Gender), lwd = 1) +
  geom_point(aes(color = Gender, shape = Gender)) +
  theme_linedraw() +
  labs(title = "Evolution of daily new Coronavirus cases by gender in Maryland, USA (2020)",
       x = "Date",
       y = "New cases each day",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
#q

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
#r
ggsave("../figures/gender-cases.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")

# Graph B - section cases of the last date
# Now donw at E below

# Graph C1 - cases evolution of each gender relative to its own population

# Get the total population and get the relative cases
# Look at cases (positive tests) per 100,000 for each gender

# Get gender counts from the MD State (arrange from the ini file -> dataframe)

genderTotal <- data.frame("Date" = as.double(1),
                       "Male" = as.double(pointData[["MDPopulation"]][["Male"]]),
                       "Female" = as.double(pointData[["MDPopulation"]][["Female"]]))

dateGenderDataUpdate <- pointData$MDPopulation$DateAccessed

# Date is in Date, datTmp will get it in number
datTmp <- datGC
datTmp$Date <- as.integer(datTmp$Date)

matR <- as.matrix(datTmp) # transform to matrix for processing
matT <- as.matrix(genderTotal)

matX <- sweep(matR, 2, matT, '/') # simple processing
matX <- matX * 100000
matX[,1] <- matX[,1] / 100000 # well, shouldn't have multiplied for Date ...

datX <- as.data.frame(matX) # get back to dataframe
datX$Date <- as.Date(datX$Date, origin = "1970-01-01")

cols2pivot <- colnames(datX)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtX <- pivot_longer(data = datX, cols = cols2pivot, names_to = "Gender", values_to = "Cases", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtX$Gender <- factor(dtX$Gender , levels = c("Female", "Male"))

p <- ggplot(dtX, aes(x = Date, y = Cases, group = Gender)) +
  geom_line(aes(color = Gender), lwd = 1) +
  geom_point(aes(color = Gender, shape = Gender)) +
  theme_linedraw() +
  labs(title = "Evolution of COVID-19 cases relative by gender in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative cases / 100,000 pop")
#p

# Graph C2 - cases evolution of each gender (contribution to 100% - not relative ot pop)

datTT <- datGC
datTT$Total <- datTT$Male + datTT$Female
datTT$`PcFemale` <- datTT$`Female` / datTT$Total * 100
datTT$`PcMale` <- datTT$`Male` / datTT$Total * 100

cols2pivot <- colnames(datTT)
# removing raw numbers
datTT[,(2:4)] <- NULL
# renaming headers
colnames(datTT) <- c("Date", "Female", "Male")
# small digression: keeping the last row for the annotations
datTTlast <- tail(datTT, n = 1)
# pivoting (back to the flow)
cols2pivot <- colnames(datTT)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtTT <- pivot_longer(data = datTT, cols = cols2pivot, names_to = "Gender", values_to = "Percentage", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtTT$Gender <- factor(dtTT$Gender , levels = c("Female", "Male"))

q <- ggplot(dtTT, aes(x = Date, y = Percentage, fill = Gender)) + 
  geom_area()  +
  theme_linedraw() +
  annotate("text", label = paste("Male:", format(datTTlast$Male, digits = 0), "%"),
           x = datTTlast$Date - 5,
           y = 5,
           size = 3, fontface = "italic") +
  annotate("text", label = paste("Female:", format(datTTlast$Female, digits = 0), "%"),
           x = datTTlast$Date - 5,
           y = datTTlast$Male + 5,
           size = 3, fontface = "italic") +
  labs(title = "Evolution of COVID-19 cases by gender, relative to total in Maryland, USA (2020)",
       x = "Date",
       y = "Percentage of cases by gender",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last data update:", format(max(datGC$Date), "%b %d, %Y")))
#q

#as.Date(as.Date(sprintf("%d", max(datX$Date))
#, scientific = FALSE, big.mark = ","
#lastMaxTotalCases = max(datTT$TotalTests, na.rm = TRUE)
#lastPositivePC = if (is.na(datTT$PositivePC[nrow(datTT)])) (datTT$PositivePC[nrow(datTT) - 1]) else (datTT$PositivePC[nrow(datTT)]) # needed because sometimes last line is NA
#datTT$Date <- as.Date(sprintf("%d", datTT$Date), "%y%m%d")

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
#r
ggsave("../figures/gender-cases-relative.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")




### Deaths now ...

GDURL <- "https://opendata.arcgis.com/datasets/4245fa848a7348dcb8733b93f3f45d7d_0.csv"
GDFile <- "../data/gender-deaths.csv"
# GD = gender deaths

# Download the data
if((as.Date(file.info(GDFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(GDURL, GDFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as GDFile already downloaded today")
}
if(file.exists(GDFile)) {
  print(paste("GDFile found, created on:", file.info(GDFile)$ctime))
} else {
  stop(GDFile, " does not exist") # not the best way to stop (if it even stops!)
}

# Import the data
datGD <- read.csv(GDFile, sep = ",", colClasses = c("integer", "Date", "integer", "integer"))
datGD$DATE <- as.Date(datGD$DATE)
print(paste("Latest data point in GDFile:", max(datGD$DATE)))

# Clean the data
# we don't need columna OBJECTID nor age_unknown (assumption here!)
datGD$OBJECTID <- NULL
datGD$Unknown <- NULL
# rename headers
colnames(datGD) <- c("Date", "Male", "Female")

# Graph D1 - simply the cumulative deaths, i.e. just the data

cols2pivot <- colnames(datGD)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtGD <- pivot_longer(data = datGD, cols = cols2pivot, names_to = "Gender", values_to = "Deaths", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtGD$Gender <- factor(dtGD$Gender , levels = c("Female", "Male"))

p <- ggplot(dtGD, aes(x = Date, y = Deaths, group = Gender)) +
  geom_line(aes(color = Gender), lwd = 1) +
  geom_point(aes(color = Gender, shape = Gender)) +
  theme_linedraw() +
  labs(title = "Evolution of confirmed Coronavirus deaths by gender in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative deaths")
#p

# Graph D2 - number of new daily deaths

# Calculate daily delta from the cumulative count
deltaF <- function(x, na.rm = FALSE) (x - lag(x, default = 0))
datGD2 <- datGD
datGD2 <- mutate_all(datGD2[2:length(datGD2)], ~ deltaF(.))
datGD2 <- cbind(datGD2, Date = datGD$Date) # this is risky (desync?) but adds Date as the last column

cols2pivot <- colnames(datGD2)
cols2pivot <- cols2pivot[1:length(cols2pivot)-1] # we don't need "Date" (but here it's at the end, thanks to cbind)

dtGD2 <- pivot_longer(data = datGD2, cols = cols2pivot, names_to = "Gender", values_to = "NewDeaths", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtGD2$Gender <- factor(dtGD2$Gender , levels = c("Female", "Male"))

q <- ggplot(dtGD2, aes(x = Date, y = NewDeaths, group = Gender)) +
  geom_line(aes(color = Gender), lwd = 1) +
  geom_point(aes(color = Gender, shape = Gender)) +
  theme_linedraw() +
  labs(title = "Evolution of daily new confirmed Coronavirus deaths by gender in Maryland, USA (2020)",
       x = "Date",
       y = "New confirmed deaths each day",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
#q

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
#r
ggsave("../figures/gender-deaths.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")



# Graph E - section deaths of the last date

# Graph E/1 - section CASES of the last date

datGC3 <- tail(datGC, n = 1) # assumes datAC ordered by Date
cols2pivot <- colnames(datGC3)
cols2pivot <- cols2pivot[2:length(cols2pivot)]

dtGC3 <- pivot_longer(data = datGC3, cols = cols2pivot, names_to = "Gender", values_to = "Cases", values_drop_na = TRUE)
dtGC3$Gender <- factor(dtGC3$Gender , levels = c("Female", "Male"))


p <- ggplot(dtGC3, aes(x = Gender, y = Cases)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  #scale_fill_manual(name = "Legend", labels = c("Confirmed deaths", "Probable deaths", "Cases"), values = c("#000000", "#999999", "steelblue")) +
  theme_linedraw()+
  geom_text(aes(label=format(Cases, scientific = FALSE, big.mark = ",")), vjust=1.6, color="white", size=3.5) +
  labs(title = "Gender distribution of all Coronavirus cases in Maryland, USA (2020)",
       color = "Legend", 
       x = "Gender",
       y = "All cases")
#p

# Deaths now ...

datGD3 <- tail(datGD, n = 1) # assumes datAD ordered by Date
cols2pivot <- colnames(datGD3)
cols2pivot <- cols2pivot[2:length(cols2pivot)]

dtGD3 <- pivot_longer(data = datGD3, cols = cols2pivot, names_to = "Gender", values_to = "Deaths", values_drop_na = TRUE)
dtGD3$Gender <- factor(dtGD3$Gender , levels = c("Female", "Male"))

q <- ggplot(dtGD3, aes(x = Gender, y = Deaths)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_linedraw() +
  geom_text(aes(label=format(Deaths, scientific = FALSE, big.mark = ",")), vjust=1.6, color="white", size=3.5) +
  labs(title = "Gender distribution of all confirmed Coronavirus deaths in Maryland, USA (2020)",
       color = "Legend", 
       x = "Gender",
       y = "All deaths",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
#q

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
#r
ggsave("../figures/gender-section.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")



# Graph F1 - deaths evolution of each gender relative to its own population

# Get the total population and get the relative cases
# Look at deaths per 100,000 for each age category

# Get gender counts from the MD State (arrange from the ini file -> dataframe)
# Already done above: use variable genderTotal
# Already know dateGenderDataUpdate <- pointData$MDPopulation$DateAccessed

# Date is in Date, datTmp will get it in number
datTmp <- datGD
datTmp$Date <- as.integer(datTmp$Date)

matR <- as.matrix(datTmp) # transform to matrix for processing
matT <- as.matrix(genderTotal)

matX <- sweep(matR, 2, matT, '/') # simple processing
matX <- matX * 100000
matX[,1] <- matX[,1] / 100000 # well, shouldn't have multiplied for Date ...

datX <- as.data.frame(matX) # get back to dataframe
datX$Date <- as.Date(datX$Date, origin = "1970-01-01")

cols2pivot <- colnames(datX)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtX <- pivot_longer(data = datX, cols = cols2pivot, names_to = "Gender", values_to = "Deaths", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtX$Ages <- factor(dtX$Gender , levels = c("Female", "Male"))

p <- ggplot(dtX, aes(x = Date, y = Deaths, group = Gender)) +
  geom_line(aes(color = Gender), lwd = 1) +
  geom_point(aes(color = Gender, shape = Gender)) +
  theme_linedraw() +
  labs(title = "Evolution of confirmed COVID-19 deaths relative by gender in Maryland, USA (2020)",
       x = "Date",
       y = "Cumulative deaths / 100,000 pop")
#p


# Graph F2 - deaths evolution of each age category (contribution to 100% - not relative ot pop)

datTT <- datGD
datTT$Total <- datTT$Male + datTT$Female
datTT$`PcFemale` <- datTT$`Female` / datTT$Total * 100
datTT$`PcMale` <- datTT$`Male` / datTT$Total * 100

cols2pivot <- colnames(datTT)
# removing raw numbers
datTT[,(2:4)] <- NULL
# renaming headers
colnames(datTT) <- c("Date", "Female", "Male")
# small digression: keeping the last row for the annotations
datTTlast <- tail(datTT, n = 1)
# pivoting (back to the flow)
cols2pivot <- colnames(datTT)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dtTT <- pivot_longer(data = datTT, cols = cols2pivot, names_to = "Gender", values_to = "Percentage", values_drop_na = TRUE)
# Specify the order I want (younger people at bottom, increase with age)
dtTT$Gender <- factor(dtTT$Gender , levels = c("Female", "Male"))

q <- ggplot(dtTT, aes(x = Date, y = Percentage, fill = Gender)) + 
  geom_area()  +
  theme_linedraw() +
  annotate("text", label = paste("Male:", format(datTTlast$Male, digits = 0), "%"),
           x = datTTlast$Date - 5,
           y = 5,
           size = 3, fontface = "italic") +
  annotate("text", label = paste("Female:", format(datTTlast$Female, digits = 0), "%"),
           x = datTTlast$Date - 5,
           y = datTTlast$Male + 5,
           size = 3, fontface = "italic") +
  labs(title = "Evolution of confirmed COVID-19 deaths by gender, relative to total in Maryland, USA (2020)",
       x = "Date",
       y = "Percentage of deaths by gender",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last data update:", format(max(datGC$Date), "%b %d, %Y")))
#q

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
#r
ggsave("../figures/gender-deaths-relative.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")
