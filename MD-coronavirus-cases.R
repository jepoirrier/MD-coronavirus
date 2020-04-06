# Produce a simple graph of Coronavirus cases in Maryland State (USA) since the 1st cases detected
# Source of data: https://coronavirus.maryland.gov/ (updated daily at ~ 10am - 12pm)
# Rationale for doing this: no history of past cases and removal of some data (see below)
# Explanations: https://jepoirrier.org
# Data repository: https://github.com/jepoirrier/MD-coronavirus

library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)

#source("fnMultiplot.R")

# ********** Reading the number of cases file **********

# ***** Trends in number of cases by totals over time *****

MDCasesFile = 'MD-coronavirus-cases2.txt' # change in file (only for totals)
MDCasesNCols <- 6
# Space-delimited file with NCols fields:
# Date: date in YearMonthDay format
# Negative: number of total negative cases (was not shown after March 11, 2020)
# Positive: number of total positive cases
# Death: number of total deaths linked with positive cases
# Hospitalizations: number of people ever hospitalized
# Released: number of people released from hospital

dat <- read.csv(MDCasesFile, sep = " ", colClasses = c(rep("numeric", MDCasesNCols)))

lastDateCases <- as.Date(sprintf("%d", max(dat$Date, na.rm = TRUE)), "%y%m%d")
lastMaxPositiveCases = max(dat$Positive, na.rm = TRUE)

cols2pivot <- colnames(dat) # headers of columns to pivot
cols2pivot[2:6] <- c("Negative", "Positive", "Deaths", "Hospitalizations", "Released")
colnames(dat) <- cols2pivot
cols2pivot <- cols2pivot[2:6] # we don't need "Date"

dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "Total", values_to = "Tests") # don't drop NAs because Negative tests came back on 3/28
dt$Date <- as.Date(sprintf("%d", dt$Date), "%y%m%d")

p <- ggplot(dt, aes(x = Date, y = Tests, group = Total)) +
  # log scale and 33% daily as in FT graph
  scale_y_log10() + annotation_logticks() +
  geom_abline(intercept = 1, slope = 33) +
  geom_line(aes(color = Total)) +
  geom_point(aes(color = Total, shape = Total)) +
  labs(title = "Evolution of Coronavirus testing in Maryland, USA (2020)",
       x = "Date",
       y = "Tests counts (log scale!)",
       caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y"))) +
  # manually place the arrow and label highlighting the last data
  annotate("segment", x = as.Date(lastDateCases) - 3.5, y = lastMaxPositiveCases,
           xend = as.Date(lastDateCases) - 2, yend = lastMaxPositiveCases,
           size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
  annotate("text", label = paste("Last number of\ncases:", format(lastMaxPositiveCases, scientific = FALSE, big.mark = ",")),
           x = as.Date(lastDateCases) - 6, y = lastMaxPositiveCases + 1000,
           size = 3, fontface = "italic")

p # optional
ggsave("MD-coronavirus-cases.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")

# ***** Trends in number of DAILY cases over time *****

# compute delta per day
dat <- dat %>%
  mutate(deltaNegative = Negative - lag(Negative, default = 0))
dat <- dat %>%
  mutate(deltaPositive = Positive - lag(Positive, default = 0))
dat <- dat %>%
  mutate(deltaDeaths = Deaths - lag(Deaths, default = 0))
dat <- dat %>%
  mutate(deltaHospitalizations = Hospitalizations - lag(Hospitalizations, default = 0))
dat <- dat %>%
  mutate(deltaReleased = Released - lag(Released, default = 0))

cols2pivot <- colnames(dat) # headers of columns to pivot
cols2pivot[2:6] <- c("TotalNegative", "TotalPositive", "TotalDeaths", "TotalHospitalizations", "TotalReleased")
cols2pivot[7:11] <- c("Negative", "Positive", "Deaths", "Hospitalizations", "Released")
colnames(dat) <- cols2pivot
cols2pivot <- cols2pivot[7:11] # we don't need "Date"

dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "Delta", values_to = "DailyVariation") # don't drop NAs because Negative tests came back on 3/28
dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")

p <- ggplot(dt, aes(x = Date, y = DailyVariation, group = Delta)) +
  # log scale and 33% daily as in FT graph
  #scale_y_log10() + annotation_logticks() +
  #geom_abline(intercept = 1, slope = 33) +
  geom_line(aes(color = Delta)) +
  geom_point(aes(color = Delta, shape = Delta)) +
  labs(title = "Daily variation of Coronavirus cases in Maryland, USA (2020)",
       x = "Date",
       y = "Daily variation (# of cases)",
       caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))

p # optional
ggsave("MD-coronavirus-cases-daily.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")

# ***** Displaying total number of tests made & the positive ratio *****

datTT <- dat[1:3]
datTT$TotalTests <- datTT$TotalNegative + datTT$TotalPositive
datTT$PositivePC <- datTT$TotalPositive / datTT$TotalTests * 100

lastMaxTotalCases = max(datTT$TotalTests, na.rm = TRUE)
lastPositivePC = if (is.na(datTT$PositivePC[nrow(datTT)])) (datTT$PositivePC[nrow(datTT) - 1]) else (datTT$PositivePC[nrow(datTT)]) # needed because sometimes last line is NA

datTT$Date <- as.Date(sprintf("%d", datTT$Date), "%y%m%d")

p <- ggplot(datTT, aes(x = Date, y = TotalTests)) +
  geom_line() +
  geom_point() +
  labs(title = "Cumulative number of Coronavirus tests received in Maryland, USA (2020)",
       x = " ",
       y = "Cumulative number of tests") +
  # manually place the arrow and label highlighting the last data
  annotate("segment", x = as.Date(lastDateCases) - 3.5, y = lastMaxTotalCases,
           xend = as.Date(lastDateCases) - 2, yend = lastMaxTotalCases,
           size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
  annotate("text", label = paste("Last total number\nof tests:", format(lastMaxTotalCases, scientific = FALSE, big.mark = ",")),
           x = as.Date(lastDateCases) - 6, y = lastMaxTotalCases + 1000,
           size = 3, fontface = "italic")

q <- ggplot(datTT, aes(x = Date, y = PositivePC)) +
  geom_line() +
  geom_point() +
  labs(title = "Proportion of positive Coronavirus tests in Maryland, USA (2020)",
       x = "Date",
       y = "Percentage of positive tests (%)",
       caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y"))) +
  # manually place the arrow and label highlighting the last data
  annotate("segment", x = as.Date(lastDateCases) - 3.5, y = lastPositivePC,
           xend = as.Date(lastDateCases) - 2, yend = lastPositivePC,
           size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
  annotate("text", label = paste("Last percentage of\npositive tests:", sprintf("%.2f%%", lastPositivePC)),
           x = as.Date(lastDateCases) - 6.5, y = lastPositivePC,
           size = 3, fontface = "italic")

#multiplot(p, q, cols = 1)
r <- ggarrange(p, q, heights = c(1, 1), 
          ncol = 1, nrow = 2, align = "v")

ggsave("MD-coronavirus-total-tests.png", plot = r, device = "png", width = 3840/300, height = 2160/300, units = "in")

# ********** Reading the number of cases in COUNTIES file **********

# ***** Trends in number of cases by COUNTY over time *****

MDCountiesFile <- 'MD-coronavirus-counties.txt'
MDCountiesNCols <- 25
# Space-delimited file with NCols fields:
# Date: date in YearMonthDay format
# AnneArundel: number of positive cases in Anne Arundel County (started on March 15, 2020)
# then all the other counties

dat <- read.csv(MDCountiesFile, sep = " ", colClasses = c(rep("numeric", MDCasesNCols)))

lastDateCases <- as.Date(sprintf("%d",max(dat$Date)), "%y%m%d")
lastMaxCountyCases = max(dat$Montgomery) # manually find the county with max cases

cols2pivot <- colnames(dat) # headers of columns to pivot
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date" nor totals

dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "County", values_to = "Tests", values_drop_na = TRUE)
dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")

p <- ggplot(dt, aes(x = Date, y = Tests, group = County)) +
  geom_line(aes(color = County)) +
  geom_point(aes(color = County, shape = County)) +
  labs(title = "Evolution of Coronavirus testing in Maryland counties, USA (2020)",
       x = "Date",
       y = "Tests counts",
       caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))
p # optional
ggsave("MD-coronavirus-counties.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")

# ********** Reading the number of cases BY AGE file **********

# ***** Trends in number of cases by age group over time *****

MDAgeFile = 'MD-coronavirus-byage.txt'
MDAgeNCols <- 15
# Space-delimited file with NCols fields:
# Date: date in YearMonthDate format
# 0-9: number of positive cases aged 0 to 9 years old
# 10-19: number of positive cases aged 10 to 19years old
# 20-29: number of positive cases aged 20 to 29 years old
# 30-39: number of positive cases aged 30 to 39 years old
# 40-49: number of positive cases aged 40 to 49 years old
# 50-59: number of positive cases aged 50 to 59 years old
# 60-69: number of positive cases aged 60 to 69 years old
# 70-79: number of positive cases aged 70 to 79 years old
# 80+: number of positive cases aged 80 years old and over
# 0-18: number of positive cases aged 0 to 18 years old
# 19-64: number of positive cases aged 19 to 64 years old
# 65plus: number of positive cases aged 65 years old and more

dat <- read.csv(MDAgeFile, sep = " ", colClasses = c(rep("numeric", MDAgeNCols)))

cols2pivot <- colnames(dat) # headers of columns to pivot
cols2pivot <- c("Date", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+", "0-18", "19-64", "65+", "Female", "Male")
colnames(dat) <- cols2pivot
cols2pivot <- cols2pivot[2:(MDAgeNCols-2)] # we don't need "Date" nor sex

dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "AgeGroups", values_to = "Tests", values_drop_na = TRUE)
dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")

p <- ggplot(dt, aes(x = Date, y = Tests, group = AgeGroups)) +
  geom_line(aes(color = AgeGroups)) +
  geom_point(aes(color = AgeGroups, shape = AgeGroups)) +
  labs(title = "Evolution of Coronavirus testing in Maryland by age group, USA (2020)",
       x = "Date",
       y = "Tests counts",
       caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))
p # optional
ggsave("MD-coronavirus-byage.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")

# ***** Slight variation, just for the latest data *****

dat <- dat[,1:10]

cols2pivot <- colnames(dat) # headers of columns to pivot
cols2pivot <- c("Date", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
colnames(dat) <- cols2pivot
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "AgeGroups", values_to = "Tests", values_drop_na = TRUE)
dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")
dt <- dt[(nrow(dt)-8):(nrow(dt)),]

p <- ggplot(dt, aes(x = AgeGroups, y = Tests)) +
  geom_bar(stat="identity", fill="steelblue") +
  #geom_line(aes(color = AgeGroups)) +
  #geom_point(aes(color = AgeGroups, shape = AgeGroups)) +
  theme_minimal() +
  geom_text(aes(label=Tests), vjust=1.6, color="white", size=3.5)+
  labs(title = "Age distribution of all Coronavirus positive tests in Maryland, USA (2020)",
       x = "Age groups (years)",
       y = "Tests counts",
       caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))
p # optional
ggsave("MD-coronavirus-byage-grp.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")
