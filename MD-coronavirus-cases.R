# Produce a simple graph of Coronavirus cases in Maryland State (USA) since the 1st cases detected
# Source of data: https://coronavirus.maryland.gov/ (updated daily at ~ 10am - 12pm)
# Rationale for doing this: no history of past cases and removal of some data (see below)

library(ggplot2)
library(tidyr)

MDCasesFile = 'MD-coronavirus-cases.txt'
MDCasesNCols <- 28
# Space-delimited file with NCols fields:
# Date: date in YearMonthDay format
# TotalNegative: number of total negative cases (was not shown after March 11, 2020)
# TotalPositive: number of total positive cases
# TotalDeath: number of total deaths linked with positive cases
# AnneArundel: number of positive cases in Anne Arundel County (started on March 15, 2020)
# then all the other counties

dat <- read.csv(MDCasesFile, sep = " ", colClasses = c(rep("numeric", MDCasesNCols)))
#dat$DateTime <- as.POSIXct(dat$Date, format = "%m/%d/%y")

# ***** Trends in number of cases by totals over time *****

cols2pivot <- colnames(dat) # headers of columns to pivot
cols2pivot[2:4] <- c("Negative", "Positive", "Deaths")
colnames(dat) <- cols2pivot
cols2pivot <- cols2pivot[2:4] # we don't need "Date" nor states

dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "Total", values_to = "Tests", values_drop_na = TRUE)
dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")

p <- ggplot(dt, aes(x = Date, y = Tests, group = Total)) +
  geom_line(aes(color = Total)) +
  geom_point(aes(color = Total)) +
  labs(title = "Evolution of Coronavirus testing in Maryland, USA (2020)",
       x = "Date",
       y = "Tests counts",
       caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))
p # optional
ggsave("MD-coronavirus-cases.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")



# ***** Trends in number of cases by COUNTY over time *****

cols2pivot <- colnames(dat) # headers of columns to pivot
cols2pivot <- cols2pivot[5:length(cols2pivot)] # we don't need "Date" nor totals

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

# ***** Trends in number of cases by age group over time *****

MDAgeFile = 'MD-coronavirus-byage.txt'
MDAgeNCols <- 4
# Space-delimited file with NCols fields:
# Date: date in YearMonthDate format
# 0-18: number of positive cases aged 0 to 18 years old
# 19-64: number of positive cases aged 19 to 64 years old
# 65plus: number of positive cases aged 65 years old and more

dat2 <- read.csv(MDAgeFile, sep = " ", colClasses = c(rep("numeric", MDAgeNCols)))

cols2pivot <- colnames(dat2) # headers of columns to pivot
cols2pivot[2:4] <- c("0-18", "19-64", "65+")
colnames(dat2) <- cols2pivot
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date" nor totals

dt2 <- pivot_longer(data = dat2, cols = cols2pivot, names_to = "AgeGroups", values_to = "Tests", values_drop_na = TRUE)
dt2$Date <- as.Date(sprintf("%d",dt2$Date), "%y%m%d")

p <- ggplot(dt2, aes(x = Date, y = Tests, group = AgeGroups)) +
  geom_line(aes(color = AgeGroups)) +
  geom_point(aes(color = AgeGroups, shape = AgeGroups)) +
  labs(title = "Evolution of Coronavirus testing in Maryland by age group, USA (2020)",
       x = "Date",
       y = "Tests counts",
       caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))
p # optional
ggsave("MD-coronavirus-byage.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")

