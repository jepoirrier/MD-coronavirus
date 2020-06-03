# Deaths statewide analysis of data from Maryland
# Using data from the MD Data Respository: https://data.imap.maryland.gov/

# WORK IN PROGRESS!

# MDCOVID19 TotalConfirmedDeathsStatewide
# https://data.imap.maryland.gov/datasets/mdcovid19-totalconfirmeddeathsstatewide
# Download URL to check if not moving every day
#
# MDCOVID19 TotalConfirmedDeathsByDateOfDeath
# https://data.imap.maryland.gov/datasets/mdcovid19-totalconfirmeddeathsbydateofdeath
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

# Read an .ini file with point data in it
iniFile <- "../data-other-sources/pointData.ini"
pointData <- read.ini(iniFile)

# CUMULATIVE death confirmed

DCURL <- "https://opendata.arcgis.com/datasets/096cca5f77404a06babb9367530136b9_0.csv"
DCFile <- "../data/deaths-confirmed.csv"
# DC = deaths confirmed

# Download the data
if(as.Date(file.info(DCFile)$ctime) < as.Date(Sys.Date())) {
  download.file(DCURL, DCFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as DCFile already downloaded today")
}
if(file.exists(DCFile)) {
  print(paste("DCFile found, created on:", file.info(DCFile)$ctime))
} else {
  stop(DCFile, " does not exist") # not the best way to stop (if it even stops!)
}

# Import the data
datDC <- read.csv(DCFile, sep = ",", colClasses = c("integer", "Date", "integer"))

# Clean the data
# we don't need column OBJECTID
datDC$OBJECTID <- NULL
# rename headers
colnames(datDC) <- c("Date", "Confirmed")

# calculate the COVID-19 mortality rate
print(paste("COVID-19 mortality rate:", max(datDC$Confirmed) / as.double(pointData$MDPopulation$Total) * 100000, " / 100,000"))

# Calculate daily delta from the cumulative count
deltaF <- function(x, na.rm = FALSE) (x - lag(x, default = 0))
dtDC <- datDC
dtDC <- mutate_all(dtDC[2:length(dtDC)], ~ deltaF(.))
dtDC <- cbind(dtDC, Date = datDC$Date) # this is risky (desync?) but adds Date as the last column

# Get start / end dates to define Nbreaks
Nbreaks <- floor((as.integer(max(dtDC$Date)) - as.integer(min(dtDC$Date))) / 5)

# DAILY CONFIRMED DEATHS

DCDURL <- "https://opendata.arcgis.com/datasets/ecce72a93ca24096a4463aac1e1bf771_0.csv"
DCDFile <- "../data/deaths-confirmedDate.csv"
# DC = deaths confirmed

# Download the data
if(as.Date(file.info(DCDFile)$ctime) < as.Date(Sys.Date())) {
  download.file(DCDURL, DCDFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as DCDFile already downloaded today")
}
if(file.exists(DCDFile)) {
  print(paste("DCDFile found, created on:", file.info(DCFile)$ctime))
} else {
  stop(DCDFile, " does not exist") # not the best way to stop (if it even stops!)
}

# Import the data
datDCD <- read.csv(DCDFile, sep = ",", colClasses = c("integer", "Date", "integer"))

# Clean the data
# we don't need column OBJECTID
datDCD$OBJECTID <- NULL
# rename headers
colnames(datDCD) <- c("Date", "ByDate")

# Merge daily delta and datDCD and make a data frame ready for plotting
dtDCD <- merge(dtDC, datDCD, by = "Date", all = TRUE)
cols2pivot <- colnames(dtDCD)
cols2pivot <- cols2pivot[2:length(cols2pivot)]
dtDCDlong <- pivot_longer(data = dtDCD, cols = cols2pivot, names_to = "Deaths", values_to = "Count", values_drop_na = FALSE)



# CHART ALL DEATHs over time

p <- ggplot(datDC, aes(x = Date, y = Confirmed)) +
  geom_line(lwd = 1) +
  theme_linedraw() +
  #theme(legend.position = "none") +
  labs(title = "Evolution of Coronavirus confirmed deaths in Maryland, USA (2020)",
       y = "Cumulative confirmed deaths") +
  annotate("segment", x = max(datDC$Date), y = datDC$Confirmed[datDC$Date == max(datDC$Date)] - 200,
           xend = max(datDC$Date), yend = datDC$Confirmed[datDC$Date == max(datDC$Date)] - 50,
           size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
  annotate("text", label = paste("Latest:", datDC$Confirmed[datDC$Date == max(datDC$Date)], "\nconfirmed"),
           x = max(datDC$Date) - 2, y = datDC$Confirmed[datDC$Date == max(datDC$Date)] - 400,
           size = 4, fontface = "italic")
#p


# CHART ALL DAILY DEATHs over time

q <- ggplot(dtDCDlong, aes(x = Date, y = Count, group = Deaths)) +
  geom_line(aes(color = Deaths), lwd = 1) +
  scale_color_grey(start = 0.7, end = 0) +
  theme_linedraw() +
  theme(legend.position = "none") +
  labs(title = "Evolution of daily Coronavirus confirmed deaths in Maryland, USA (2020)",
       y = "Daily confirmed deaths",
       caption = paste("Black lines: number of deaths at date of reporting; grey line: number of deaths by date of death\nExplanations at https://jepoirrier.org/mdcovid19/ - data from https://coronavirus.maryland.gov/ - last update:", format(Sys.Date(), "%b %d, %Y"))) +
  # annotation for maximum # of deaths reported daily
  annotate("segment", x = dtDC$Date[dtDC$Confirmed == max(dtDC$Confirmed)], y = max(dtDC$Confirmed) + 5,
           xend = dtDC$Date[dtDC$Confirmed == max(dtDC$Confirmed)], yend = max(dtDC$Confirmed) +2,
           size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
  annotate("text", label = paste("Maximum daily confirmed death:", max(dtDC$Confirmed)),
           x = min(dtDC$Date[dtDC$Confirmed == max(dtDC$Confirmed)]) + 5, y = max(dtDC$Confirmed) + 10,
           size = 4, fontface = "italic")
#q

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")
#r
ggsave("../figures/deaths-confirmed.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")



# Let's look for a TREND

# non-ggplot way
#plot(x = dtDC$Date, y = dtDC$Confirmed, type='l')
#units <- ts(dtDC$Confirmed, frequency = 7)
#decomp <- stl(units, s.window = 'periodic')
#plot(decomp)

s <- ggsdc(dtDC, aes(x = Date, y = Confirmed), frequency = 7, method = 'decompose') +
  geom_line() +
  theme_linedraw() +
  scale_x_date(breaks = pretty_breaks(Nbreaks)) +
  #theme(legend.position = "none") +
  labs(title = "Evolution of daily Coronavirus confirmed deaths in Maryland, USA (2020)",
       x = "Date of reporting",
       y = "Daily confirmed deaths",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ - data from https://coronavirus.maryland.gov/ - last update:", format(Sys.Date(), "%b %d, %Y")))
#s
ggsave("../figures/deaths-confirmedTrend.png", plot = s, device = "png", width = plotWidth, height = plotHeight, units = "in")


t <- ggsdc(datDCD, aes(x = Date, y = ByDate), frequency = 7, method = 'decompose') +
  geom_line() +
  theme_linedraw() +
  scale_x_date(breaks = pretty_breaks(Nbreaks)) +
  #theme(legend.position = "none") +
  labs(title = "Evolution of Coronavirus deaths (by date of death) in Maryland, USA (2020)",
       x = "Date of death",
       y = "Daily confirmed deaths",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/ - data from https://coronavirus.maryland.gov/ - last update:", format(Sys.Date(), "%b %d, %Y")))
#t
ggsave("../figures/deaths-confirmedTrendD.png", plot = t, device = "png", width = plotWidth, height = plotHeight, units = "in")


