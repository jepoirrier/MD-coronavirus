# Zip code analysis of data from Maryland
# Using data from the MD Data Respository: https://data.imap.maryland.gov/

# WORK IN PROGRESS!

# MDCOVID19 MASTER ZIP CODE CASES
# https://data.imap.maryland.gov/datasets/mdcovid19-master-zip-code-cases
# Download URL to check if not moving every day

library(dplyr)
library(gghighlight)
library(ggplot2)
library(tidyr)

print("Entering zip-analysis.R")

plotWidth <- 12
plotHeight <- 7 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong <- 10 # for multiple graphs: 9 (= 3 times 3) + 1
preventMultipleDownload <- FALSE

ZipURL <- "https://opendata.arcgis.com/datasets/5f459467ee7a4ffda968139011f06c46_0.csv"
ZipFile <- "../data/zip.csv"

# Download the data
if((as.Date(file.info(ZipFile)$ctime) < as.Date(Sys.Date())) | !isTRUE(preventMultipleDownload)) {
  download.file(ZipURL, ZipFile, "auto") # might switch to curl to support Windows
} else {
  print("Download skipped as ZipFile already downloaded today")
}
if(file.exists(ZipFile)) {
  print(paste("ZipFile found, created on:", file.info(ZipFile)$ctime))
} else {
  stop(ZipFile, " does not exist") # not the best way to stop (if it even stops!)
}

# Import the data
datZip <- read.csv(ZipFile, sep = ",", colClasses = c("integer"))

# Clean the data
# we don't need column OBJECTID
datZip$OBJECTID <- NULL
Zipcols <- colnames(datZip)
# remove 'F' if string starts with 'F'
Zipcols <- gsub('F', '', Zipcols)
# remove "total" if string starts with "total"
Zipcols <- gsub('total', '', Zipcols)
# cosmetic: change "ZIP_CODE" to "zipcode" (do not just remove '_')
Zipcols <- gsub('ZIP_CODE', 'ZIPCODE', Zipcols)

colnames(datZip) <- Zipcols

datZip$ZIPCODE <- as.factor(as.character(datZip$ZIPCODE))

cols2pivot <- Zipcols[2:length(Zipcols)] # we don't need ZIPCODE
dt <- pivot_longer(data = datZip, cols = cols2pivot, names_to = "Date", values_to = "Cases", values_drop_na = TRUE)
dt$Date <- as.character(dt$Date)
dt$Date <- as.Date(sprintf("%s",dt$Date), "%m_%d_%Y")
print(paste("Latest data point in ZipFile:", max(dt$Date)))

# CHART ALL ZIPs over time

p <- ggplot(dt, aes(x = Date, y = Cases, group = ZIPCODE)) +
  geom_line(aes(color = ZIPCODE)) +
  #gghighlight(ZipCode == zip2highlight) +
  gghighlight(Cases > 3000, label_key = ZIPCODE) +
  theme_linedraw() +
  theme(legend.position = "none") +
  labs(title = "Evolution of Coronavirus positive cases by zip codes in Maryland, USA (2020)",
       y = "Cumulative positive tests counts")
p

dt2 <- pivot_wider(data = dt, names_from = ZIPCODE, values_from = Cases)

deltaF <- function(x, na.rm = FALSE) (x - lag(x, default = 0))
dt3 <- dt2
dt3 <- mutate_all(dt3[2:length(dt3)], ~ deltaF(.))
dt3 <- cbind(dt3, Date = dt2$Date) # this is risky (desync?) but adds Date as the last column

cols2pivot <- colnames(dt3)
cols2pivot <- cols2pivot[1:length(cols2pivot)-1]
dt4 <- pivot_longer(data = dt3, cols = cols2pivot, names_to = "ZIPCODE", values_to = "Cases", values_drop_na = TRUE)

ZipInMoCo <- c("20812","20814","20815","20816","20817","20818","20832","20833","20837","20838","20839","20841","20842","20850","20851","20852","20853","20854","20855","20860","20861","20862","20866","20868","20871","20872","20874","20876","20877","20878","20879","20880","20882","20886","20895","20896","20899","20901","20902","20903","20904","20905","20906","20910","20912")

q <- ggplot(dt4, aes(x = Date, y = Cases, group = ZIPCODE)) +
  geom_line(aes(color = ZIPCODE)) +
  ylim(0, 150) +
  #gghighlight((Cases > 50 & Date > "2020-05-20"), label_key = ZIPCODE) +
  gghighlight(ZIPCODE %in% c("21224", "21223", "20906", "20783", "20906", "20706"), label_key = ZIPCODE, label_params = list(size = 3)) +
  theme_linedraw() +
  theme(legend.position = "none") +
  labs(title = "Evolution of daily positive COVID-19 cases in Montgomery County zip codes (2020)",
       x = "Date",
       y = "Daily positive tests counts",
       caption = paste("Note: data for ZIP codes with 7 or fewer cases are not present on the MDH dashboard (and hence nor here)\nExplanations at https://jepoirrier.org/mdcovid19/ - data from https://coronavirus.maryland.gov/ - last update:", format(Sys.Date(), "%b %d, %Y")))
q
r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2,  align = "v")
r
ggsave("../figures/zip-cases.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")

