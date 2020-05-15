# Produce graphs of Coronavirus cases in Maryland State (USA) - reported by other metrics
# Source of data: https://coronavirus.maryland.gov/ (updated daily at ~ 10am)
# Rationale for doing this: have some fun
# Explanations: https://jepoirrier.org/mdcovid19/ 
# Data repository: https://github.com/jepoirrier/MD-coronavirus

library(dplyr)
library(gghighlight)
library(ggplot2)
library(ggpubr)
library(tidyr)
#library(viridis)

plotWidth <- 12
plotHeight <- 7 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong <- 10 # for multiple graphs: 9 (= 3 times 3) + 1

# Read the data from the counties
MDCountiesFile <- 'MD-coronavirus-counties.txt'
MDCountiesNCols <- 26
# See MD-coronavirus-cases.R for details
datCounty <- read.csv(MDCountiesFile, sep = " ", colClasses = c(rep("numeric", MDCountiesNCols)))

# Look at cases (positive tests) per 100,000 in each county

# Get county projections from the MD State
MDCountiesPopFile <- 'data-other-sources/TotalPopProj.dat'
MDCountiesPopNCols <- 26
datCountyPop <- read.csv(MDCountiesPopFile, sep = " ", colClasses = c(rep("numeric", MDCountiesPopNCols)))

# Get also the # cases per 100,00 for the whole US
# Source: https://ourworldindata.org/grapher/total-confirmed-cases-of-covid-19-per-million-people?tab=chart&country=USA (couldn't find on CDC website?)
cphtUS <- 4283.62 / 10 # it's per million --> / 10 to per 100,000
# Get also the # cases per 100,00 for the whole US
# Source: https://www.cdc.gov/covid-data-tracker/index.html
cphtMD <- 594.2
dateOutsideDataUpdate <- "May 15, 2020" # BOTH should be updated

matC <- as.matrix(datCounty) # transform to matrix for processing
matP <- as.matrix(datCountyPop)

matX <- sweep(matC, 2, matP, '/') # simple processing
matX <- matX * 100000
matX[,1] <- matX[,1] / 100000 # well, shouldn't have multiplied for Date ...

datX <- as.data.frame(matX) # get back to dataframe

# First build the graph for raw cases per county

cols2pivot <- colnames(datCounty)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dt <- pivot_longer(data = datCounty, cols = cols2pivot, names_to = "County", values_to = "Tests", values_drop_na = TRUE)
dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")

p <- ggplot(dt, aes(x = Date, y = Tests, group = County)) +
  geom_line(aes(color = County), lwd = 1) +
  geom_point(aes(color = County, shape = County)) +
  theme_linedraw() +
  labs(title = "Evolution of COVID-19 confirmed cases in Maryland counties, USA (2020)",
       y = "Cumulative cases")

# Then build the graph for the cases / 100,000 per county

cols2pivot <- colnames(datX)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dt <- pivot_longer(data = datX, cols = cols2pivot, names_to = "County", values_to = "Tests", values_drop_na = TRUE)
dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")

q <- ggplot(dt, aes(x = Date, y = Tests, group = County)) +
  geom_line(aes(color = County), lwd = 1) +
  geom_point(aes(color = County, shape = County)) +
  #geom_hline(yintercept = cphtUS, linetype = "dashed", color = "black") + # complete horiz line is wrong because induce constant level since time on x-axis
  annotate("segment", x = as.Date(as.Date(sprintf("%d", max(datX$Date)), "%y%m%d")) - 5, y = cphtUS,
           xend = as.Date(as.Date(sprintf("%d", max(datX$Date)), "%y%m%d")), yend = cphtUS,
           size = 0.5) +
  annotate("text", label = paste("USA:", format(cphtUS, scientific = FALSE, big.mark = ",")),
           x = as.Date(as.Date(sprintf("%d", max(datX$Date)), "%y%m%d")) - 5, y = cphtUS + 20,
           size = 3, fontface = "italic") +
  #geom_hline(yintercept = cphtMD, color = "black") + # complete horiz line is wrong because induce constant level since time on x-axis
  annotate("segment", x = as.Date(as.Date(sprintf("%d", max(datX$Date)), "%y%m%d")) - 5, y = cphtMD,
           xend = as.Date(as.Date(sprintf("%d", max(datX$Date)), "%y%m%d")), yend = cphtMD,
           size = 0.5) +
  annotate("text", label = paste("MD:", format(cphtMD, scientific = FALSE, big.mark = ",")),
           x = as.Date(as.Date(sprintf("%d", max(datX$Date)), "%y%m%d")) - 5, y = cphtMD + 20,
           size = 3, fontface = "italic") +
  theme_linedraw() +
  labs(x = "Date",
       y = "Cumulative cases / 100,000 pop",
       caption = paste("DnA = Data not Available ; US data: OurWorldInData.org ; MD average: CDC (both", dateOutsideDataUpdate, ")\nCOVID-19 data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y")))

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2, align = "v")

ggsave("figures/MD-COVID19-counties-casesPHT.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")

