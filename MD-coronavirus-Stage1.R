# Produce graphs of Coronavirus cases in Maryland Counties (USA) - Stage 1 analysis
# Source of data: https://coronavirus.maryland.gov/ (updated daily at ~ 10am)
# Rationale for doing this: have some fun
# Explanations: https://jepoirrier.org/mdcovid19/ 
# Data repository: https://github.com/jepoirrier/MD-coronavirus
# Maryland Recovery website: https://governor.maryland.gov/recovery/

library(dplyr)
library(gghighlight)
library(ggplot2)
library(ggpubr)
library(ini)
library(tidyr)
library(TTR) # for SMA (moving average)
#library(viridis)

plotWidth <- 12
plotHeight <- 7 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong <- 10 # for multiple graphs: 9 (= 3 times 3) + 1

# Read an .ini file with point data in it
iniFile <- "data-other-sources/pointData.ini"
pointData <- read.ini(iniFile)

# The idea is to plot the evolution of variation of # cases over time for 3 groups of counties
# The 3 groups are: Stage1, Partial, Closed
# Day 0 = May 14, 2020 - bring all states to 100%
# Plot subsequent days as a relative % of May 14, 2020

# Read the data from the counties
MDCountiesFile <- 'MD-coronavirus-counties.txt'
MDCountiesNCols <- 26
datCounty <- read.csv(MDCountiesFile, sep = " ", colClasses = c(rep("numeric", MDCountiesNCols)))
#lastDateCounty <- as.Date(sprintf("%d",max(datCounty$Date)), "%y%m%d")
#lastMaxCounty = max(datCounty$PrinceGeorges) # TODO manually find the county with max cases - TODO change this behavior

# MANUALLY CHANGE TO DEATHS - TODO: improve!
# Read the data from the deaths in counties
#MDCountiesDeathsFile <- 'MD-coronavirus-counties-deaths.txt'
#MDCountiesDeathsNCols <- 25
#datCountyDeaths <- read.csv(MDCountiesDeathsFile, sep = " ", colClasses = c(rep("numeric", MDCountiesDeathsNCols)))
#datCounty <- datCountyDeaths

# Read the data from the Counties status for Stage 1
MDCountiesStage1File <- 'data-other-sources/CountiesStage1Status.dat'
datStage1 <- read.csv(MDCountiesStage1File, sep = " ", colClasses = c("character", "character"))

# Let's massage this data!

countiesList <- colnames(datCounty)
counties2remove <- countiesList[-c(1)] # we'll keep date and remove all counties (incl. DnA) - to keep the delta ("D") cols

# Find daily variation (I'm sure there is a faster way)
datCounty <- datCounty %>%
  mutate(DAllegany = Allegany - lag(Allegany, default = 0)) %>%
  mutate(DAnneArundel = AnneArundel - lag(AnneArundel, default = 0)) %>%
  mutate(DBaltimoreCity = BaltimoreCity - lag(BaltimoreCity, default = 0)) %>%
  mutate(DBaltimoreCounty = BaltimoreCounty - lag(BaltimoreCounty, default = 0)) %>%
  mutate(DCalvert = Calvert - lag(Calvert, default = 0)) %>%
  mutate(DCaroline = Caroline - lag(Caroline, default = 0)) %>%
  mutate(DCarroll = Carroll - lag(Carroll, default = 0)) %>%
  mutate(DCecil = Cecil - lag(Cecil, default = 0)) %>%
  mutate(DCharles = Charles - lag(Charles, default = 0)) %>%
  mutate(DDorchester = Dorchester - lag(Dorchester, default = 0)) %>%
  mutate(DFrederick = Frederick - lag(Frederick, default = 0)) %>%
  mutate(DGarrett = Garrett - lag(Garrett, default = 0)) %>%
  mutate(DHarford = Harford - lag(Harford, default = 0)) %>%
  mutate(DHoward = Howard - lag(Howard, default = 0)) %>%
  mutate(DKent = Kent - lag(Kent, default = 0)) %>%
  mutate(DPrinceGeorges = PrinceGeorges - lag(PrinceGeorges, default = 0)) %>%
  mutate(DMontgomery = Montgomery - lag(Montgomery, default = 0)) %>%
  mutate(DQueenAnne = QueenAnne - lag(QueenAnne, default = 0)) %>%
  mutate(DSomerset = Somerset - lag(Somerset, default = 0)) %>%
  mutate(DStMary = StMary - lag(StMary, default = 0)) %>%
  mutate(DTalbot = Talbot - lag(Talbot, default = 0)) %>%
  mutate(DWashington = Washington - lag(Washington, default = 0)) %>%
  mutate(DWicomico = Wicomico - lag(Wicomico, default = 0)) %>%
  mutate(DWorcester = Worcester - lag(Worcester, default = 0))

# dat will have names of counties but with daily delta only
dat <- datCounty[ , -which(names(datCounty) %in% counties2remove)]
colnames(dat) <- countiesList[1:length(dat)]

# datA will have moving average of daily new cases
#datA <- dat
matA <- apply(dat[,2:length(dat)], 2, SMA, n=7)
datA <- cbind(dat$Date, as.data.frame(matA)) # get back to dataframe
colnames(datA) <- countiesList[1:length(datA)]

# create datB = since May 14, 100% = data on that day (Note: use <- dat if not SMA)
datB <- datA
datB$Date <- as.Date(sprintf("%d",datB$Date), "%y%m%d")
datB <- datB[datB$Date >= as.Date("200515", "%y%m%d"), ]

# Find daily variation (I'm sure there is a faster way)
datB <- datB %>%
  mutate(PAllegany = Allegany / Allegany[1] * 100) %>%
  mutate(PAnneArundel = AnneArundel / AnneArundel[1] * 100) %>%
  mutate(PBaltimoreCity = BaltimoreCity / BaltimoreCity[1] * 100) %>%
  mutate(PBaltimoreCounty = BaltimoreCounty / BaltimoreCounty[1] * 100) %>%
  mutate(PCalvert = Calvert / Calvert[1] * 100) %>%
  mutate(PCaroline = Caroline / Caroline[1] * 100) %>%
  mutate(PCarroll = Carroll / Carroll[1] * 100) %>%
  mutate(PCecil = Cecil / Cecil[1] * 100) %>%
  mutate(PCharles = Charles / Charles[1] * 100) %>%
  mutate(PDorchester = Dorchester / Dorchester[1] * 100) %>%
  mutate(PFrederick = Frederick / Frederick[1] * 100) %>%
  mutate(PGarrett = Garrett / Garrett[1] * 100) %>%
  mutate(PHarford = Harford / Harford[1] * 100) %>%
  mutate(PHoward = Howard / Howard[1] * 100) %>%
  mutate(PKent = Kent / Kent[1] * 100) %>%
  mutate(PPrinceGeorges = PrinceGeorges / PrinceGeorges[1] * 100) %>%
  mutate(PMontgomery = Montgomery / Montgomery[1] * 100) %>%
  mutate(PQueenAnne = QueenAnne / QueenAnne[1] * 100) %>%
  mutate(PSomerset = Somerset / Somerset[1] * 100) %>%
  mutate(PStMary = StMary / StMary[1] * 100) %>%
  mutate(PTalbot = Talbot / Talbot[1] * 100) %>%
  mutate(PWashington = Washington / Washington[1] * 100) %>%
  mutate(PWicomico = Wicomico / Wicomico[1] * 100) %>%
  mutate(PWorcester = Worcester / Worcester[1] * 100)

# datB will have names of counties but with RELATIVE daily delta only
datB <- datB[ , -which(names(datB) %in% counties2remove)]
colnames(datB) <- countiesList[1:length(datB)]


# Now let's plot this! 

# Plot 1 = crude daily variation by county
# Note: change datA -> dat (next line and to create dt) if not using SMA
cols2pivot <- colnames(datA)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dt <- pivot_longer(data = datA, cols = cols2pivot, names_to = "County", values_to = "Cases", values_drop_na = TRUE)
dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")

p <- ggplot(dt, aes(x = Date, y = Cases, group = County)) +
  geom_line(aes(color = County), lwd = 1) +
  theme_linedraw() +
  labs(title = "7-Day average daily new COVID-19-confirmed cases in Maryland counties, USA (2020)",
       x = "Date",
       y = "7-Day average daily number of cases")

# Plot 2 = daily variabtion brought back to 100% = value on May 14, 2020
#          and in 3 groups according to Phase 1 stage
#          and labels only at the end
cols2pivot <- colnames(datB)
cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"

dt <- pivot_longer(data = datB, cols = cols2pivot, names_to = "County", values_to = "Cases", values_drop_na = TRUE)
dt <- merge(dt,datStage1, by = "County")

q <- ggplot(dt, aes(x = Date, y = Cases, group = County)) +
  geom_line(aes(color = Status), lwd = 1) +
  theme_linedraw() +
  labs(title = "Cases relative to 1st day of Stage 1 (May 15, 2020)",
       x = "Date",
       y = "% variation (100% = daily # cases on May 14, 2020)",
       caption = paste("Explanations at https://jepoirrier.org/mdcovid19/; COVID-19 data from https://coronavirus.maryland.gov/; last update:", format(Sys.Date(), "%b %d, %Y"))) +
  annotate("segment", x = as.Date("200519", "%y%m%d"), y = 10,
           xend = as.Date("200519", "%y%m%d"), yend = 0,
           size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
  annotate("text", label = "Testing broadening\nMay 19, 2020",
           x = as.Date("200519", "%y%m%d"), y = 22,
           size = 4, fontface = "italic")

r <- ggarrange(p, q, heights = c(1, 1), 
               ncol = 1, nrow = 2)
r

ggsave("figures/MD-COVID19-counties-Stage1cases.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")
